library(shiny)
library(gtsummary)
library(ggplot2)
library(htmltools)
library(DBI)
library(bigrquery)
library(gt)
library(tidyverse)
library(dbplyr)
library(shinycssloaders)

# Load the data
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds")

# path to the service account token 
satoken <- "../biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

patients <- tbl(con_bq, "patients")
admission <- tbl(con_bq, "admissions")
transfers <- tbl(con_bq, "transfers")
procedures_icd <- tbl(con_bq, "procedures_icd")
diagnoses_icd <- tbl(con_bq, "diagnoses_icd")
d_icd_procedures <- tbl(con_bq, "d_icd_procedures")
d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses")
labevents <- tbl(con_bq, "labevents")
d_items <- tbl(con_bq, "d_items")
chartevents <- tbl(con_bq, "chartevents")
icustays <- tbl(con_bq, "icustays")

ui <- fluidPage(
  titlePanel("ICU Cohort Data Exploration"),
  tabsetPanel(
    tabPanel("Summary",
             selectInput("category", "Select Category:", 
                         choices = c("None", "Demographic", 
                                     "Lab Measurements", "Vitals")),
             uiOutput("subCategory"),
             checkboxInput("showNumeric", "Numeric", value = FALSE),
             checkboxInput("showGraph", "Graphic", value = FALSE),
             conditionalPanel(
               condition = "input.showNumeric == true",
               tableOutput("numericSummary")
             ),
             conditionalPanel(
               condition = "input.showGraph == true",
               plotOutput("graphicalSummary")
             )
    ),
    tabPanel("Patient Details",
             selectizeInput(
               inputId = "PatientID",
               label = "Select Patient ID:",
               choices = NULL, # initially no choices
               options = list(placeholder = 'Type or select a patient ID')
             ),
             uiOutput("ADTorICU"),
             withSpinner(plotOutput("ggplotOutput"))
             # dataTableOutput("patientDetails")
    )
  ),
  tableOutput("summaryOutput")
)



server <- function(input, output, session) {
  output$subCategory <- renderUI({
    if (input$category == "Demographic") {
      selectInput("subCategoryDemo", "Select Demographic Option:", 
                  choices = c("None", "Gender", "Age", "Insurance Type",
                              "Marital Status", "Race", "Language")) 
    } else if (input$category == "Lab Measurements") {
      selectInput("subCategoryLab", "Select Lab Measurement:", 
                  choices = c("None", "Sodium", "Glucose",
                              "Chloride", "Potassium", "Creatinine", 
                              "Hematocrit", "Bicarbonate", 
                              "White Blood Cells")) 
    } else if (input$category == "Vitals") {
      selectInput("subCategoryVital", "Select Vital:", 
                  choices = c("None", "Heart Rate", "Respiratory Rate",
                              "Temperature Fahrenheit",
                              "Non Invasive Blood Pressure systolic",
                              "Non Invasive Blood Pressure diastolic")) 
    }
  })
  # Observe changes in the category and subcategory selections
  observeEvent(input$category, {
    # Reset the subcategory value when the main category changes
    updateSelectInput(session, "subCategoryDemo", selected = "None")
    updateSelectInput(session, "subCategoryLab", selected = "None")
    updateSelectInput(session, "subCategoryVital", selected = "None")
  })
  
  
  observe({
    # Variable to store the selected subcategory based on the main category
    selectedSubCategory <- NULL
    
    if (input$category == "Demographic") {
      selectedSubCategory <- input$subCategoryDemo
    } else if (input$category == "Lab Measurements") {
      selectedSubCategory <- input$subCategoryLab
    } else if (input$category == "Vitals") {
      selectedSubCategory <- input$subCategoryVital
    }
    
    output$numericSummary <- renderTable({
      if (!is.null(selectedSubCategory) && input$showNumeric) {
        if (selectedSubCategory == "Gender") {
          return(mimic_icu_cohort |>
                   select(gender) |>
                   tbl_summary(
                     missing_text = "(Missing)"
                   ))
          # summary_table <- mimic_icu_cohort |>
          #   select(gender) |>
          #   tbl_summary()
          # gt_output <- as_gt(summary_table)
          # htmltools::tagList(gt_output)
        } else if (selectedSubCategory == "Age") {
          mimic_icu_cohort |> 
                   select(age_intime) |>
                   tbl_summary(
                     type = all_continuous() ~ "continuous2",
                     statistic = all_continuous() ~ c(
                       "{N_nonmiss}",
                       "{median} ({p25}, {p75})",
                       "{min}, {max}"
                     ),
                     missing = "no"
                   )
        }
      }
    }, show = input$showNumeric)  
    
    # Graphical Summary
    output$graphicalSummary <- renderPlot({
      if (!is.null(selectedSubCategory) && input$showGraph) {
        if (input$category == "Demographic"){
          plot <- switch(selectedSubCategory,
                         "Gender" = ggplot(data = mimic_icu_cohort) +
                           geom_bar(aes(x = gender, fill = gender)) +
                           ggtitle("Gender count among patients"),
                         "Age" = ggplot(data = mimic_icu_cohort) +
                           geom_histogram(aes(x = age_intime), bins = 30) +
                           ggtitle("Age distribution among patients"),
                         "Insurance Type" = ggplot(data = mimic_icu_cohort) +
                           geom_bar(aes(x = insurance, fill = insurance)) +
                           ggtitle(paste("Insurance Type distribution",
                           "among patients")),
                         "Marital Status" = ggplot(data = mimic_icu_cohort) +
                           geom_bar(
                             aes(x = marital_status, 
                                 fill = marital_status)
                             ) +
                           ggtitle(paste("Marital Status distribution",
                           "among patients")),
                         "Race" = ggplot(data = mimic_icu_cohort) +
                           geom_bar(aes(x = race, fill = race)) +
                           ggtitle("Race distribution among patients"),
                         "Language" = ggplot(data = mimic_icu_cohort) +
                           geom_bar(aes(x = language, fill = language)) +
                           ggtitle(paste("Language Spoken distribution",
                           "among patients"))
          )
        } 
        else if (input$category == "Vitals"){
          plot <- switch(selectedSubCategory,
                         "Heart Rate" = ggplot(data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 2, 
                             aes(x = Heart_Rate), 
                             ) +
                           scale_x_continuous(
                             breaks = seq(0, 200, 10), 
                             lim = c(0, 200)
                             ) +
                           ggtitle("Heart Rate distribution among patients"),
                         
                         "Respiratory Rate" = ggplot(data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 1, 
                             aes(x = Respiratory_Rate), 
                             ) +
                           scale_x_continuous(
                             breaks = seq(0, 80, 5), 
                             lim = c(0, 80)
                             ) +
                           ggtitle(paste("Respiratory Rate distribution",
                           "among patients")),
                         
                         "Temperature Fahrenheit" = ggplot(
                           data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 0.1, 
                             aes(x = Temperature_Fahrenheit), 
                             ) +
                           scale_x_continuous(
                             breaks = seq(80, 110, 5), 
                             lim = c(80, 110)
                             ) +
                           ggtitle(paste("Temperature Fahrenheit",
                           "distribution among patients")
                           ),
                         
                         "Non Invasive Blood Pressure systolic" = 
                           ggplot(data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 1,
                             aes(x = Non_Invasive_Blood_Pressure_systolic), 
                             ) +
                           scale_x_continuous(
                             breaks = seq(0, 250, 5), 
                             lim = c(0, 250)
                             ) +
                           ggtitle(
                             paste("Non Invasive Blood Pressure",
                             "systolic distribution among patients")),
                         
                         "Non Invasive Blood Pressure diastolic" = 
                           ggplot(data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 1,
                             aes(x = Non_Invasive_Blood_Pressure_diastolic), 
                             ) + 
                           scale_x_continuous(
                             breaks = seq(0, 250, 5), 
                             lim = c(0, 250)) +
                           ggtitle(paste("Non Invasive Blood Pressure",
                           "diastolic distribution among patients"))
          )
        } 
        else if (input$category == "Lab Measurements"){
          plot <- switch(selectedSubCategory,
                         "Sodium" = ggplot(data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 2, 
                             aes(x = Sodium), 
                             ) +
                           scale_x_continuous(
                             breaks = seq(90, 180, 5),
                             lim = c(90, 180)
                             ) +
                           ggtitle("Sodium distribution among patients"),
                         
                         "Glucose" = ggplot(data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 5,
                             aes(x = Glucose), 
                             ) +
                           scale_x_continuous(
                             breaks = seq(0, 600, 20),
                             lim = c(0, 600)
                             ) +
                           ggtitle("Glucose distribution among patients"),
                         
                         "Potassium" = ggplot(data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 0.1,
                             aes(x = Potassium), 
                             ) +
                           scale_x_continuous(
                             breaks = seq(0, 10, 2),
                             lim = c(0, 10)
                             ) +
                           ggtitle("Potassium distribution among patients"),
                         
                         "Chloride" = ggplot(data = mimic_icu_cohort) +
                           geom_histogram(
                             binwidth = 2,
                             aes(x = Chloride), 
                             ) +
                           scale_x_continuous(
                             breaks = seq(60, 150, 5),
                             lim = c(60, 150)
                             ) +
                           ggtitle("Chloride distribution among patients"),
                         
                         "Bicarbonate" = ggplot(data = mimic_icu_cohort) +
                             geom_histogram(
                               binwidth = 0.5,
                               aes(x = Bicarbonate), 
                               ) +
                             scale_x_continuous(
                               breaks = seq(0, 50, 2),
                               lim = c(0, 50)
                               ) +
                             ggtitle("Bicarbonate distribution among patients"),
                         
                         "Creatinine" = ggplot(data = mimic_icu_cohort) +
                               geom_histogram(
                                 binwidth = 0.1,
                                 aes(x = Creatinine), 
                                 ) +
                               scale_x_continuous(
                                 breaks = seq(0, 10, 2),
                                 lim = c(0, 10)
                                 ) +
                               ggtitle(paste("Creatinine distribution",
                               "among patients")),
                         
                         "Hematocrit" = ggplot(data = mimic_icu_cohort) +
                                   geom_histogram(
                                     binwidth = 0.5,
                                     aes(x = Hematocrit), 
                                     ) +
                                   scale_x_continuous(
                                     breaks = seq(0, 70, 5),
                                     lim = c(0, 70)
                                     ) +
                                   ggtitle(paste("Hematocrit distribution",
                                   "among patients")),
                         
                         "White Blood Cells" = ggplot(data = mimic_icu_cohort) +
                                       geom_histogram(
                                         binwidth = 1,
                                         aes(x = White_Blood_Cells), 
                                         ) +
                                       scale_x_continuous(
                                         breaks = seq(0, 80, 2),
                                         lim = c(0, 80)
                                         ) +
                                       ggtitle(paste("White Blood Cell Count",
                                       "distribution among patients"))
                         
          )
        }
        if (!is.null(plot)) {
          return(plot)
        }
      }
    })
  })
  output$ADTorICU <- renderUI({
    selectInput("ADTorICUdemo", "Select Information Type:", 
                choices = c("None", "Admission", "ICU stay")) 
  })
  
  observe({
    updateSelectizeInput(session, "PatientID", 
                         choices = mimic_icu_cohort$subject_id, 
                         server = TRUE) # Enable server-side selectize
  })
  observe({
    selectedADTorICU <- input$ADTorICUdemo
    
    output$ggplotOutput <- renderPlot({
      id = as.integer(input$PatientID)
      
      if(selectedADTorICU == "Admission"){
        procedure_info = procedures_icd |>
          # exclude rows with other subject id
          filter(subject_id == id) |>
          # join table to get the type procedures
          left_join(d_icd_procedures, by = "icd_code") |>
          collect() |>
          # convert date variable to POSIXct format
          mutate(chartdate = as.POSIXct(chartdate, format = "%Y-%m-%d")) |>
          # represent the type of procedures with first 30 characters
          mutate(long_title = str_sub(long_title, 1, 50))
        
        
        diagnoses_info = diagnoses_icd |>
          # exclude rows with other subject id
          filter(subject_id == id) |>
          # join table to get the type diagnoses
          left_join(d_icd_diagnoses, by = c("icd_code", "icd_version")) |>
          # sort in ascending order by hadm id and sequence number
          arrange(hadm_id, seq_num) |>
          collect()
        # exclude rows with other subject id
        patients_filtered <- patients |> filter(subject_id == id) |> collect()
        
        admission_filtered <- admission |>
          # exclude rows with other subject id
          filter(subject_id == id) |>
          # keep one unique row and all variables
          distinct(subject_id, .keep_all = TRUE) |>
          collect()
        
        # create a new variable to indicate whether the care unit is an ICU/CCU
        transfer_icu <- transfers  |>
          # exclude rows with other subject id
          filter(subject_id == id) |>
          # create a new variable to indicate whether the care 
          # unit is in ICU/CCU
          mutate(icu_status = ifelse(str_detect(careunit, "ICU|CCU"),
                                     "ICU/CCU", "non-ICU/CCU")) |>
          # exclude rows with missing value in careunit
          filter(!is.na(careunit)) |>
          collect()
        # store subject_id of the patient
        id_char = toString(patients_filtered['subject_id'])
        # store gender of the patient
        gender_char = toString(patients_filtered['gender'])
        # store recorded age of the patient
        age_char = toString(patients_filtered['anchor_age'])
        # store race of the patient
        race_char = toString(admission_filtered['race'])
        
        # store top three most occurrence diagonsis of the patient
        diagnoses_info_1 = diagnoses_info['long_title'] |> slice(1)
        diagnoses_info_2 = diagnoses_info['long_title'] |> slice(2)
        diagnoses_info_3 = diagnoses_info['long_title'] |> slice(3)
        
        procedure_info_count <- procedure_info |>
          count(long_title) |> 
          nrow()
        if (procedure_info_count %% 2 != 0) {
          procedure_row_count <- procedure_info_count %/% 2 + 1
        } else {
          procedure_row_count <- procedure_info_count %/% 2 
        }
        
        careunit_count <- transfer_icu |> 
          count(careunit) |> 
          nrow()
        
        if (procedure_info_count %% 3 != 0) {
          careunit_row_count <- careunit_count %/% 3 + 1
        } else {
          careunit_row_count <- careunit_count %/% 3 
        }
        
        # create title and subtitle
        title = paste0("Patient ", id_char, ", ", gender_char, ", ", age_char,
                       " years old, ", race_char)
        subtitle = paste0(diagnoses_info_1, "\n", diagnoses_info_2, "\n",
                          diagnoses_info_3)
        # data visualization
        ggplot() +
          # create a line segment to represent the hospital stay and use 
          # different color of segment to represent stays in different 
          # care units
          geom_segment(data = transfer_icu, 
                       aes(x = intime, xend = outtime, y = 3, yend = 3,  
                           color = careunit),
                       # adjust the width of the line segment of indicate 
                       # icu stay
                       linewidth = ifelse(transfer_icu$icu_status == 'ICU/CCU', 
                                          4, 1)) +
          # create points to represent the charted lab events with the specific 
          # subject id
          geom_point(data = labevents |> filter(subject_id == id), 
                     aes(x = charttime, y = 2), shape = 3, size = 1) +
          # create points to represent the charted procedures and use different
          # shape of points to represent different procedures
          geom_point(data = procedure_info, 
                     aes(x = chartdate, y = 1, shape = long_title), size = 3) + 
          # rename the legend for procedures and manually assign shapes
          scale_shape_manual(values = c(1:procedure_info_count), 
                             name = "Procedure") +
          # rename the legend for care units
          scale_color_discrete(name = "Care Unit") +
          # adjust legend orders and the number of rows and columns
          guides(color = guide_legend(order = 2, 
                                      nrow = careunit_row_count, 
                                      byrow = TRUE),
                 shape = guide_legend(order = 1, 
                                      nrow = procedure_row_count, 
                                      byrow = TRUE)) +
          # rename the x and y axis
          xlab('Calender Time') +
          ylab('') +
          # adjust the theme of the legend
          theme(
            legend.text = element_text(size = 6),
            legend.position = "bottom",
            legend.box = "vertical",
            legend.title.align = 0,
            legend.direction = 'horizontal') +
          # assign name to each plot on y axis
          scale_y_continuous(breaks = 1:3, labels = c("Procedure", 
                                                      "Lab", "ADT")) + 
          # assign title and subtitle
          ggtitle(label = title, subtitle = subtitle)
      } else if (selectedADTorICU == "ICU stay"){
        d_items <- d_items |> 
          collect() |>
          filter(itemid %in% c(220045, 220179, 220180, 223761, 220210)) |>
          select(itemid, abbreviation)
        
        chartevents <- chartevents |>
          # exclude rows with other subject id
          filter(subject_id == id) |>
          collect() |>
          # convert date variable to POSIXct format
          mutate(charttime = as.POSIXct(charttime, 
                                        format = "%Y-%m-%d %H:%M:%S")) |>
          filter(itemid %in% d_items$itemid) |>
          left_join(d_items, by = "itemid")

        
        icustays <- icustays |>
          filter(subject_id == id) 
        
        # plot vitals during each ICU stays
        ggplot(chartevents, aes(x = charttime, y = valuenum, 
                                color = abbreviation)) +
          geom_point() +
          geom_line() +
          # set x and y scales of each facet to be free
          facet_grid(abbreviation ~stay_id, scales = "free") +
          # turn off legend
          theme(legend.position = "none") +
          # add title
          ggtitle(label = paste0("Patient ", id, " ICU stays - Vitals")) +
          # adjust x axis to display date and time in a more readable format
          scale_x_datetime(guide = guide_axis(n.dodge = 2))
      }
    })
  })
}
shinyApp(ui, server)

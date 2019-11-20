
library(shiny)
library(tidyverse)
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(openxlsx)
library(zip)

load('data/IR_data.Rdata')
load("data/assessed_AUs.Rdata")


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("yeti"),

    # Application title
    titlePanel(
        fluidRow(
            column(6, img(src = "logo.png")), 
            column(6,  "2018/2020 Integrated Report Data Download",style = "font-family: 'Arial'; font-si16pt; vertical-align: 'bottom'")),
        windowTitle = "2018/2020 Integrated Report Data"
    ),
    sidebarLayout(
        sidebarPanel(

        # Show a plot of the generated distribution
                   downloadButton('downloadallData', label = "Download All Assessment Data"),
            downloadButton('downloadData', label = "Download Assessment Data by Unit"),
            selectizeInput("AUs",
                           "Select one or more Assessment Units",
                           choices = assessed_AUs,
                           multiple = TRUE,
                           options = list(maxOptions = 7000))
        
    ),
    
    
    
    mainPanel(
        tabsetPanel(type = "tabs",
                    id = "Tabset",
                    tabPanel("Instructions",
                             value = "InstructionTab",
                             h2(strong("Download numeric data used in the 2018/2020 Integrated Report"), style = "font-family: 'Arial'"),
                             p("DEQ recommends using the current version of Google Chrome or Mozilla Firefox for this application.", style = "font-family: 'times'"),
                             p("This application provides the numeric data used in new assessments for the 2018/2020 Integrated Report. Clicking on the", strong('Download All Assessment Data'), "will
                               download all numeric data used in new 2018/2020 assessments. Entering one or more Assessment Units in the search box and pressing",strong('Download All Assessment Data'), 
                               "will download select data. Data will be downloaded bundled into a zip file" , 
                               style = "font-family: 'times'"),
                             p(strong("Due to the size of the file, downloading All Assessment Data may take a few minutes"), style = "font-family: 'times'"),
                             p("A dictionary describing column headers is included in the zip file", style = "font-family: 'times'"),
                             p("A  complete mapping and dataset, including water quality standards information can be found on the ", 
                               a("Interactive web map.", href="https://hdcgcx2.deq.state.or.us/HVR291/?viewer=wqsa#", target="_blank"), 
                               "Assessment conculsions can be found on the ", a("online assessment database.", href="https://travispritchard.shinyapps.io/2018-2020_IR_Database/", target="_blank"), style = "font-family: 'times'"),
                             p( 
                                 a("The 2018/2020 Assessment Methodology can be found here.", href="https://www.oregon.gov/deq/FilterDocs/ir2018assessMethod.pdf", target="_blank"), style = "font-family: 'times'"),
                             p(
                                 a("The DEQ 2018/2020 IR webpage page can be found here.", href="https://www.oregon.gov/deq/wq/Pages/2018-Integrated-Report.aspx", target="_blank"), style = "font-family: 'times'")
                             ))
                             
                             
                    
                    )),
    add_busy_spinner(spin = "fading-circle")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    filtered_data <- reactive({
        
        filtered_bacteria_coast_contact <- bacteria_coast_contact %>%
            filter(AU_ID %in% input$AUs)
        
        
        filtered_bacteria_fresh_contact <- bacteria_fresh_contact  %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_bacteria_Shell_harvest <- bacteria_Shell_harvest  %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_chl <- chl  %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_DO_cont_spawn <- DO_cont_spawn %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_DO_cont_yearround <- DO_cont_yearround  %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_DO_instant_spawn <- DO_instant_spawn %>%
            filter(AU_ID %in% input$AUs) 
        
        filtered_DO_inst_yearround <- DO_inst_yearround %>%
            filter(AU_ID %in% input$AUs) 
        
        filtered_DO_estuary_spawn <- DO_estuary_spawn %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_DO_estuary_yearround <- DO_estuary_yearround %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_pH <- pH %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_temp <- temp   %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_Tox_AL_Ammonia <-Tox_AL_Ammonia %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_Tox_AL_CU <-Tox_AL_CU %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_Tox_AL_Hardness_Metals <-Tox_AL_Hardness_Metals %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_Tox_AL_Others <- Tox_AL_Others %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_Tox_AL_Penta <- Tox_AL_Penta %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_Tox_HH <- Tox_HH %>%
            filter(AU_ID %in% input$AUs)
        
        filtered_Tox_HH_Hg_tissue <- Tox_HH_Hg_tissue %>%
            filter(AU_ID %in% input$AUs)
        
        return(list(filtered_bacteria_coast_contact = filtered_bacteria_coast_contact,
                    filtered_bacteria_fresh_contact = filtered_bacteria_fresh_contact,
                    filtered_bacteria_Shell_harvest = filtered_bacteria_Shell_harvest, 
                    filtered_chl = filtered_chl, 
                    filtered_DO_cont_spawn = filtered_DO_cont_spawn,
                    filtered_DO_cont_yearround = filtered_DO_cont_yearround,
                    filtered_DO_instant_spawn = filtered_DO_instant_spawn,
                    filtered_DO_inst_yearround = filtered_DO_inst_yearround,
                    filtered_DO_estuary_spawn = filtered_DO_estuary_spawn,
                    filtered_DO_estuary_yearround = filtered_DO_estuary_yearround,
                    filtered_pH = filtered_pH,
                    filtered_temp = filtered_temp,
                    filtered_Tox_AL_Ammonia = filtered_Tox_AL_Ammonia,
                    filtered_Tox_AL_CU = filtered_Tox_AL_CU,
                    filtered_Tox_AL_Hardness_Metals = filtered_Tox_AL_Hardness_Metals,
                    filtered_Tox_AL_Others = filtered_Tox_AL_Others,
                    filtered_Tox_AL_Penta =filtered_Tox_AL_Penta,
                    filtered_Tox_HH = filtered_Tox_HH,
                    filtered_Tox_HH_Hg_tissue =filtered_Tox_HH_Hg_tissue))
        
    })
    
    
    
   
    
    
    
    # datasetInput <- reactive({
    #     return(list(rock=filtered_temp, pressure=pressure, cars=cars))
    # })
    # 

    output$downloadData <- downloadHandler(
        filename = '2018_2020_IR_select_data_download.zip',
        content = function(fname) {
            original_wd <- getwd()
            tmpdir <- tempdir()
            setwd(tempdir())
            print(tempdir())

            fs <- c("temp.xlsx", "Bacteria.xlsx", "Chlorophyll.xlsx",
                    "DO.xlsx", "pH.xlsx",
                    "Aquatic_Life_Toxics.xlsx", "Human_Health_Toxics.xlsx",
                    "IR_Data_Dictionary.xlsx"
                    )
            
            #temperature
            write.xlsx(filtered_data()$filtered_temp, file = "temp.xlsx",
                       overwrite = TRUE)
            
            wb <- createWorkbook()
            
            # bacteria
            addWorksheet(wb, "E coli")
            addWorksheet(wb, "Enterococcus")
            addWorksheet(wb, "Fecal Coliform")
            
            writeData(wb,"E coli",  filtered_data()$filtered_bacteria_fresh_contact, rowNames = FALSE)
            writeData(wb,"Enterococcus", filtered_data()$filtered_bacteria_coast_contact, rowNames = FALSE)
            writeData(wb,"Fecal Coliform", filtered_data()$filtered_bacteria_Shell_harvest, rowNames = FALSE)
            
            saveWorkbook(wb, file = "Bacteria.xlsx", 
                         overwrite = TRUE)
            
            #chl
            write.xlsx(filtered_data()$filtered_chl, 
                        file = "Chlorophyll.xlsx", 
                        overwrite = TRUE)
            
            # pH
            write.xlsx( filtered_data()$filtered_pH, 
                        file = "pH.xlsx", 
                        overwrite = TRUE)
            
            #DO
            
            # wb <- createWorkbook()
            # addWorksheet(wb, "DO_spawn_continuous")
            # addWorksheet(wb, "DO_spawn_instantaneous")
            # 
            # writeData(wb,"DO_spawn_continuous",  filtered_data()$filtered_DO_cont_spawn, rowNames = FALSE)
            # writeData(wb,"DO_spawn_instantaneous", filtered_data()$filtered_DO_instant_spawn, rowNames = FALSE)
            # 
            # saveWorkbook(wb, "DO_Spawning.xlsx", 
            #              overwrite = TRUE)
            
            
            
            
            wb <- createWorkbook()
            addWorksheet(wb, "DO_yearround_continuous")
            addWorksheet(wb, "DO_yearround_instantaneous")
            addWorksheet(wb, "DO_spawn_continuous")
            addWorksheet(wb, "DO_spawn_instantaneous")
            
            writeData(wb,"DO_yearround_continuous",  filtered_data()$filtered_DO_cont_yearround, rowNames = FALSE)
            writeData(wb,"DO_yearround_instantaneous",filtered_data()$filtered_DO_inst_yearround, rowNames = FALSE)
            writeData(wb,"DO_spawn_continuous",  filtered_data()$filtered_DO_cont_spawn, rowNames = FALSE)
            writeData(wb,"DO_spawn_instantaneous", filtered_data()$filtered_DO_instant_spawn, rowNames = FALSE)
            
            saveWorkbook(wb, file = "DO.xlsx", 
                         overwrite = TRUE)
            
        
            
            wb <- createWorkbook()
            addWorksheet(wb, 'Tox_AL_Others')
            addWorksheet(wb,'Tox_AL_Ammonia')
            addWorksheet(wb,'Tox_AL_CU')
            addWorksheet(wb, 'Tox_AL_Hardness_Metals')
            addWorksheet(wb, 'Tox_AL_Pentachlorophenol')
            
            writeData(wb,  'Tox_AL_Others', filtered_data()$filtered_Tox_AL_Others, rowNames = FALSE)
            writeData(wb, 'Tox_AL_Ammonia',  filtered_data()$filtered_Tox_AL_Ammonia, rowNames = FALSE)
            writeData(wb, 'Tox_AL_CU',  filtered_data()$filtered_Tox_AL_CU, rowNames = FALSE)
            writeData(wb, 'Tox_AL_Hardness_Metals',  filtered_data()$filtered_Tox_AL_Hardness_Metals, rowNames = FALSE)
            writeData(wb, 'Tox_AL_Pentachlorophenol',  filtered_data()$filtered_Tox_AL_Penta, rowNames = FALSE)
            
            saveWorkbook(wb, file = "Aquatic_Life_Toxics.xlsx", 
                         overwrite = TRUE)
            
            
            
            
            wb <- createWorkbook()
            addWorksheet(wb, 'Tox_HH')
            addWorksheet(wb,'Tox_HH_Hg_Tissue')
            writeData(wb, 'Tox_HH',  filtered_data()$filtered_Tox_HH, rowNames = FALSE)
            writeData(wb, 'Tox_HH_Hg_Tissue',  filtered_data()$filtered_Tox_HH_Hg_tissue, rowNames = FALSE)
            
            
            saveWorkbook(wb, file = "Human_Health_Toxics.xlsx", 
                         overwrite = TRUE)
            
            file.copy(paste0(original_wd, "/data/IR_Data_Dictionary.xlsx"), "IR_Data_Dictionary.xlsx")
            
            print (fs)
            
            zip(zipfile=fname, files=fs)
           
            setwd(original_wd)
        },
        contentType = "application/zip"
    )
    
    
    # output$downloadallData <- downloadHandler(
    #     filename = 'data_download.zip',
    #     content = function(fname) {
    #         tmpdir <- tempdir()
    #         setwd(tempdir())
    #         print(tempdir())
    # 
    #         fs <- c("temp.xlsx", "Bacteria.xlsx", "Chlorophyll.xlsx",
    #                 "DO_Spawning.xlsx", "DO_Yearround.xlsx", "pH.xlsx",
    #                 "Aquatic_Life_Toxics.xlsx", "Human_Health_Toxics.xlsx"
    #         )
    # 
    #         #temperature
    #         write.xlsx(filtered_data()$filtered_temp, file = "temp.xlsx",
    #                    overwrite = TRUE)
    # 
    #         wb <- createWorkbook()
    # 
    #         # bacteria
    #         addWorksheet(wb, "E coli")
    #         addWorksheet(wb, "Enterococcus")
    #         addWorksheet(wb, "Fecal Coliform")
    # 
    #         writeData(wb,"E coli",  bacteria_fresh_contact, rowNames = FALSE)
    #         writeData(wb,"Enterococcus", bacteria_coast_contact, rowNames = FALSE)
    #         writeData(wb,"Fecal Coliform", bacteria_Shell_harvest, rowNames = FALSE)
    # 
    #         saveWorkbook(wb, file = "Bacteria.xlsx",
    #                      overwrite = TRUE)
    # 
    #         #chl
    #         write.xlsx(chl,
    #                    file = "Chlorophyll.xlsx",
    #                    overwrite = TRUE)
    # 
    #         # pH
    #         write.xlsx( pH,
    #                     file = "pH.xlsx",
    #                     overwrite = TRUE)
    # 
    #         #DO
    # 
    #         wb <- createWorkbook()
    #         addWorksheet(wb, "DO_spawn_continuous")
    #         addWorksheet(wb, "DO_spawn_instantaneous")
    # 
    #         writeData(wb,"DO_spawn_continuous",  DO_cont_spawn, rowNames = FALSE)
    #         writeData(wb,"DO_spawn_instantaneous", DO_instant_spawn, rowNames = FALSE)
    # 
    #         saveWorkbook(wb, "DO_Spawning.xlsx",
    #                      overwrite = TRUE)
    # 
    # 
    # 
    # 
    #         wb <- createWorkbook()
    #         addWorksheet(wb, "DO_yearround_continuous")
    #         addWorksheet(wb, "DO_yearround_instantaneous")
    # 
    #         writeData(wb,"DO_yearround_continuous",  DO_cont_yearround, rowNames = FALSE)
    #         writeData(wb,"DO_yearround_instantaneous",DO_inst_yearround, rowNames = FALSE)
    # 
    #         saveWorkbook(wb, file = "DO_Yearround.xlsx",
    #                      overwrite = TRUE)
    # 
    # 
    # 
    #         wb <- createWorkbook()
    #         addWorksheet(wb, 'Tox_AL_Others')
    #         addWorksheet(wb,'Tox_AL_Ammonia')
    #         addWorksheet(wb,'Tox_AL_CU')
    #         addWorksheet(wb, 'Tox_AL_Hardness_Metals')
    #         addWorksheet(wb, 'Tox_AL_Pentachlorophenol')
    # 
    #         writeData(wb,  'Tox_AL_Others', Tox_AL_Others, rowNames = FALSE)
    #         writeData(wb, 'Tox_AL_Ammonia',  Tox_AL_Ammonia, rowNames = FALSE)
    #         writeData(wb, 'Tox_AL_CU',  Tox_AL_CU, rowNames = FALSE)
    #         writeData(wb, 'Tox_AL_Hardness_Metals',  Tox_AL_Hardness_Metals, rowNames = FALSE)
    #         writeData(wb, 'Tox_AL_Pentachlorophenol',  Tox_AL_Penta, rowNames = FALSE)
    # 
    #         saveWorkbook(wb, file = "Aquatic_Life_Toxics.xlsx",
    #                      overwrite = TRUE)
    # 
    # 
    # 
    # 
    #         wb <- createWorkbook()
    #         addWorksheet(wb, 'Tox_HH')
    #         addWorksheet(wb,'Tox_HH_Hg_Tissue')
    #         writeData(wb, 'Tox_HH',  Tox_HH, rowNames = FALSE)
    #         writeData(wb, 'Tox_HH_Hg_Tissue',  Tox_HH_Hg_tissue, rowNames = FALSE)
    # 
    # 
    #         saveWorkbook(wb, file = "Human_Health_Toxics.xlsx",
    #                      overwrite = TRUE)
    # 
    #         print (fs)
    # 
    #         zip(zipfile=fname, files=fs)
    #     },
    #     contentType = "application/zip"
    # )
    
    
    output$downloadallData <-  downloadHandler(
        filename <- function() {
            paste("2018_2020_IR_all_data_download", "zip", sep=".")
        },
        
        content <- function(file) {
            file.copy("data/All_data.zip", file)
        },
        contentType = "application/zip"
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(tidyverse)
library(shinybusy)
library(knitr)
library(kableExtra)
library(shinythemes)
library(shinyWidgets)
library(openxlsx)

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


        # Show a plot of the generated distribution
        mainPanel(
            
            downloadButton('downloadallData', label = "Download All Assessment Data"),
            downloadButton('downloadData', label = "Download Assessment Data by Unit"),
            selectizeInput("AUs",
                           "Select Assessment Unit",
                           choices = assessed_AUs,
                           multiple = TRUE,
                           options = list(maxOptions = 7000))
        
    ),
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
        filename = 'data_download.zip',
        content = function(fname) {
            tmpdir <- tempdir()
            setwd(tempdir())
            print(tempdir())
            
            fs <- c("temp.xlsx", "Bacteria.xlsx", "Chlorophyll.xlsx",
                    "DO_Spawning.xlsx", "DO_Yearround.xlsx", "pH.xlsx",
                    "Aquatic_Life_Toxics.xlsx", "Human_Health_Toxics.xlsx"
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
            
            wb <- createWorkbook()
            addWorksheet(wb, "DO_spawn_continuous")
            addWorksheet(wb, "DO_spawn_instantaneous")
            
            writeData(wb,"DO_spawn_continuous",  filtered_data()$filtered_DO_cont_spawn, rowNames = FALSE)
            writeData(wb,"DO_spawn_instantaneous", filtered_data()$filtered_DO_instant_spawn, rowNames = FALSE)
            
            saveWorkbook(wb, "DO_Spawning.xlsx", 
                         overwrite = TRUE)
            
            
            
            
            wb <- createWorkbook()
            addWorksheet(wb, "DO_yearround_continuous")
            addWorksheet(wb, "DO_yearround_instantaneous")
            
            writeData(wb,"DO_yearround_continuous",  filtered_data()$filtered_DO_cont_yearround, rowNames = FALSE)
            writeData(wb,"DO_yearround_instantaneous",filtered_data()$filtered_DO_inst_yearround, rowNames = FALSE)
            
            saveWorkbook(wb, file = "DO_Yearround.xlsx", 
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
            
            print (fs)
            
            zip(zipfile=fname, files=fs)
        },
        contentType = "application/zip"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

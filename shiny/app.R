library(shiny)
library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
library(shinyFiles)
#library(tidyverse)
#library(readxl)


# UI ----------------------------------------------------------------------



ui <- 
  navbarPage("The IndiMap",
             
      tabPanel("Upload file",
        sidebarLayout(
          sidebarPanel(
            # Input: Select folder ----
            
            shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
            
            
            pickerInput('pub', 'Publication Profile',
                        choices = NA,
                        options = list(
                          `live-search` = TRUE)),
            
            # Here's an option for manually finding the file.
            # But this is difficult if file names are UUID's
            #fileInput("file1", "Choose CSV File",
            #          multiple = FALSE,
            #          accept = c("text/csv",
            #                     "text/comma-separated-values,text/plain",
            #                     ".csv")),
            
            # Horizontal line
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ";"),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
          ),
          
          mainPanel(
            # Output: Data file ----
            tableOutput("uploaded")
          ),
          )
          ),
                    
      tabPanel("Register publication",
        actionButton('populate', 'Populate form with values from the uploaded file'),       
        DTOutput('test'),
        downloadButton("downloadData", "Download")
               ),
        
                     
      tabPanel("Register indicator"),
             
             
             
    navbarMenu("More",
      tabPanel("Instructions"),
      tabPanel("Contact")
             )
  )
    
  


# **SERVER -------------



server <- function(input, output, session) ({
  
  
  # Preview Publication Profile ----
  output$uploaded <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  
  
  
  # Make the uploaded file available through an reactive element 
  previewPub <- reactive({
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
  })

  
  
  # TEST Print data table in the next tab over ----
  output$test <- renderDT({
    previewPub()
    
  })  
    
  
  
  # Downloadable csv of selected data set ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(uuid::UUIDgenerate(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(testReactive(), file, row.names = FALSE)
    }
  )
    
  
  
  # Chose folder ------------
  # Compile data set from existing publication profiles
  # Let the user navigate to the directory for the publication profiles
  # If the GitHub.repo was cloned, it will be under data/publicationProfiles 
  shinyDirChoose(input, 'folder', 
                 roots=c('C' = 'C:/', 'home' = '/home'), # the directory needs to be on C or /home. 
                 allowDirCreate = F) 
  
  
  
  
  
  # Compile publication profiles ----------------
  # Create a reactive data set
  publicationList <- reactive({
    req(input$folder)
    
    # Get file paths
    myfilesUUID = list.files(
      path = "../data/publicationProfiles",
      #path= input$folder, 
      pattern="*.csv", 
      full.names=TRUE
      )
    
    # Read the csv's and rbind them
    combined <- plyr::ldply(myfilesUUID, function(x) {
      temp <<- read.csv(x,
                       sep = ';'
                       #,
                       #header = input$header,
                       #sep = input$sep,
                       #quote = input$quote
                       )
      temp$ID <<- temp$value[temp$parameter=="pID"]
      temp
      })
    
    
    # transpose
    combined2 <- data.table::dcast(
      data.table::setDT(combined),
      formula = ID~parameter)
    
  })
  
  
  
  # reactive list of publications titles ----
  observeEvent(input$folder, {
    req(input$folder)
    updatePickerInput(session = session, inputId = "pub",
                    choices = unique(publicationList()$pTitle))
  })
  
})


shinyApp(ui = ui, server = server)
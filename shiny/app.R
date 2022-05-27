library(shiny)
library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
library(shinyFiles)
library(shinyWidgets)
library(DT)
library(uuid)
#library(tidyverse)
#library(readxl)


# Column1 (parameters) =  list of terms for the publication profiles
# Column2 (values) =  NA
publicationParameters <- 
  read.csv("shinyData/publicationProfileFields.csv", sep=";")

# Duplicate data set for export 
#pExport <- publicationParameters






# UI ----------------------------------------------------------------------



ui <- 
  navbarPage("The IndiMap",
             
             
# **TAB Upload file ---------------------
    tabPanel("Upload file",
      sidebarLayout(
        sidebarPanel(
            
      # Input: 'localPub' ----      
      # Here's an option for manually locating the local folder 
      # containing unpublished (unpushed) publication profiles.
      # The files, or paths, in the folder are listed, read, and compiled.
      
      
      fileInput("localPub", "Choose CSV files",
                multiple = T,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      
      # Input: 'pubDrop'  ----
      # Drop down list of publication profiles
      # Profiles (for indicators and publications alike)
      # are stored using time stamps as file names. 
      # To find and upload the correct file (to modify it)
      # requires that we first must read the indicator names
      # stored inside all the files
          pickerInput('pubDrop', 'Publication Profile',
                  choices = NA,
                  options = list(
                    `live-search` = TRUE)),
      
      
      
      
      
            
        # Horizontal line
          tags$hr(),
            
      # Input: 'header' ----
      #Checkbox if file has header
          checkboxInput("header", "Header", TRUE),
            
      # Input: 'sep' ----
      #Select separator
          radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                             selected = ";"),
            
      # Input: 'quote' ----
      # Select quotes
          radioButtons("quote", "Quote",
                   choices = c(None = "",
                     "Double Quote" = '"',
                     "Single Quote" = "'"),
                           selected = '"'),
            
        # Horizontal line
          tags$hr(),
            
      # Input: 'disp' ----
      # Select number of rows to display ----
          radioButtons("disp", "Display",
            choices = c(Head = "head",
                         All = "all"),
                    selected = "head")
      
      
),  # end sidebar panel

# Main Panel PRINT FILE ---------------------

          mainPanel(
            
            # Info
      h3("INFORMATION"),
      h5("Here you can choose to upload a cvs file so that you can modify them.\n
         In order to find the correct file, first use the 'Choose CVS files' 
         function to select all the crypically names files (typically using Ctrl+A inside
         the main folder containing the publication og indikator profiles).
         Then choose the correct file from the dropdown list. A preview of the import 
         allows you to adjust import settings like headers and seperators."),
      
            # Output: Data file ----
            tableOutput("uploaded")
          ),
          )
          ),
             

# '-------------       
# **TAB Register Publication ----
      tabPanel("Register publication",
        
               
      # INPUT: populate     
        #actionButton('populate', 'Populate form with values from the uploaded file'),       
        materialSwitch(
          inputId = "populate",
          label = "Populate form with values from the uploaded file",
          value = FALSE, 
          status = "info"),
        
      
        
        
        # create some space
        br(), br(),
        
      # INPUT pZoteroID ----
        textInput("pzoteroid", 
                  "Enter the full URL for the Zotero entry", 
                  value = ""),
      
      
      
        
        downloadButton("downloadData", "Download")
               ),



        
# '-------------
# **TAB Register indicator ----
      tabPanel("Register indicator"),
             


# '-------------             
# **TAB More ----
    navbarMenu("More",
      tabPanel("Instructions"),
      tabPanel("Contact")
             )
  )
    
  
# ' ------------
# ' ------------
# ' ------------


# SERVER -------------



server <- function(input, output, session) ({
  
#*********************************************************************************

# ***TAB UPLOAD FILE -------------------------------
  # REACT: publicationList ----------------
  
  # Compile local publication profiles 
  # Create a reactive data set
  # If we had a locally deployd app and used shinyChooseDir()
  # then just use the myLocalFiles part as the first arg in ldply
  
  publicationList <- reactive({
    req(input$localPub)
    # Read the csv's, add path and ID, and finally rbind them
    combined <- plyr::ldply(input$localPub[,"datapath"], function(x) {
      temp <- read.csv(x,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote
      )
      temp[nrow(temp)+1,] <- c("filename", x)
      temp$ID <- temp$value[temp$parameter=="pID"]
      temp
    })
    
    
    # transpose from long to wide format
    data.table::dcast(
      data.table::setDT(combined),
      formula = ID~parameter)
    
  })
  
  #*********************************************************************************  
  
  # REACT: title to path ----
  # Take the chosen publication title from pubDrop
  # and link it to the corresponding file path
  titleToPath <- reactive({
    p <- publicationList()
    p <- p$filename[p$pTitle == input$pubDrop]
    p
  })
  
  
  #*********************************************************************************  
  
  # updatePicker pubDrop ----
  # reactive list of publications titles
  observeEvent(input$localPub, {
    updatePickerInput(session = session, inputId = "pubDrop",
                      choices = unique(publicationList()$pTitle))
  })
  
  
#*********************************************************************************
  
  # REACT: uploadedPub ----
  # Make the uploaded publication profile file available through an reactive element 
  
  uploadedPub <- reactive({
    read.csv(
      titleToPath(),
      header = input$header,
      sep = input$sep,
      quote = input$quote)
  })
  
#*********************************************************************************
  
  # Output: uploaded ----
  # Preview the chosen Publication Profile to make sure it's imported correctly
  output$uploaded <- renderTable({
    
    # input$pubDrop will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$pubDrop)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(
                       titleToPath(),
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
  
  
  
  
  
  
  
  
  
  
  # ' ------------
## ** TAB Register Publication ----
  
  ## Populate form ----
  pform <- reactive({
    ifelse(input$populate == FALSE, 
           return(publicationParameters),
           return(uploadedPub()))
  })
  
  
  ## pZoteroID ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'pzoteroid',
                    value = pform()$value[pform()$parameter == "pZoteroID"])
    
  })
  
  
  
  
#*********************************************************************************
  # Compile CSVs for the publication profile
  
  
  pExport <- reactive({
    
    # shorten name
    dat <- publicationParameters
    
    #update value column based on input
    dat$value[dat$parameter == "pZoteroID"] <- input$pzoteroid
    
    dat$value[dat$parameter == "pID"] <- ifelse(pform()$value[pform()$parameter == "pID"] == "", 
                                                 uuid::UUIDgenerate(),
                                                 pform()$value[pform()$parameter == "pID"])
    
    dat
    
  })
  

  
  #
  
  
  
  
  
  
  
  # ' ------------

  

 

  
#*********************************************************************************
  
  # DOWNLOAD  ----
  # download the new or edited profile as csv
  # the data is set as uploadedPub(), but that needs to be changed later
  output$downloadData <- downloadHandler(
    filename = function() {
        paste0("pProfile_", 
             gsub(":", "-", Sys.time()),
             ".csv")  # the extension doesn't work ...
    },
    content = function(file) {
      write.csv(pExport(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
})


shinyApp(ui = ui, server = server)






# '-------------
# ' ------------
# ' ------------


# NOTES ----

# *UUID ####
#uuid::UUIDgenerate()



# *Input: 'folder' ----
# this could've been a good solution,
# but web browsers don't allow selecting entire
# folders. This code is linked with shinyChoseDir()
# This code could still work with a dockerised app, or RInno app.
## shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),




## *Output: 'folderName' ----
#textOutput('folderName'),


# *Print folder name ----
# this is not used (see Input: folder)
#output$folderName <- renderText({input$folder})

# *Chose folder ------------
# Web browsers do not allow one to list files inside folder!
# But in case we opt for local hosting, eg using RInno,
# this is a way to let the user navigate to the directory for the publication profiles
# If the GitHub.repo was cloned, it will be under data/publicationProfiles 

#shinyDirChoose(input, 'folder', 
#              roots=c('C' = 'C:/', 'home' = '/home'), # the directory needs to be on C or /home. 
#               allowDirCreate = F) 


# *RInno  -------
#https://mran.revolutionanalytics.com/snapshot/2017-03-22/web/packages/RInno/vignettes/Deployment.html
#https://ficonsulting.github.io/RInno/


### *WD  ------------
# Not sure what this is, but will forget about it for now
##global <- reactiveValues(datapath = getwd())
##dir <- reactive(input$folder)  # not sure this is actually needed

##observeEvent(ignoreNULL = TRUE,
##             eventExpr = {
##               input$folder
##             },
##             handlerExpr = {
##               if (!"path" %in% names(dir())) return()    # not sure why this is needed
##               home <- normalizePath("~")
##               global$datapath <-
##                 file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
##             })
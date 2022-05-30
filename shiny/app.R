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
      
      tags$div(title = "Use this functionality to find an already existing publication profile in order to edit it. 
               Navigate to the folder containg the file you want. If there are more than one file in the folder then slect all (Ctrl+A).
               All the files need to me csv-files created using this app.",
      fileInput("localPub", "Choose CSV files",
                multiple = T,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))),
      
      
      # Input: 'pubDrop'  ----
      # Drop down list of publication profiles
      # Profiles (for indicators and publications alike)
      # are stored using time stamps as file names. 
      # To find and upload the correct file (to modify it)
      # requires that we first must read the indicator names
      # stored inside all the files
      tags$div(title = "This dropdown menu is poplated with publication titles from the files you selected above. Pick the one you want.",
      pickerInput('pubDrop', 'Select publication by title',
                  choices = NA,
                  options = list(
                    `live-search` = TRUE))),
      
      
      
      
      
            
        # Horizontal line
          tags$hr(),
            
      # Input: 'header' ----
      #Checkbox if file has header
          checkboxInput("header", "Header", TRUE),
            
      # Input: 'sep' ----
      #Select separator
          tags$div(title = "This option chould not be changed - all files from this app are save using ',' as seperator.",
          radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                             selected = ",")),
            
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
                    selected = "head"),
      
      
      
      
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
        
               
      # POPULATE FORM ----     
        #actionButton('populate', 'Populate form with values from the uploaded file'),       
      
      
      h5("Hover the input fields for more information and examples of use"),
      
        tags$div(title = "Populate form from uploaded file. \n\nOBS! Toggling the switch below will reset the form and delete unsaved work.",
        materialSwitch(
          inputId = "populate",
          label = "Populate form",
          value = FALSE, 
          status = "info")),
        
      
        
        
        # create some space
        br(), br(),
        
      # INPUT pZoteroID ----
        tags$div(title = "Example: https://www.zotero.org/groups/4630169/the_rescalable_indicator_review/collections/KDCY6DCS/items/8GALH26U/collection",
        textInput("pzoteroid", 
                  "Enter the full URL for the Zotero entry", 
                  value = "")),
      
      # INPUT pTitle ----
        tags$div(title = "Example: 'Norwegian Arctic Tundra: a Panel-based Assessment of Ecosystem Condition'",
        textInput("ptitle", 
                "Enter publication title", 
                value = "")),
      
      # INPUT pBibliography ----
        tags$div(title = "Example: Jepsen, Jane Uhd; Speed, James David Mervyn; Austrheim, Gunnar; Rusch, Graciela; Petersen, Tanja Kofod; Asplund, Johan;, Bjerke, Jarle W.; et al. “Panel-Based Assessment of Ecosystem Condition – a Methodological Pilot for Four Terrestrial Ecosystems in Trøndelag.” NINA Rapport. Vol. 2094, 2022.",
        textInput("pbibliography", 
                "Enter the full reference to the publication", 
                value = "")),
      
      # INPUT githubUser ----
        tags$div(title = "Example: 'anders-kolstad'. \n\nNote: please update the contact info in you GitHub profile.", 
               textInput("githubuser", 
                         "Enter your GitHub user name", 
                         value = "")),
      
      # INPUT pRealm ----
      tags$div(title = "Only publications referring to the terrestrial realm will be considered past this point.",  
          checkboxGroupButtons(
          inputId = "pRealm",
          label = "Does the publication include indikatorsrelated to the terrestrial realm?",
          choices = "Terrestrial",
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square", 
                       style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o", 
                      style = "color: steelblue"))
         )),
      
      
      conditionalPanel("input.pRealm == 'Terrestrial'",
      # INPUT pNormalised ----
      checkboxGroupButtons(
        inputId = "pNormalised",
        label = "Containing normalised indicators", 
        choices = c("Yes", "No"),
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-square-o", 
                      style = "color: steelblue"))
      ),
      
      conditionalPanel("input.pNormalised == 'Yes'",
      # INPUT pRedundant ----
      tags$div(title = "Is the publication related to another reference? For example, pre-prints and published versions are related, and we only want to consider one of them.",  
        radioGroupButtons(
         inputId = "pRedundant",
         label = "Redundant?",
         choices = c("Redundant", "Possibly redundant", "Unique"),
         selected = "Unique",
         checkIcon = list(
           yes = tags$i(class = "fa fa-check-square", 
                        style = "color: steelblue"),
           no = tags$i(class = "fa fa-square-o", 
                       style = "color: steelblue"))
        )))),
      
      
      # INPUT Replace ----
        tags$div(title = "Click to create a new file name (and hence a new file) for the csv file that you are about to export. The default is to replace the csv file that you uploaded, or to create new csv file if you started this form form scratch.",
          radioGroupButtons(
           inputId = "replace",
           label = "Replace the uploaded file?",
           choices = c("Yes", 
                       "No"),
           justified = TRUE
          )
        ),
        
        h6("This is the new filename:"),
        textOutput('newFileName'),
            
        tags$div(title = "Click to open a 'Save as' dialogue window. Manually add 'csv' after the file name, but DO NOT change the file name itself.",
        downloadButton("downloadData", "Download"))
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
      temp[nrow(temp)+1,] <- c("filename_local", input$localPub$name[input$localPub$datapath == x])
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
  
  # REACT: title to filename ----
  # Take the chosen publication title from pubDrop
  # and link it to the corresponding file path
  titleToFilename <- reactive({
    p <- publicationList()
    p <- p$filename_local[p$pTitle == input$pubDrop]
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
        df <- uploadedPub()
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
  
  ## pTitle ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'ptitle',
                    value = pform()$value[pform()$parameter == "pTitle"])
    
  })
  
  ## pBibliography ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'pbibliography',
                    value = pform()$value[pform()$parameter == "pBibliography"])
    
  })
  
  ## githubUser ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'githubuser',
                    value = pform()$value[pform()$parameter == "githubUser"])
    
  })
  
  ## pRealm ----
  observeEvent(input$populate, {
    updateCheckboxGroupButtons(session = session,
                    'pRealm',
                    choices = "Terrestrial",
                    selected = pform()$value[pform()$parameter == "pRealm"])
    
  })

  ## pNormalised ----
  observeEvent(input$populate, {
    updateCheckboxGroupButtons(session = session,
                               'pNormalised',
                               choices = c("Yes", "No"),
                               selected = pform()$value[pform()$parameter == "pNormalised"])
    
  })
  
  
#*********************************************************************************
  # OUTPUT: pExport   ----
  # Compile CSVs for the publication profile
  
  
  pExport <- reactive({
    
    # shorten name
    dat <- pform()
    
    #update value column based on input
    dat$value[dat$parameter == "pZoteroID"] <- input$pzoteroid
    
    dat$value[dat$parameter == "pID"] <- ifelse(pform()$value[pform()$parameter == "pID"] == "", 
                                                 uuid::UUIDgenerate(),
                                                 pform()$value[pform()$parameter == "pID"])
    
    dat$value[dat$parameter == "pTitle"] <- input$ptitle
    
    dat$value[dat$parameter == "pBibliography"] <- input$pbibliography

    dat$value[dat$parameter == "githubUser"] <- input$githubuser
    
    dat$value[dat$parameter == "pRealm"] <- input$pRealm
    
    dat$value[dat$parameter == "pNormalised"] <- input$pNormalised
    
    dat$value[dat$parameter == "pRedundant"] <- input$pRedundant
    
    dat
    
  })
  

  
  
  
  
  
  # ' ------------

  

 

  
#*********************************************************************************
  # Replace? ----
  fileName <- reactive({
    ifelse(input$replace == "Yes",
           basename(titleToFilename()),
           paste0("pProfile_", 
                  gsub(":", "-", Sys.time()),
                  ".csv"))
    })
  
  
  output$newFileName <- renderText(
                      fileName())
    
  
  
  # DOWNLOAD  ----
  # download the new or edited profile as csv
  # the data is set as uploadedPub(), but that needs to be changed later
  output$downloadData <- downloadHandler(
    filename = fileName(),
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

# TOP ----
library(shiny)
#library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
library(shinyFiles)
library(shinyWidgets)
library(DT)
library(uuid)
#library(tidyverse)
#library(readxl)


publicationParameters <- 
  read.csv("shinyData/publicationProfileFields.csv", sep=";")
# Column1 (parameters) =  list of terms for the publication profiles
# Column2 (values) =  NA

# Duplicate data set for export 
#pExport <- publicationParameters

ISO3166 <- read.csv("shinyData/ISO3166.csv", sep=";")
ISO3166_v2 <- setNames(ISO3166$Alpha.2.code, ISO3166$English.short.name)




# UI ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤----------------------------------------------------------------------



ui <- 
  navbarPage("The IndiMap",
             


# '-------------       
# **TAB Register Publication ----
      tabPanel("Register publication",
      
sidebarLayout(
  sidebarPanel(width = 6,        
      
      h5(tags$i("Hover the input fields for more information and examples of use")),
      
      
      # 3 INPUT pNew  ----
      tags$div(title = "Click Edit to import and modify a existing publication profile, or click Create new to start processing a new publication.",  
               radioGroupButtons(
                 inputId = "pNew",
                 label = NULL,
                 choices = c("Edit", "Create new"),
                 selected = "Create new"
               )),
      
      # 3 INPUT localPub ----      
      # Here's an option for manually locating the local folder 
      # containing unpublished (unpushed) publication profiles.
      # The files, or paths, in the folder are listed, read, and compiled.
      conditionalPanel("input.pNew == 'Edit'",
                       
                       h3("INFORMATION"),
                       h5("Here you can choose to upload a cvs file so that you can modify them.\n
         In order to find the correct file, first use the 'Choose CVS files' 
         function to select all the crypically names files (typically using Ctrl+A inside
         the main folder containing the publication og indikator profiles).
         Then choose the correct file from the dropdown list. A preview of the import 
         allows you to adjust import settings like headers and seperators."),
                       
                       
                tags$div(title = "Use this functionality to find an already existing publication profile in order to edit it. 
        Navigate to the folder containg the file you want. If there are more than one file in the folder then slect all (Ctrl+A).
        All the files need to me csv-files created using this app.",
                         
      fileInput("localPub", "Choose CSV files",
                multiple = T,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))),
      
      # 3 INPUT pubDrop ----
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
      
      # 3 INPUT header ----
      #Checkbox if file has header
      checkboxInput("header", "Header", TRUE),
      
      # 3 INPUT sep ----
      #Select separator
      tags$div(title = "This option should normally not be changed - all files from this app are saved using ',' as seperator.",
               radioGroupButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ",")),
      
      # 3 INPUT quote ----
      # Select quotes
      radioGroupButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # 3 INPUT disp ----
      # Select number of rows to display
      radioGroupButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # 3 INPUT populate ----
      tags$div(title = "Populate form from uploaded file. \n\nOBS! Toggling the switch below will reset the form and delete unsaved work.",
               materialSwitch(
                 inputId = "populate",
                 label = "Populate form (careful...)",
                 value = FALSE, 
                 status = "info"))
      ),
                       
        
        
      
      tags$hr(),
        
        # create some space
        br(), br(),
        
      # 3 INPUT pZoteroID ----
        tags$div(title = "Example: https://www.zotero.org/groups/4630169/the_rescalable_indicator_review/collections/KDCY6DCS/items/8GALH26U/collection",
        textInput("pzoteroid", 
                  "Enter the full URL for the Zotero entry", 
                  value = "")),
      
      # 3 INPUT pTitle ----
        tags$div(title = "Example: 'Norwegian Arctic Tundra: a Panel-based Assessment of Ecosystem Condition'",
        textInput("ptitle", 
                "Enter publication title", 
                value = "")),
      
      # 3 INPUT pBibliography ----
        tags$div(title = "Example: Jepsen, Jane Uhd; Speed, James David Mervyn; Austrheim, Gunnar; Rusch, Graciela; Petersen, Tanja Kofod; Asplund, Johan;, Bjerke, Jarle W.; et al. “Panel-Based Assessment of Ecosystem Condition – a Methodological Pilot for Four Terrestrial Ecosystems in Trøndelag.” NINA Rapport. Vol. 2094, 2022.",
        textInput("pbibliography", 
                "Enter the full reference to the publication", 
                value = "")),
      
      # 3 INPUT githubUser ----
        tags$div(title = "Example: 'anders-kolstad'. \n\nNote: please update the contact info in you GitHub profile.", 
               textInput("githubuser", 
                         "Enter your GitHub user name", 
                         value = "")),
      
      
  # 3 INPUT pRedundant ----
      tags$div(title = "Is the publication related to another reference? For example, pre-prints and published versions are related, and we only want to consider one of them. Chosing anything but 'Unique' here will work to flag the publication, but it will not remove it. If the publication is very clearly a duplicate of another, then you should exclude the least relevant publication. Consult the review team if in doubt.",
               
        radioGroupButtons(
         inputId = "pRedundant",
         label = "Redundant?",
         choices = c("Unique", "Possibly redundant",  "Redundant"),
         selected = "Unique"
    #    ))
      )),
      
  # 3 INPUT pType ----
  tags$div(title = "Chose the relevant publication type from the list.\n\n
               'Unpublished' in this case means it is not publically available.",  
           radioGroupButtons(
             inputId = "pType",
             label = "Type of publication",
             choices = c("Peer-reviewed article", 
                         "Book", 
                         "Book chapter",
                         "Rapport",
                         "Web resource",
                         "Unpublished"),
             selected = NULL
           )),
  
  
  # 3 INPUT pJournal ----
  # This could perhaps be standardised with a drop down menu later
  conditionalPanel("input.pType == 'Peer-reviewed article'",
  tags$div(title = "Example: 'Ecological Indicators", 
           textInput("pJournal", 
                     "Enter the journal name, without abbreviations", 
                     value = ""))),
  
  # 3 INPUT pAssessment ----
  tags$div(title = "Does the publication contain an assessment of ecosystem condition based on multiple indicators?",
           radioGroupButtons(
             inputId = "pAssessment",
             label = "Ecosystem condition assessment?",
             choices = c("Assessment", 
                         "Not an assessment"),
             selected = "Assessment"
           )
  ),
  
  # 3 INPUT pEAAextent ----
  conditionalPanel("input.pAssessment == 'Assessment'",
  tags$div(title = "The spatial extent of the ecosystem assessment/accounting area(s)
           
           Sub-local here means a scale lower than the typical administrative unit",
           radioGroupButtons(
             inputId = "pEAAextent",
             label = "The extent of the Ecosystem Accounting Area",
             choices = c("global", "continent", "country", "region", "local", "sub-local"),
             selected = NULL
           )
          ),
  
  # 3 INPUT pEAAarea ----
  tags$div(title = "Example: 385207\n\n The area (km2) of the ecosystem assessment/accounting area(s). No spaces.",
         numericInput(
           inputId = "pEAAarea",
           label = "The total area of the Ecosystem Accounting Area in km2",
           value = NA,
           min = 0
         )
       ),
  
  # 3 INPUT pAssessmentYear ----
  tags$div(title = "Example: 2022
           
           The final year covered by the overall assessment",
           numericInput(
             inputId = "pAssessmentYear",
             label = "Assessment year",
             value = NA,
             min = 1950,
             max = 2050
           )
  ),
  
  # 3 INPUT pAggregation ----
 
  tags$div(title = "The highest level of spatial aggregation of the condition estimate(s) reported in the publication. Only relevant for normalised indicator sets.
                    Examples: 0 = indicators reported seperately with no aggregation
                              1 = E.g. an Amphibian index, or a SEEA ECT class
                              2 = Basic Spatial Unit. The finest spatial scale where data is available. E.g. a grid cell or a municipality
                              3 = Ecosystem Asset. The finest spatial scale where indicators can be aggregated. E..g a county. If EA = BSU, then pick BSU
                              4 = Ecosystem type. Chose this is indicators are aggregtaed to produce one condition value for an entire ecosystem type with the EAA 
                              5 = Ecosystem Accounting Area. Chose this option if the publication has aggregated condition estimates accross ETs",
           numericInput(
             inputId = "pAggregation",
             label = "Level of aggregation",
             value = NA,
             min = 0,
             max=5
           )),
    h5("0 - None (i.e. metric level)"),
    h5("1 - Thematic level (e.g. a species group or some level that does not span several SEEA ECT classes"),
    h5("2 - BSU level"),
    h5("3 - EA level"),
    h5("4 - ET level"),
    h5("5 - EAA level"),
  
  
  # 3 INPUT pAggregationRemark ----
  tags$div(title = "Example: 'They have only one indicator for condition, but they have aggregeted that across all assests.' (level 1)
           
           Example 2: Most indicators are not rescaled or aggregeted. Some indicators are aggregated to ECT class level (level 1).",
           
           textInput("pAggregationRemark", 
                     "Remarks to the chosen level of aggregation", 
                     value = "")),
  
  # 3 INPUT pTotalNumberOfIndicators ----
  conditionalPanel("input.pAggregation > 1",
  tags$div(title = "If level of aggregation >=2 : The total number of indicators used in the aggregated condition estimation",
           numericInput(
             inputId = "pTotalNumberOfIndicators",
             label = "Number of (aggregated) indicators",
             value = NA,
             min = 0
           ))),
                   
  ),
  
  # 3 INPUT pDirective ----
  tags$div(title = "Tick of the boxes that the publication explicitly states that it is reporting to",
           checkboxGroupButtons(
             inputId = "pDirective",
             label = "Reported to the following programs",
             choices = c("Not relevant",
                         "EU Birds Directive",
                         "EU Habitats Directive")
           )),
           
           
  # 3 INPUT Replace ----
      tags$div(title = "Chose whether to create a new file name (and hence a new file) for the csv file that you are about to export, or to overwrite the uploaded file that you have edited.",
          radioGroupButtons(
           inputId = "replace",
           label = "Replace the uploaded file?",
           choices = c("Replace the uploaded file", 
                       "Create a new file")
          )
        ),
        
        h6("This is the new filename:"),
        textOutput('newFileName'),
        h6("Don't create a new file name if you have edited an existing file. The UUIDs will not have changed."),    
  
  
  
  
  
  
  
    # add some space at the bottom
    br(), br()
               ),
  
  
  
# '-------------
  mainPanel(width = 6,
            
    conditionalPanel("input.pNew == 'Edit'",      
        # 3 OUTPUT: uploaded ----
      h5("Preview of the uploaded publication profile (before any new edits):"),
      tableOutput("uploaded")     
    ),
            
    br(),
    # 3 OUTPUT previewP ----
    h4("Publication profile"),
    h6("This is what you download when you press the button below this table"),
    DTOutput('previewP'),
    
    # 3 DOWNLOAD ----
    
    tags$hr(),  # Horizontal line
    h3("Download the publication profile"),
    h6("If you have a copy of the project github repo on you computer, 
       you probably want to save this under 'data/publicationProfiles' 
       so that you can upload them later to the main branch via a pull request."),
    h6("If this is greek to you, you can save it to any local and dedicated folder
       on you computer and contact the project leader about how to get you csv file
       submitted to the mina githib branch"),
    tags$div(title = "Click to open a 'Save as' dialogue window.\n\nDO NOT change the file name.",
             downloadButton("downloadData", "Download"))
  )
  )
), # end tab



        
# '-------------
# **TAB Register indicator ----
  tabPanel("Register indicator",
      sidebarLayout(
        sidebarPanel(width = 6,     
    h5(tags$i("Hover the input fields for more information and examples of use")),             

  # 4 INPUT iNew  --------
  tags$div(title = "Click Edit to import and modify a existing publication profile, or click Create new to start processing a new publication.",  
         radioGroupButtons(
           inputId = "iNew",
           label = NULL,
           choices = c("Edit", "Create new"),
           selected = "Create new"
         )),

# 4 INPUT localInd ----      
# Here's an option for manually locating the local folder 
# containing unpublished (unpushed) indicator profiles.
# The files, or paths, in the folder are listed, read, and compiled.
conditionalPanel("input.iNew == 'Edit'",
                 
                 h3("INFORMATION"),
                 h5("Here you can choose to upload a cvs file so that you can modify them.\n
         In order to find the correct file, first use the 'Choose CVS files' 
         function to select all the crypically names files (typically using Ctrl+A inside
         the main folder containing the publication og indikator profiles).
         Then choose the correct file from the dropdown list. A preview of the import 
         allows you to adjust import settings like headers and seperators."),
                 
                 
                 tags$div(title = "Use this functionality to find an already existing indicator profile in order to edit it. 
        Navigate to the folder containg the file you want. If there are more than one file in the folder then slect all (Ctrl+A).
        All the files need to be csv-files created using this app.",
                          
                          fileInput("localInd", "Choose CSV files",
                                    multiple = T,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv"))),
                 
    # 4 INPUT indDrop ----
    # Drop down list of publication profiles
    # Profiles (for indicators and publications alike)
    # are stored using time stamps as file names. 
    # To find and upload the correct file (to modify it)
    # requires that we first must read the indicator names
    # stored inside all the files
    tags$div(title = "This dropdown menu is poplated with indicator names from the files you selected above. Pick the one you want.",
             pickerInput('indDrop', 'Select indicator by its name',
                         choices = NA,
                         options = list(
                           `live-search` = TRUE))),
                 
    # 4 INPUT i_header ----
    #Checkbox if file has header
    checkboxInput("i_header", "Header", TRUE),
                 
    # 4 INPUT i_sep ----
    #Select separator
    tags$div(title = "This option should normally not be changed - all files from this app are saved using ',' as seperator.",
             radioGroupButtons("i_sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ",")),
                 
    # 4 INPUT i_quote ----
    # Select quotes
    radioGroupButtons("i_quote", "Quote",
                      choices = c(None = "",
                                  "Double Quote" = '"',
                                  "Single Quote" = "'"),
                      selected = '"'),
                 
    # 4 INPUT i_disp ----
    # Select number of rows to display
    radioGroupButtons("i_disp", "Display",
                      choices = c(Head = "head",
                                  All = "all"),
                      selected = "head"),
                 
    # 4 INPUT i_populate ----
    tags$div(title = "Populate form from uploaded file. \n\nOBS! Toggling the switch below will reset the form and delete unsaved work.",
             materialSwitch(
               inputId = "i_populate",
               label = "Populate form (careful...)",
               value = FALSE, 
               status = "info"))
),




tags$hr(),

# create some space
  br(), br(),


# 4 INPUT localPubTitles 
h4("Tie the indicator to the correct publication"),
h5("Click", style = "display: inline;"), 
tags$i("Choose CVS files",style = "display: inline;"),
h5("and navigate to the folder 
   containing all the publication profiles. Select all 
   the files in tha folder using Ctr+A and press Open", 
   style = "display: inline;"),

# I've only added the functionality to import local publicatio profiles, and not profiles from GitHub.
# This can be added later potentially.
tags$div(title = "Use this functionality to locate the already existing publication profile so that you may later find the publication associated with this indicator. 
         All the files need to me csv-files created using this app.",
  
  # 4 INPUT localPub2 ----
  fileInput("localPub2", "Choose CSV files",
                   multiple = T,
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"))),

  # 4 INPUT pubDrop2 ----
  # Drop down list of publication titles
  tags$div(title = "This dropdown menu is poplated with publication titles from the files you selected above. Pick the one you want.",
           pickerInput('pubDrop2', 'Select the associated publication by it title',
                       choices = NA,
                       options = list(
                         `live-search` = TRUE))),

  tags$hr(),

  # 4 INPUT iName ----
  tags$div(title = "Type a human readable name for the indicator. Preferreably unique. For example: Tree cover Netherlands.",
         textInput("iName", 
                   "Indicator name", 
                   value = "")),

  
  # 4 INPUT githubUser2 ----
  tags$div(title = "Example: 'anders-kolstad'. \n\nNote: please update the contact info in you GitHub profile.", 
           textInput("githubuser2", 
                     "Enter your GitHub user name", 
                     value = "")),

  # 4 INPUT iRedundant ----
  tags$div(title = "Is the indicator described in another reference? For example, an indicator can we presented both in a national assessment and in a stand-alone peer-reviewd paper. Selecting 'Yes' or 'Partly' here will work to flag it as potentially redundant. The following remarks field will help the analyst in making this call later. If the indicator is clearly the same as another indiator describes elsewhere (i.e. same data set, same method, same temporal scope, everything), then you may chose to only register the indiator once, using the most appropriate publication (ideally an assessment) as the source.",  
           radioGroupButtons(
             inputId = "iRedundant",
             label = "Redundant?",
             choices = c("No", "Partly", "Yes"),
             selected = "No"
           )),

  # 4 INPUT iRedundantRemarks ----
conditionalPanel(condition =  "input.iRedundant != 'No'",
  tags$div(title = "Use tis field to elaborate if you chose 'Partly or 'Yes' above",
           textInput("iRedundantRemarks", 
                     "Remarks to the above", 
                     value = ""))),

 # 4 INPUT iContinent ----
tags$div(title = "Select the continent(s) where the indicator has been applied, eiether as a test or as part of an assessment.",
         pickerInput(
           inputId = "iContinent",
           label = "Continent(s)",
           multiple = T,
           choices = c("Africa",
                       "Antactica", 
                       "Asia",
                       "Australia",
                       "Europe",
                       "North America",
                       "South America"
                       ),
           options = list(
             `multiple-separator` = " | "
           )
         )),

  # 4 iCountry ----
  tags$div(title = "Search and select the country(ies) where the indicator has been applied, eiether as a test or as part of an assessment.",
     pickerInput('iCountry', 'Country',
          choices = ISO3166_v2,
          multiple = T,
          options = list(
            `live-search` = TRUE,
            `actions-box` = TRUE,
            `deselect-all-text` = "Deselect all",
            `multiple-separator` = " | "
            ))),

  # 4 iLowerGeography
  tags$div(title = "If relevant, type the name of any lower geography, i.e. the name of an area/place/region within a country, where the indicator has been applied.
           Example: Oslo",
         textInput("iLowerGeography", 
                   "Lower geography (if relevant)", 
                   value = "")),

  # 4 INPUT iLatitude
  conditionalPanel("input.iLowerGeography != ''",
    tags$div(title = "Enter the Latitude of the Lower Geography in decimal degrees WGS84",
        numericInput("iLatitude", 
                  "Latitude",
                  value = NA,
                  min = -90,
                  max = 90,
                  step = 0.1)),
    tags$div(title = "Enter the Longitude of the Lower Geography in decimal degrees WGS84",
        numericInput("iLongitude", 
                     "Longitude",
                     value = NA,
                     min = -90,
                     max = 90,
                     step = 0.1))
    ),

  # 4 dName
  tags$div(title = "The name of the underlying dataset(s) (comma seperated). If there are several underlying datasets, consider mentioning only the most essential one, the one that determines the main characteristics of the resulting indicator.
           Example: The Norwegian Forest Inventory, Living Planet Index",
         textInput("dName", 
                   "Dataset name(s)", 
                   value = "")),

  # 4 dReference
  tags$div(title = "If possible, enter a reference (e.g. url, doi) to the dataset(s) (comma seperated) metioned above",
         textInput("dReference", 
                   "Dataset reference(s)", 
                   value = "")),

  # 4 dOrigin ----
  tags$div(title = "The origin of the munderlying dataset. If the indicator requires modelling, this question asks about the data that goes into the model, not the model output. If the indicator is designed around several datasets, consider if one dataset is more important than the rest, and report only for that. Otherwise, yuo may also check multiple boxes here to account for multiple datasets with different origins.",
      pickerInput('dOrigin', 'Dataset origin',
                     choices = c("RS - remotely sensed",
                                 "MP - established monitoring program",
                                 "FS - field sampling",
                                 "CS - crowd sourced",
                                 "EO - expert opinion",
                                 "OT - others or unsure"),
                     multiple = T,
                     options = list(
                       `multiple-separator` = " | "
                     )))





 ), # end side panel
  mainPanel(width = 6)
 )
),


# '-------------             
# **TAB More ----
    navbarMenu("More",
      tabPanel("Instructions"),
      
# 5 Instructions----

      tabPanel("Contact")

# 5 Contact ----
             )
  )
    
  
# ' ------------
# ' ------------
# ' ------------


# SERVER ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤----------------------------------------------------------------------
# '-------------       



server <- function(input, output, session) ({
  
#*********************************************************************************


  
  
  
  
  
  # ' ------------
# ** TAB Register Publication ----
  
  # '-------------       
  
  # *** UPLOAD FILE -------------------------------
  # A* REACT: publicationList ----------------
  
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
  
  # A* REACT: publicationList2 ----------------
  # Now a duplicat eof the same reactive elemet for use in in pID
  publicationList2 <- reactive({
    req(input$localPub2)
    # Read the csv's, add path and ID, and finally rbind them
    combined <- plyr::ldply(input$localPub2[,"datapath"], function(x) {
      temp <- read.csv(x,
                       header = T,    #   input$i_header,
                       sep    =  ",", #   input$i_sep,
                       quote  =  '"'  #   input$i_quote
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
  
  # A* REACT: title to path ----
  # Take the chosen publication title from pubDrop
  # and link it to the corresponding file path
  titleToPath <- reactive({
    p <- publicationList()
    p <- p$filename[p$pTitle == input$pubDrop]
    p
  })
  
  # A* REACT: title to filename ----
  # Take the chosen publication title from pubDrop
  # and link it to the corresponding file path
  titleToFilename <- reactive({
    p <- publicationList()
    p <- p$filename_local[p$pTitle == input$pubDrop]
    p
  })
  
  
  #*********************************************************************************  
  
  # A* UPDATE pubDrop ----
  # reactive list of publications titles
  observeEvent(input$localPub, {
    updatePickerInput(session = session, inputId = "pubDrop",
                      choices = unique(publicationList()$pTitle))
  })
  
  
  #*********************************************************************************
  
  # A* REACT: uploadedPub ----
  # Make the uploaded publication profile file available through an reactive element 
  
  uploadedPub <- reactive({
    read.csv(
      titleToPath(),
      header = input$header,
      sep = input$sep,
      quote = input$quote)
  })
  
  #*********************************************************************************
  
  # A* OUTPUT uploaded ----
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
  
  # A* REACT pform ----
  pform <- reactive({
    ifelse(input$populate == FALSE, 
           return(publicationParameters),
           return(uploadedPub()))
  })
  # '-------------       
 
  
  
  # B* UPDATEs: ----
  
  ## pZoteroID ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'pzoteroid',
                    value = pform()$value[pform()$parameter == "pZoteroID"])
  })
  
  ## pID ----
  pUUID <- reactive({
    ifelse(is.na(pform()$value[pform()$parameter == "pID"]), 
           uuid::UUIDgenerate(),
           pform()$value[pform()$parameter == "pID"])
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
  
  
#  ## pRealm ----
#  observeEvent(input$populate, {
#    updateCheckboxGroupButtons(session = session,
#                    'pRealm',
#                    choices = "Terrestrial",
#                    selected = pform()$value[pform()$parameter == "pRealm"])
#    
#  })
#
#  ## pNormalised ----
#  observeEvent(input$populate, {
#    updateCheckboxGroupButtons(session = session,
#                               'pNormalised',
#                               choices = c("Yes", "No"),
#                               selected = pform()$value[pform()$parameter == "pNormalised"])
#    
#  })
  
  ## pRedundant ----
  observeEvent(input$populate, {
    updateRadioGroupButtons(session = session,
                    'pRedundant',
                    choices = c("Redundant", "Possibly redundant", "Unique"),
                    selected = pform()$value[pform()$parameter == "pRedundant"])
    
  })
  
  ## pType ----
  observeEvent(input$populate, {
    updateRadioGroupButtons(session = session,
                            'pType',
                            choices = c("Peer-reviewed article", 
                                        "Book", 
                                        "Book chapter",
                                        "Rapport",
                                        "Web resource",
                                        "Unpublished"),
                            selected = pform()$value[pform()$parameter == "pType"])
    
  })
  
  ## pJournal ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'pJournal',
                    value = pform()$value[pform()$parameter == "pJournal"])
    
  })
  
  ## pAssessment ----
  observeEvent(input$populate, {
    updateRadioGroupButtons(session = session,
                            'pAssessment',
                            choices = c("Assessment", 
                                        "Not an assessment"),
                            selected = pform()$value[pform()$parameter == "pAssessment"])
  })
  
  ## pEAAextent ----
  observeEvent(input$populate, {
    updateRadioGroupButtons(session = session,
                            'pEAAextent',
                            choices = c("global", "continent", "country", "region", "local", "sub-local"),
                            selected = pform()$value[pform()$parameter == "pEAAextent"])
  })
  
  ## pEAAarea ----
  observeEvent(input$populate, {
    updateNumericInput(session = session,
                            'pEAAarea',
                            value = pform()$value[pform()$parameter == "pEAAarea"])
  })
  
  ## pAssessmentYear ----
  observeEvent(input$populate, {
    updateNumericInput(session = session,
                       'pAssessmentYear',
                       value = pform()$value[pform()$parameter == "pAssessmentYear"])
  })
  
  
  ## pAggregation ----
  observeEvent(input$populate, {
    updateNumericInput(session = session,
                            'pAggregation',
                            value = pform()$value[pform()$parameter == "pAggregation"])
  })
  
  ## pAggregationRemark ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'pAggregationRemark',
                    value = pform()$value[pform()$parameter == "pAggregationRemark"])
    
  })
  
  ## pTotalNumberOfIndicators ----
  observeEvent(input$populate, {
    updateNumericInput(session = session,
                       'pTotalNumberOfIndicators',
                       value = pform()$value[pform()$parameter == "pTotalNumberOfIndicators"])
  })
  
  ## pDirective ----
  observeEvent(input$populate, {
    updateCheckboxGroupButtons(session = session,
                            'pDirective',
                            choices = c("Not relevant",
                                        "EU Birds Directive",
                                        "EU Habitats Directive"),
                            selected = stringr::str_split(
                                             pform()$value[pform()$parameter == "pDirective"],
                                             " \\| ", simplify = T))
  })
  
 
  
#*********************************************************************************

# '-------------
# B* OUTPUT: pExport   ----
  
# Compile CSVs for the publication profile
  
  
  pExport <- reactive({
    
    # shorten name
    dat <- pform()
    
    #update value column based on input
    dat$value[dat$parameter == "pZoteroID"] <- input$pzoteroid
    
    dat$value[dat$parameter == "pID"] <- pUUID()
      #ifelse(is.na(pform()$value[pform()$parameter == "pID"]), 
      #  uuid::UUIDgenerate(),
      #  pform()$value[pform()$parameter == "pID"])
    
    dat$value[dat$parameter == "pTitle"] <- input$ptitle
    
    dat$value[dat$parameter == "pBibliography"] <- input$pbibliography

    dat$value[dat$parameter == "githubUser"] <- input$githubuser
    
    # These are cut out and treated as inclusion criteria:
    #dat$value[dat$parameter == "pRealm"] <- input$pRealm
    #dat$value[dat$parameter == "pNormalised"] <- input$pNormalised
    
    dat$value[dat$parameter == "pRedundant"] <- input$pRedundant

    dat$value[dat$parameter == "pType"] <- input$pType

    dat$value[dat$parameter == "pJournal"] <- ifelse(input$pType == "Peer-reviewed article", input$pJournal, NA)
    
    dat$value[dat$parameter == "pAssessment"] <- input$pAssessment
    
    dat$value[dat$parameter == "pEAAextent"] <- ifelse(input$pAssessment == "Assessment", input$pEAAextent, NA)
    
    dat$value[dat$parameter == "pEAAarea"] <- ifelse(input$pAssessment == "Assessment", input$pEAAarea, NA)

    dat$value[dat$parameter == "pAssessmentYear"] <- ifelse(input$pAssessment == "Assessment", input$pAssessmentYear, NA)
    
    dat$value[dat$parameter == "pAggregation"] <- ifelse(input$pAssessment == "Assessment", input$pAggregation, NA)

    dat$value[dat$parameter == "pAggregationRemark"] <- ifelse(input$pAssessment == "Assessment", input$pAggregationRemark, NA)
    
    dat$value[dat$parameter == "pTotalNumberOfIndicators"] <- ifelse(input$pAssessment == "Assessment",
                                                                ifelse(input$pAggregation > 1, input$pTotalNumberOfIndicators, NA),
                                                                NA)
    
    dat$value[dat$parameter == "pDirective"] <- paste(input$pDirective, collapse = " | ") 
    
    dat
    
  })
  
  

 

# B* OUTPUT previewP ----
output$previewP <- renderDT(
                      pExport(),
                      options = list(pageLength = 50, 
                                     scrollY = "600px",
                                     scollX = T))
  
#*********************************************************************************
  
  # B* REACT fileName ----
  # Overwrite or create new csv?  
  fileName <- reactive({
    ifelse(input$replace == "Replace the uploaded file",
           ifelse(input$pubDrop == "NA", "Invalid option", basename(titleToFilename())),
           paste0("pProfile_", 
                  gsub(":", "-", Sys.time()),
                  ".csv"))
    })
  
  
  output$newFileName <- renderText(
                      fileName())
    
  
  
  # B* DOWNLOAD  ----
  # download the new or edited profile as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      fileName()
      },
    content = function(file) {
      write.csv(pExport(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
  # '-------------
  # ' ------------
  
  # C* UPDATE pubDrop2 ----
  # reactive list of publications titles
  observeEvent(input$localPub2, {
    updatePickerInput(session = session, inputId = "pubDrop2",
                      choices = unique(publicationList2()$pTitle))
  })
  
  
  
})


shinyApp(ui = ui, server = server)






# '-------------
# ' ------------


# ''''''''''''''''END'''''''''''''''''''''''''''''''''''''''''' ------------


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
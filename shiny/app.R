
# TOP ----
library(shiny)
library(ggplot2)
library(shinyFiles)
library(shinyWidgets)
library(DT)
library(uuid)
library(shinyalert)

source("shinyData/lookup.R")

publicationParameters <- 
  read.csv("shinyData/publicationProfileFields.csv", sep=";")
indicatorParameters <- 
  read.csv("shinyData/indicatorProfileFields.csv", sep=";")
# Column1 (parameters) =  list of terms for the publication profiles
# Column2 (values) =  NA

# Duplicate data set for export 
#pExport <- publicationParameters

ISO3166 <- read.csv("shinyData/ISO3166.csv", sep=";")
ISO3166_v2 <- setNames(ISO3166$Alpha.2.code, ISO3166$English.short.name)



# UI ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤----------------------------------------------------------------------



ui <- 
  navbarPage(
    # add title and logos inside a div
    title = div(
      
      div(img(src='indimaplogo4.png',
              style="margin-top: -14px;
                               padding-right:15px;
                               padding-bottom:15px",
              height = 60)),
      tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://www.nina.no/\", target=\"_blank\"><img src=\"NINA_logo_sort_txt_engelsk_under.png\" alt=\"alt\" style=\"float:right;width:50px;padding-top:5px;padding-bottom:0px;\"> </a></div>');
header.append('<div style=\"float:right\"><a href=\"https://github.com/anders-kolstad/theIndiMap\", target=\"_blank\"><img src=\"githublogo.png\" alt=\"alt\" style=\"float:right;width:50px;padding-top:5px;padding-bottom:0px;padding-right:15px;\"> </a></div>');
    console.log(header)"))
                ),

    
    # fix the navbar so that it doesn't scroll
    position = "fixed-top", 
    # add padding to the navbar doesn't overlay content
    tags$style(type="text/css", "body {padding-top: 70px;}"), #.navbar { background: #9cbff7; }


# '-------------       
# **TAB Register Publication ----
      tabPanel("Register publication",
              
      
sidebarLayout(
  sidebarPanel(width = 6,        
      style = "position: fixed; width: 45%; height: 90vh; overflow-y: auto;",
      h5(tags$i("Hover the input fields or press the info buttons for more information and examples of use.")),
      
      
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
                       
                       h4("Locate and upload existing publication profile:", style="background-color:lightblue;"),
                       
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
               style="color:red; text-decoration-line: underline;",
               materialSwitch(
                 inputId = "populate",
                 label = "Populate form (careful...)",
                 value = FALSE, 
                 status = "info"))
      ),
                       
        
        
      
      tags$hr(),
        
        # create some space
        br(), br(),
        
      
     
      h4("General fields:", style="background-color:lightblue;"),
      
      # 3 INPUT githubUser ----
      tags$div(title = "Example: 'anders-kolstad'. \n\nNote: please update the contact info in you GitHub profile.", 
               textInput("githubuser", 
                         "Enter your GitHub user name", 
                         value = "")),
     
       # 3 INPUT pTitle ----
        tags$div(title = "Example: 'Norwegian Arctic Tundra: a Panel-based Assessment of Ecosystem Condition'",
        textInput("ptitle", 
                "Enter publication title", 
                value = "")),
      
      # 3 INPUT pRayyanID ----
        tags$div(title = "The Rayyan ID is called System ID inside Rayyan itself. See it by first clicking a row with a publication and read it from the blue screen below. Example: 534633005",
        textInput("pRayyanID", 
                  "Enter the Rayyan ID", 
                  value = "")),
      
      # 3 INPUT pOrigin ----
      tags$div(title = "The original systematic search refers to papers stored in Rayyan under 'Search methods: Uploaded References [The IndiMap Review.bib]'.
               \nThe SEEA EA maintained list refers to this web page: https://seea.un.org/content/knowledge-base
               \nIf you are entering a publication that is not identified either through the initial systematic search, or one that you found on the SEEA EA lst, then choose the third option.",
               pickerInput(
                 inputId = "pOrigin", 
                 label   = "Where did you get this reference from?",
                 choices = origin,
                 options = list(
                   title = "Nothing selected"))),
      
      br(),
      
      
      # 3 INPUT UUID ----
      conditionalPanel("input.pNew == 'Create new'",
        actionButton("puuid_new", "Regenerate UUID"
                   )),
      
      br(),
      
      # 3 INPUT pBibliography ----
        tags$div(title = "Example: Jepsen, Jane Uhd; Speed, James David Mervyn; Austrheim, Gunnar; Rusch, Graciela; Petersen, Tanja Kofod; Asplund, Johan;, Bjerke, Jarle W.; et al. “Panel-Based Assessment of Ecosystem Condition – a Methodological Pilot for Four Terrestrial Ecosystems in Trøndelag.” NINA Rapport. Vol. 2094, 2022.",
        textInput("pbibliography", 
                "Enter the full reference to the publication", 
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
           pickerInput(
             inputId = "pType",
             label = "Type of publication",
             choices = publicationTypes,
             options = list(
               title = "Nothing selected")
           )),
  
  
  # 3 INPUT pJournal ----
  # This could perhaps be standardised with a drop down menu later
  conditionalPanel("input.pType == 'Peer-reviewed article'",
  tags$div(title = "The list is taken from Rayyan and should be complete, but if you have a jounral not listed here you can chose 'Other' and notify Anders Kolstad who will add it to the list.", 
           pickerInput("pJournal", 
                     "Enter the journal name",
                     choices = sort(journals),
                     options = list(
                       title = "Nothing selected")
                     ))),
  
  # 3 INPUT pComment ----
  tags$div(title = "Optional. Add a short description for the publication. 
           Example: 'Paper suggestion new indicator for biodiversity', 
           or 'National ECA with many indicators.'", 
                            textInput("pComment", 
                                      "Comments on the publication", 
                                      value = "")),
  
  
  # 3 INPUT pDirective ----
  tags$div(title = "Tick of the boxes that the publication explicitly states that it is reporting to",
           checkboxGroupButtons(
             inputId = "pDirective",
             label = "Reported to the following programs",
             choices = c("Not relevant",
                         "EU Birds Directive",
                         "EU Habitats Directive")
           )),
  
  h4("Field related to ecosystem assessment publications:", style="background-color:lightblue;"),
  
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
  tags$div(title = "The spatial extent of the ecosystem assessment/accounting area(s)",
           pickerInput(
             inputId = "pEAAextent",
             label = "The extent of the Ecosystem Accounting Area",
             choices = scale1,
             options = list(
               title = "Nothing selected")
           )
          ),
  
  actionButton("pEAAextantINFO", "",
               icon = icon("info")),
  
  # 3 INPUT pEAAarea ----
  tags$div(title = "Example: 385207\n\nThe area (km2) of the ecosystem assessment/accounting area(s). No spaces. If the EAA extant is not reported in the reference it is encouraged that you find out by googleing. It is OK to use an approximate or rounded of number",
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
 
  tags$div(title = "The highest level of spatial aggregation of the condition estimate(s) reported in the publication. Only relevant for normalised indicator sets.",
           numericInput(
             inputId = "pAggregation",
             label = "Level of aggregation",
             value = NA,
             min = 0,
             max=5
           )),
    h5("0 - None"),
    h5("1 - Thematic level"),
    h5("2 - BSU level"),
    h5("3 - EA level"),
    h5("4 - ET level"),
    h5("5 - EAA level"),
  
  actionButton("pAggregationINFO", "",
               icon = icon("info")),
  
  
  
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
             min = 2
           )))
                   
  ),
  
           
  h4("Create file name", style="background-color:lightblue;"),
  
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
        h6("Don't create a new file name if you have edited an existing file. The UUIDs will not have changed.",
           style="color:red;"),    
  
  
  
  
  
  
  
    # add some space at the bottom
    br(), br()
               ),
  
  
  
# '-------------
  mainPanel(width = 6,
            
    conditionalPanel("input.pNew == 'Edit'",      
        # 3 OUTPUT: uploaded ----
      h5("Preview of the uploaded publication profile (before any new edits):", 
         style="background-color:lightgreen;"),
      tableOutput("uploaded")     
    ),
            
    br(),
    
    
    # 3 OUTPUT previewP ----
    h4("Publication profile", style="background-color:lightgreen;"),
    h6("This is what you download when you press the button below this table"),
    DTOutput('previewP'),
    
    # 3 DOWNLOAD ----
    
    tags$hr(),  # Horizontal line
    h4("Download the publication profile", style="background-color:lightgreen;"),
    h6("If you have a copy of the project github repo on you computer, 
       you probably want to save this under 'data/publicationProfiles' 
       so that you can upload them later to the main branch via a pull request."),
    h6("If this is greek to you, you can save it to any local and dedicated folder
       on you computer and contact the project leader about how to get you csv file
       submitted to the main github branch"),
    actionButton("validate_p", "Simplified validation"),
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
        style = "position: fixed; width: 45%; height: 90vh; overflow-y: auto;",
                  
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
                 
                 h4("Locate and upload existing publication profile:", style="background-color:lightblue;"),
                 h5("Here you can choose to upload a cvs file so that you can modify them.\n
         In order to find the correct file, first use the 'Choose CVS files' 
         function to select all the crypically names files (typically using Ctrl+A inside
         the main folder containing the publication og indicator profiles).
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
             style="color:red; text-decoration-line: underline;",
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
h4("Tie the indicator to the correct publication", style="background-color:lightblue;"),
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
           pickerInput('pubDrop2', 'Select the associated publication by its title',
                       choices = NA,
                       options = list(
                         `live-search` = TRUE,
                          title = "Noting selected"
                       ))),

tags$hr(),
h4("General fields:", style="background-color:lightblue;"),

# 4 INPUT githubUser2 ----
tags$div(title = "Example: 'anders-kolstad'. \n\nNote: please update the contact info in you GitHub profile.", 
         textInput("githubuser2", 
                   "Enter your GitHub user name", 
                   value = "")),

br(),
# 4 INPUT UUID ----
conditionalPanel("input.iNew == 'Create new'",
                 actionButton("iuuid_new", "Regenerate UUID"
                 )),
br(),
# 4 INPUT iName ----
  tags$div(title = "Type a human readable name for the indicator. Preferably unique. For example: Tree cover Netherlands.",
         textInput("iName", 
                   "Indicator name", 
                   value = "ENTER INDICATOR NAME")),

# 4 INPUT iComment ----
tags$div(title = "Here you can for example ellaborate on possible uncertanties if the indicator should be included in the review or not.",
         textInput("iComment", 
                   "Comments on the indicator", 
                   value = "")),

  

  # 4 INPUT iRedundant ----
  tags$div(title = "Is the indicator described in another reference? For example, you are processing an indicator presented in an assessment of ecosystem condition, but it is also described in a seperate peer-reviewed paper cited in the assessment. Select 'Redundant' or 'Partly redundant' here to flag it as potentially redundant, and add the source reference that can be used to manually link these references later. Note that if you are processing the peer-reviewed paper, you will not be aware of this redundancy/duplicate issue and should not flag it as redundant.",  
           radioGroupButtons(
             inputId = "iRedundant",
             label = "Redundant?"
             ,
             choices = c("Redundant", "Possibly redundant", "Unique"),
             selected = "Unique"
           )),

  # 4 INPUT iRedundantRemarks ----
conditionalPanel(condition =  "input.iRedundant != 'Unique'",
  tags$div(title = "Use this field to elaborate if you chose 'Partly or 'Yes' above",
           textInput("iRedundantRemarks", 
                     "Comments on why it may be redundant", 
                     value = "")),
  
  # 4 INPUT iRedundantReferences ----
  tags$div(title = "Enter a reference, preferrabluy a doi, to the duplicate resource. For example, if the paper you are reading reports an indicator that it borrows from another reference that they cite. All duplicates, with the exeption of pre-prints when published versione exist, should be processed in the app in the normal way.",
           textInput("iRedundantReferences", 
                     "Source reference", 
                     value = ""))
  
  ),

 # 4 INPUT iContinent ----
tags$div(title = "Select the continent(s) where the indicator has been applied, eiether as a test or as part of an assessment.",
         pickerInput(
           inputId = "iContinent",
           label = "Continent(s)",
           multiple = T,
           choices = c("Africa",
                       "Antarctica", 
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

  # 4 INPUT iCountry ----
  tags$div(title = "Search and select the country(ies) where the indicator has been applied, eiether as a test or as part of an assessment. Only use this is its two or three countries. Don't list all the countries in EU for example. ",
     pickerInput('iCountry', 'Country',
          choices = ISO3166_v2,
          multiple = T,
          options = list(
            `live-search` = TRUE,
            `actions-box` = TRUE,
            `deselect-all-text` = "Deselect all",
            `multiple-separator` = " | "
            ))),

  # 4 INPUT iLowerGeography ----
  tags$div(title = "If relevant, type the name of any lower geography, i.e. the name of an area/place/region within a country, where the indicator has been applied.
           Example: Oslo",
         textInput("iLowerGeography", 
                   "Lower geography (if relevant)", 
                   value = "")),

  # 4 INPUT iLatitude ----
  #conditionalPanel("input.iLowerGeography != ''",
  #  tags$div(title = "Optional. Enter the Latitude of the Lower Geography in decimal degrees WGS84",
  #      numericInput("iLatitude", 
  #                "Latitude",
  #                value = NA,
  #                min = -90,
  #                max = 90,
  #                step = 0.1)),
  #  tags$div(title = "Optional. Enter the Longitude of the Lower Geography in decimal degrees WGS84",
  #      numericInput("iLongitude", 
  #                   "Longitude",
  #                   value = NA,
  #                   min = -90,
  #                   max = 90,
  #                   step = 0.1))
  #  ),

    # 4 INPUT iET ----
  tags$div(title = "Ecosystem type (multiple choice)",
         pickerInput('iET', 'Ecosystem type',
                     choices = ETs,
                     multiple = T,
                     options = list(
                       `multiple-separator` = " | "
                     ))),
actionButton("iETINFO", "",
             icon = icon("info")),
# 4 INPUT iETlink  ----

tags$div(title = "Conceptual connection between the variable and the ET",
         pickerInput('iETlink', 
                     'Connection to ecosystem type',
                     choices = ETlink,
                     options = list(
                       title = "Noting selected")
                     )),

actionButton("iETlinkINFO", "",
             icon = icon("info")),



tags$hr(),
h4("Fields related to the underlying dataset(s):", style="background-color:lightblue;"),

  # 4 INPUT dName ----
  tags$div(title = "The name of the underlying dataset(s) (comma seperated). If there are several underlying datasets, consider mentioning only the most essential one, the one that determines the main characteristics of the resulting indicator.
           Example: The Norwegian Forest Inventory, Living Planet Index",
         textInput("dName", 
                   "Dataset name(s)", 
                   value = "")),

  # 4 INPUT dReference ----
  tags$div(title = "If possible, enter a reference (e.g. url, doi) to the dataset(s) (comma seperated) mentioned above",
         textInput("dReference", 
                   "Dataset reference(s)", 
                   value = "")),

  # 4 INPUT dOrigin ----
  tags$div(title = "The origin of the underlying dataset. If the indicator requires modelling, this question asks about the data that goes into the model, not the model output. If the indicator is designed around several datasets, consider if one dataset is more important than the rest, and report only for that. Otherwise, yuo may also check multiple boxes here to account for multiple datasets with different origins.",
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
                     ))),

  # 4 INPUT dSpatialCoverage ----
  tags$div(title = "Saying something about the level of area representativeness in the underlying dataset(s).",
    pickerInput('dSpatialCoverage', 'Spatial Coverage of underlying dataset',
      choices = coverage,
      options = list(
        title = "Noting selected"))),

actionButton("dSpatialCoverageINFO", "",
             icon = icon("info")),

tags$hr(),
h4("Fields related to the indicator itself:", style="background-color:lightblue;"),


  # 4 INPUT iDescriptionSnippet ----
  tags$div(title = "A snippet of text (1-10 lines) COPIED DIRECTLY from the original publication, describing the indicator, its relevans and how it is produced or calculated.",
         textInput("iDescriptionSnippet", 
                   "Indicator description - snippet", 
                   value = "")),

  # 4 INPUT iDescription ----
  tags$div(title = "A relatively short description (can be a single sentence) of the indicator and what it represents. Usually it can be a shortened version of the iDescriptionSnippet",
         textInput("iDescription", 
                   "Indicator description", 
                   value = "")),


  # 4 INPUT iSpatialExtent ----
  tags$div(title = "What is the extent (size of the total area) for which the indicator has been calculated? Do not consider parts of the same dataset or indicator which is not reported in this exact publication. 
           Example: If you have an indicator on forest canopy structure which is reported with unique estimates at regional levels across Norway, and which is based on area representaive monitoring data, then the spatial extent is country",
         pickerInput('iSpatialExtent', 'Spatial extent',
                     choices = scale1,
                     options = list(
                       title = "Nothing selected"))),

actionButton("iSpatialextentINFO", "",
             icon = icon("info")),

  # 4 INPUT iSpatialResolution ----
  tags$div(title = "What is the finest spatial scale that this indicator has been calcuated at?",
         pickerInput('iSpatialResolution', 
                     'Spatial resolution or grain',
                     choices = scale1,
                     options = list(
                       title = "Nothing selected"))),

actionButton("iSpatialresolutionINFO", "",
             icon = icon("info")),

  # 4 INPUT iYear ----
  tags$div(title = "The latest year for which the indicator value has been caluculated and reported",
         numericInput("iYear", 
                      "Year",
                      value = NA,
                      min = 1900,
                      max = 2100,
                      step = 1,
                      width = '30%')),

  # 4 INPUT iTemporalCoverage ----
  tags$div(title = "The length of the time series at the time of publication (years). 
           
           \nDuration less than 1 year is reported as 1 year.
           
           \nIf unknown, enter 999.
           ",
    numericInput("iTemporalCoverage", 
       "Temporal coverage (length of time series)",
       value = NA,
       min = 1,
       step = 1,
       width = '30%')),

  # 4 INPUT iMap ----
  tags$div(title = "Is the indicator presented as a map? This map may be included in the printable publication or simply linked to or made available on a digital platform.",
    pickerInput(
     inputId = "iMap",
     label = "Presented as map?",
     choices = maps,
     options = list(
       title = "Nothing selected")
    )),


  # 4 INPUT iBiome ----
  tags$div(title = "IUCN Global Ecosystem Typology 2.0, level 2.",
         pickerInput('iBiome', 'Biome',
            choices = c(
              "T1 - Tropical-subtropical forests",
              "T2 - Temperate-boreal forests & woodlands",
              "T3 - Shrublands & shrubby woodlands",
              "T4 - Savannas and grasslands",
              "T5 - Deserts and semi-deserts",
              "T6 - Polar-alpine",
              "T7 - Intensive land-use systems",
              "UNK - Unknown or dont fit"),
            multiple = T,
            options = list(
              `multiple-separator` = " | "
            )
          )),


HTML("<p>Link to  <a href='https://global-ecosystems.org/explore/realms/T', target='_blank'> definitions</a>.</p>"),


  # 4 INPUT iSubIndex ----
  tags$div(title = "Is the indicator a composite indicator, like a sub-index, made op of several variables/criteria. For example, the red-list index is composed of data on multiple species. On the other hand, the Shannon index is not a composite indicator (but it is an index).",
         pickerInput('iSubIndex', 
                     'Composite indicator?',
                     choices = c("No", "Yes", "Unclear"),
                     options = list(
                       title = "Nothing selected"))),

actionButton("iSubIndexINFO", "",
             icon = icon("info")),

  # 4 INPUT iModelling ----
  tags$div(title = "Does the indicator (or the reference value) require modeling outside of what is included in the underlying dataset (i.e. lots of mathemtical steps)? This typically means the indicator is derived from raw data, but it is not itself the raw data.",
         pickerInput('iModelling', 
                     'Is the indicator a result from a model?',
                     choices = c("No", "Yes", "Unclear"),
                     options = list(
                       title = "Nothing selected"))),

  actionButton("iModellingINFO", "", icon = icon("info")),

  # 4 INPUT iOriginalUnits ----
  tags$div(title = "Original unit for the variable. e.g. meters, hectares, kilograms. For unitless indicators, write 'unitless'.",
         textInput("iOriginalUnits", 
                   "Original units", 
                   value = "")),


tags$hr(),
h4("Fields related to SEEA EA:", style="background-color:lightblue;"),


  # 4 INPUT iECTclass ----
  tags$div(title = "The class may not be reported, and in any case, it's is the reviewer that must assign the indicator to the correct or the most correct class.",
         pickerInput('iECTclass', 'SEEA Ecosystem Condition Typology Class',
                     choices = ECTs,
                     options = list(
                       title = "Nothing selected")
         )),

HTML("<p>Link to  <a href='https://oneecosystem.pensoft.net/article/58218/', target='_blank'> definitions</a>. (Scroll to Table 1).</p>"),

actionButton("iECTclassINFO", "", icon = icon("info")),

  # 4 INPUT iECTsnippet ----
  tags$div(title = "A short excerpt from the publication (1-10 sentences) that justifies the ECT assignment. It may be the same text as what you use in 'Indicator description - snippet', but without the same technical details. Here it is more about the ecological significans of the indicator",
         textInput("iECTsnippet", 
                   "ECT snippet", 
                   value = "")),



tags$hr(),
h4("Fields related to the reference condition:", style="background-color:lightblue;"),

  # 4 INPUT rType ----
  tags$div(title = "The list of options is non-exhaustive, but chose the one you think fits best. Otherwise select 'other'. For definitions, see SEEA EA white paper table 5.8, page 115.",
         pickerInput('rType', 
                     "Type of reference condition",
                     choices = refStates,
                     options = list(
                       title = "Nothing selected")
         )),
actionButton("rTypeINFO", "", icon = icon("info")),


  # 4 INPUT rTypeSnippet ----
  tags$div(title = "A short excerpt from the publication (1-10 sentences) that justifies the assignment of reference condition. The text must be directly copied, but may consist of sentences that are not next to each other in the original text. Note that we are NOT asking about REFERENCE VALUES here.",
         textInput("rTypeSnippet", 
                   "Reference condition - snippet", 
                   value = "")),

  # 4 INPUT rTypeRemarks ----
  tags$div(title = "An optional field where you can comment on the choice of reference condition 
           Example: Year 1850 was used to define the reference condition.",
         textInput("rTypeRemarks", 
                   "Comments on choice of reference condition", 
                   value = "")),

tags$hr(),
h4("Fields related to the reference values:", style="background-color:lightblue;"),

  # 4 INPUT rDeliberate ----
tags$div(title = "Was the choice of reference values the result of a deliberate process, or are they 'accidental'.",
         pickerInput("rDeliberate", 
                     "Deliberate and stated choice", 
                     choices = deliberate,
                     options = list(
                       title = "Nothing selected"))),
actionButton("rDeliberateINFO", "", icon = icon("info")),


# 4 INPUT rMethod ----
  tags$div(title = "See definitions in the SEEA EA white paper A5.5 - A5.11 (page 116).",
           pickerInput("rMethod", 
                     "What method(s) was used for estimating the reference levels?  (multiple choice)", 
                     choices = refValMethod,
                     multiple = TRUE)),
actionButton("rMethodINFO", "", icon = icon("info")),



  # 4 INPUT rRescalingMethod ----
  tags$div(title = "Pick the category that fits the best. If a two-sided rescaling has been done (i.e. both values that are higher and those that are lower than the reference value is scaled to become indicator values lower than the maximum possible value), this should always be chosen. If the variable is normalised between two extremes (a best and worst possible condition for example), this implies a linear rescaling method.",
         pickerInput('rRescalingMethod', 
                     "Rescaling method",
                     choices = rescalingMethod,
                     options = list(
                       title = "Nothing selected")
         )),

# 4 INPUT rResolution ----
tags$div(title = "The finest geographical resolution of the reference value(s). The scale for the reference value is often somewhere between that of iSpatialExtent and iSpatialResolution. 
        If the reference value is the same across the EAA, then rResolution equals iSpatialExtent.
         If the reference values are unique to each indicator value (i.e. unique reference value for each grid cell), then rResolution equals iSpatialResolution.",
         pickerInput('rResolution', 
                     "Spatial resolution of the reference value(s)",
                     choices = scale1,
                     options = list(
                       title = "Nothing selected")
         )),


  
  # 4 INPUT rMax ----
  tags$div(title = "A definition or description of the upper reference value, i.e. the maximum indicator value.
           
           \nExamples: 'A species composition similar to a reference community, or
           
           \nan air temperatur similar to the mean for the last climatic normal period.'",
           textInput("rMax", 
                     "Explanation of the upper reference value", 
                     value = "")),
  
  # 4 INPUT rMin ----
  tags$div(title = "A definition or description of the lower limit for the indicator (the porest condition).
  
           \nExample: 'Species extinct'",
           textInput("rMin", 
                     "Explanation of the lower limit value", 
                     value = "")),

  

  h4("Create file name", style="background-color:lightblue;"),
  # 4 INPUT replace_i ----
  tags$div(title = "Chose whether to create a new file name (and hence a new file) for the csv file that you are about to export, or to overwrite the uploaded file that you have edited.",
           radioGroupButtons(
             inputId = "replace_i",
             label = "Replace the uploaded file?",
             choices = c("Replace the uploaded file", 
                         "Create a new file")
           )
  ),
  
  h6("This is the new filename:"),
  textOutput('newFileName_i'),
  h6("Don't create a new file name if you have edited an existing file. The UUIDs will not have changed.",
     style="color:red;")





 ), # end side panel
  mainPanel(width = 6,
    
    conditionalPanel("input.iNew == 'Edit'",      
      # 4 OUTPUT: i_uploaded ----
      h5("Preview of the uploaded indicator profile (before any new edits):", 
         style="background-color:lightgreen;"),
      tableOutput("i_uploaded")     
    ),
    
    br(),        
            
    # 4 OUTPUT previewI ----
    h4("Indicator profile", style="background-color:lightgreen;"),
    h6("This is what you download when you press the button below this table"),
    DTOutput('previewI'),
    
    # 4 DOWNLOAD ----
    
    tags$hr(),  # Horizontal line
    h4("Download the indicator profile", style="background-color:lightgreen;"),
    
    h6("If you have a copy of the project github repo on you computer, 
         you probably want to save this under 'data/indicatorProfiles' 
         so that you can upload them later to the main branch via a pull request."),
    h6("If this is greek to you, you can save it to any local and dedicated folder
         on you computer and contact the project leader about how to get you csv file
         submitted to the main github branch"),
    tags$div(title = "Click to open a 'Save as' dialogue window.\n\nDO NOT change the file name.",
             downloadButton("downloadData_i", "Download"))
    
            )

 
 )
),


# '-------------             
# **TAB More ----
    navbarMenu("More",
               
# 5 Instructions----
      tabPanel("Instructions",
               
               p("This app was developed by Anders L. Kolstad with the purpuse of aiding and standardising the data entrering for a systematic review nicknamed ", tags$a(href="https://github.com/anders-kolstad/theIndiMap/", target='_blank', "The IndiMap"), "This is a crowd sourced review, meaning that in principle anyone can contribute. The app is (or will be made) available online.",style = "width: 500px;"),
               
               p("The data entry is hierarchical, with a one-to-many relationship between selected publications and the indicators that are reported inside these. The", 
                 tags$a(href="https://anders-kolstad.github.io/theIndiMap/", target='_blank', "review itself"), 
                 "will eventually explain in more detail how we selected the publications for the review, and what inclusion criteria we used, both for the publications and the indicators. Most importantly, all the indicators are rescaled (i.e. normalised according to a reference value) and developed for terrestrial ecosystems.",style = "width: 500px;"),
               
               p("Before you can start entering information about the indicators you first need to enter information about the publication. If you are starting from scratch, go to", tags$i("Register publication"), "and select", tags$i("Create new"), ". This will autogenerate a uniqe identifier. Continue filling out the form. You can alway see the updated preview of you data on the right. when done, select", tags$i("Create a new file"), "at the bottom, and then press the download botton in the lower right part of the screen, below the data preview.", tags$i("(Pro tip: you may want to read the publication before opening the app, and use your notes to quickly fill in the form all at once. This is because the app may shut down if you go for coffee!)."), "This file is a standalone cvs file, and we call this a publication profile. The file name is just a time stamp.",style = "width: 500px;"),
               
               p("If you want to edit the publication profile you just made, go back to the top and select", tags$i("Edit."), "Follow the instruction to locate the folder where you saved the csv. This should be the same folder where you keep all the publication profiles. If you have cloned the entire GitHub repo (which is the recomended workflow) then this folder should be /data/publicationProfiles. Now, because the file names are so cryptic, you need to initially select all of them and let the app extract the publication titles, which you then pick from in order to upload the correct profile. After you've selected a publication by its title, you can preview that datafile to the right, above the edited version. When you have the correct publication profile, you can populate the form using the switch. Control that the", tags$i("Publication profile"),  "on the right is updated correctly. Continue editing the file. When reaching the end, in most cases you want to overwrite the existing profile to avoid duplications. Therefore, this time select", tags$i("Replace the uploaded file."), "This will reuse the file name and overwrite the old version.",style = "width: 500px;"),
               
               p("When you have a poblication profile, you can go on to process the individual indicators that are described in it. Switch to the", tags$i("Register indicator"), "tab. Most of the workflow is similar as what you just did for the publication, except you also need to be careful to link the indicator to the correct publication.", style = "width: 500px;")
      ),
      

# 5 Contact ----

      tabPanel("Contact",
               
        p("For technical issues with the app itself, including suggestions for improvments, please look though", tags$a(href="https://github.com/anders-kolstad/theIndiMap/issues", "existing issues"), "in the GitHub repo if the issue is already raised, and if not, create a new issue there.",style = "width: 500px;"),
        
        p("For other questions or comments, either about the app or the systematic review, contact Anders L. Kolstad: anders.kolstad@nina.no",
          style = "width: 500px;")
               
               
               )

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
  # If we had a locally deployed app and used shinyChooseDir()
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
  # Now a duplicate of the same reactive element for use in in pID
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
      temp[nrow(temp)+1,] <- c("filename_local", input$localPub2$name[input$localPub2$datapath == x])
      temp$ID <- temp$value[temp$parameter=="pID"]
      temp
    })
    
    
    # transpose from long to wide format
    data.table::dcast(
      data.table::setDT(combined),
      formula = ID~parameter)
    
  })
  
  # A* REACT: indicatorList ----------------
  # Compile local indicator profiles 
  indicatorList <- reactive({
    req(input$localInd)
    # Read the csv's, add path and ID, and finally rbind them
    combined <- plyr::ldply(input$localInd[,"datapath"], function(x) {
      temp <- read.csv(x,
                       header = T,    #   input$i_header,
                       sep    =  ",", #   input$i_sep,
                       quote  =  '"'  #   input$i_quote
      )
      temp[nrow(temp)+1,] <- c("filename", x)
      temp[nrow(temp)+1,] <- c("filename_local", input$localInd$name[input$localInd$datapath == x])
      temp$ID <- temp$value[temp$parameter=="iID"]
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
 
  # A* REACT: title to path (indicator) ----
  # Take the chosen indicator name from indDrop
  # and link it to the corresponding local file path
  titleToPath_i <- reactive({
    p <- indicatorList()
    p <- p$filename[p$iName == input$indDrop]
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
  
  # A* REACT: title to filename (indicator) ----
  # Take the chosen indicator name from indDrop
  # and link it to the corresponding local file path
  titleToFilename_i <- reactive({
    p <- indicatorList()
    p <- p$filename_local[p$iName == input$indDrop]
    p
  })
  
  
  #*********************************************************************************  
  
  # A* UPDATE pubDrop ----
  # reactive list of publications titles
  observeEvent(input$localPub, {
    updatePickerInput(session = session, inputId = "pubDrop",
                      choices = sort(publicationList()$pTitle))
  })
  
  # A* UPDATE indDrop ----
  # reactive list of publications titles
  observeEvent(input$localInd, {
    updatePickerInput(session = session, inputId = "indDrop",
                      choices = unique(indicatorList()$iName))
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
  
  # A* REACT: uploadedInd ----
  # Make the uploaded indicator profile file available through an reactive element 
  
  uploadedInd <- reactive({
    read.csv(
      titleToPath_i(),
      header = input$i_header,
      sep = input$i_sep,
      quote = input$i_quote)
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
  
  
  # A* OUTPUT i_uploaded ----
  # Preview the chosen Indicator Profile to make sure it's imported correctly
  output$i_uploaded <- renderTable({
    
    # input$indDrop will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$indDrop)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- uploadedInd()
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$i_disp == "head") {
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
  
  # A* REACT iForm ----
  iForm <- reactive({
    ifelse(input$i_populate == FALSE, 
           return(indicatorParameters),
           return(uploadedInd()))
  })
  # '-------------       
 
  
  
  # B* UPDATEs: ----
  
  ## pRayyanID ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'pRayyanID',
                    value = pform()$value[pform()$parameter == "pRayyanID"])
  })
  
  ## pOrigin ----
  observeEvent(input$populate, {
    updatePickerInput(session = session,
                            'pOrigin',
                            choices = origin,
                            selected = pform()$value[pform()$parameter == "pOrigin"])
    
  })
  
  
  pUUID <- eventReactive(input$puuid_new, {
      uuid::UUIDgenerate()
  }, ignoreNULL=FALSE)
  
  ## iID ----
  iUUID <- eventReactive(input$iuuid_new, {
          uuid::UUIDgenerate()
      }, ignoreNULL=FALSE)
  
  
  
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
  
  
  
  ## pRedundant ----
  observeEvent(input$populate, {
    updateRadioGroupButtons(session = session,
                    'pRedundant',
                    choices = c("Redundant", "Possibly redundant", "Unique"),
                    selected = pform()$value[pform()$parameter == "pRedundant"])
    
  })
  
  ## pType ----
  observeEvent(input$populate, {
    updatePickerInput(session = session,
                            'pType',
                            choices = publicationTypes,
                            selected = pform()$value[pform()$parameter == "pType"])
    
  })
  
  ## pJournal ----
  observeEvent(input$populate, {
    updatePickerInput(session = session,
                    'pJournal',
                    choices = sort(journals),
                    selected = pform()$value[pform()$parameter == "pJournal"])
    
  })
  
  ## pComment ----
  observeEvent(input$populate, {
    updateTextInput(session = session,
                    'pComment',
                    value = pform()$value[pform()$parameter == "pComment"])
    
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
    updatePickerInput(session = session,
                            'pEAAextent',
                            choices = scale1,
                            selected = pform()$value[pform()$parameter == "pEAAextent"])
  })
  
  observeEvent(input$pEAAextantINFO, {
    shinyalert::shinyalert(title="The spatial extent of the ecosystem assessment/accounting area(s)",
                           text=
                             "Regional scale: A regional scale implies the area consists of several management units (i.e. multiple governance levels). For example: Southern Norway, Agder Fylke, New York State, Australian National Parks.
           
           Local scale (= sub-regional scale): A local scale contains a singel management unit, where decitions about land use can be made and directly put to action. For example: Trondheim kommune (a municipality), New York City, a national park.
           
           Project scale: a scale lower than the typical administrative unit. Typically a more transient project area or a single property. If in doubt whether to use local or sub-local, think about whether this scale is likely to have it own full-time government, in which case it should be assigned local scale. If the extent is so little that the area could not be considered self-contained, or that landscape effects are of little importance for ecosystem condition, or that remotely sensed data are not generally suitable for describing ecosystem condition, then all these things should point to this being a case of Project scale. Finaly, generally chose local scale unless it is clearly sub-local. Examples: a cattle farm, a housing developement area, a small nature reserve.",
                           size="l",
                           type="info"
    )
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
  
  observeEvent(input$pAggregationINFO, {
    shinyalert::shinyalert(title="Level of indicator aggregation",
                           text="
      The highest level of spatial aggregation of the condition estimate(s) reported in the publication. Only relevant for normalised indicator sets.
                    Examples: 
                    0 = indicators reported seperately with no aggregation
                    1 = A species group or some level that does not span several SEEA ECT classes. 
                    2 = Basic Spatial Unit. The finest spatial scale where data is available. E.g. a grid cell or a municipality
                    3 = Ecosystem Asset. The finest spatial scale where indicators can be aggregated. E..g a county. If EA = BSU, then pick BSU
                    4 = Ecosystem type. Chose this is indicators are aggregtaed to produce one condition value for an entire ecosystem type with the EAA 
                    5 = Ecosystem Accounting Area. Chose this option if the publication has aggregated condition estimates accross ETs"
                           ,
                           size="l",
                           type="info",
                           inputType = "text"
                           
    )
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
  
  ## pID ----
  # update the drop down list based on what you selected in 'localPub2', i.e. the csv's you uploaded
  observeEvent(input$localPub2, {
    updatePickerInput(session = session, inputId = "pubDrop2",
                      choices = sort(publicationList2()$pTitle))
  })
  
  # update the same drop down list with the publication name in the uploaded version
  observeEvent(input$i_populate, {
    updatePickerInput(session = session, inputId = "pubDrop2",
                      choices = iForm()$value[iForm()$parameter == "pID"])
  })
 
  
   ## iName ----
  observeEvent(input$i_populate, {
   updateTextInput(session = session,
                   'iName',
                   value = iForm()$value[iForm()$parameter == "iName"])
  })
 
  ## iComment ----
  observeEvent(input$i_populate, {
    updateTextInput(session = session,
                    'iComment',
                    value = iForm()$value[iForm()$parameter == "iComment"])
  })
  
## githubUser2 ----
 observeEvent(input$i_populate, {
   updateTextInput(session = session,
                   'githubuser2',
                   value = iForm()$value[iForm()$parameter == "githubUser"])
 })
 
##  iRedundant ----
observeEvent(input$i_populate, {
  updateRadioGroupButtons(session = session,
                  'iRedundant',
                  choices = c("Redundant", "Possibly redundant", "Unique"),
                  selected = iForm()$value[iForm()$parameter == "iRedundant"]
                  )
})
 
 ## iRedundantRemarks ----
 observeEvent(input$i_populate, {
   updateTextInput(session = session,
                   'iRedundantRemarks',
                   value = iForm()$value[iForm()$parameter == "iRedundantRemarks"])
 })
  
  
  ## iRedundantReferences ----
  observeEvent(input$i_populate, {
    updateTextInput(session = session,
                    'iRedundantReferences',
                    value = iForm()$value[iForm()$parameter == "iRedundantReferences"])
  })
 
 ## iContinent ----
 observeEvent(input$i_populate, {
   updatePickerInput(session = session,
                              'iContinent',
                              choices = c("Africa",
                                          "Antarctica", 
                                          "Asia",
                                          "Australia",
                                          "Europe",
                                          "North America",
                                          "South America"
                              ),
                              selected = stringr::str_split(
                                iForm()$value[iForm()$parameter == "iContinent"],
                                " \\| ", simplify = T))
 })
 
 ## iCountry ----
 observeEvent(input$i_populate, {
   updatePickerInput(session = session,
                              'iCountry',
                              choices = ISO3166_v2,
                              selected = stringr::str_split(
                                iForm()$value[iForm()$parameter == "iCountry"],
                                " \\| ", simplify = T))
 })
 
 ## iLowerGeography ----
 observeEvent(input$i_populate, {
   updateTextInput(session = session,
                   'iLowerGeography',
                   value = iForm()$value[iForm()$parameter == "iLowerGeography"])
 })
  
 # ## iLatitude ----
 # observeEvent(input$i_populate, {
 #   updateNumericInput(session = session,
 #                      'iLatitude',
 #                      value = iForm()$value[iForm()$parameter == "iLatitude"])
 # })
 # 
 # ## iLongitude ----
 #   observeEvent(input$i_populate, {
 #     updateNumericInput(session = session,
 #                        'iLongitude',
 #                        value = iForm()$value[iForm()$parameter == "iLongitude"])
 #   })
  
  ## iET ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'iET',
                      choices = ETs,
                      selected = stringr::str_split(
                        iForm()$value[iForm()$parameter == "iET"],
                        " \\| ", simplify = T))
  })
  
  observeEvent(input$iETINFO, {
    shinyalert::shinyalert(title="Ecosystem type",
                           text="
                           Taken from Eurostats level one types (terrestrial types only).
                           Select one or more ecosystem types which is fully or partialy covered by the indicator.
                           Alpine ecosystems are not treated uniquely, and indicators that refer to this whole ecosystem could be a combination of for example type 3, 5, 6 and 7.
                           ",
                           size="m",
                           type="info"
                           
    )
  })
  
  ## iETlink ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'iETlink',
                      choices = ETlink,
                      selected = iForm()$value[iForm()$parameter == "iETlink"],
                      options = list(
                        title = "Nothing selected"))
  })
  
  observeEvent(input$iETlinkINFO, {
    shinyalert::shinyalert(title="Conceptual connection between the variable and the ET",
                           text="
There are different ways in which a variable can be connected to an ET, and the type of this connection has a great practical significance: it greatly influences the spatial scalability of the variable. Please classify the variable into one of the following types:

cc: variables with a Conseptual Connection to the target ET: (no matter where and how you measure them, they will give information about the target ET, e.g. farmland bird index)

fo: variables linked to an ET with Field Observations: (typical for ecological variables that are based on field observations

so: variables linked to ETs via Spatial Overlay with an ET map (typical for variables based on remote sensing or modelling, e.g. NDVI and modeled climate indicators)

dm: variables that are themsleves Derived from an ET map: (e.g. connectivity and fragmentation indices)

na (not applicable): for an aggregated index where the different components (sub-indices) have different types of connection.
                           ",
                           size="l",
                           type="info"
                           
    )
  })
  
  
  ## dName ----
  observeEvent(input$i_populate, {
    updateTextInput(session = session,
                    'dName',
                    value = iForm()$value[iForm()$parameter == "dName"])
  })
  
  ## dReference ----
  observeEvent(input$i_populate, {
    updateTextInput(session = session,
                    'dReference',
                    value = iForm()$value[iForm()$parameter == "dReference"])
  })
  
  ## dOrigin ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'dOrigin',
                      choices = c("RS - remotely sensed",
                                  "MP - established monitoring program",
                                  "FS - field sampling",
                                  "CS - crowd sourced",
                                  "EO - expert opinion",
                                  "OT - others or unsure"),
                      selected = stringr::str_split(
                        iForm()$value[iForm()$parameter == "dOrigin"],
                        " \\| ", simplify = T))
  })
  ## dSpatialCoverage ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'dSpatialCoverage',
                      choices = coverage,
                      selected = iForm()$value[iForm()$parameter == "dSpatialCoverage"])
  })
  
  observeEvent(input$dSpatialCoverageINFO, {
    shinyalert::shinyalert(title="Spatial coverage",
                           text=
                             "If <strong> Complete </strong>, then the value assigned to an area is thought to be representative for that entire area. Most remotely sensed data falls in this category, but very little else. 
</br>
If <strong>Area representative</strong>, the data sampling is not complete, but is arranged in such a way that they can be aggregated and become an inbiased representation of a larger area. Both random and systematic sampling designs may be eligable here. For example: climate data interpolated from meterological stations, or data from national nature monitoring programs like national forest inventories.
</br>
If <strong>Oppurtunistic or sporadic</strong>, the dataset lacks a coordinated samling design, adding an unknown level of bias. For example, citizen science or crouwd sourced data, or a smaller dataset from a field study.  
</br>
If the dataset is a combination of datasets, with a combination of categories, use the least good one (highest number)",
                           size="l",
                           type="info",
                           html=T
    )
  })
    
  ## iDescriptionSnippet ----
  observeEvent(input$i_populate, {
    updateTextInput(session = session,
                    'iDescriptionSnippet',
                    value = iForm()$value[iForm()$parameter == "iDescriptionSnippet"])
  })
  
  ## iDescription ----
  observeEvent(input$i_populate, {
    updateTextInput(session = session,
                    'iDescription',
                    value = iForm()$value[iForm()$parameter == "iDescription"])
  })
  ## iSpatialExtent ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'iSpatialextent',
                      choices = scale1,
                      selected = iForm()$value[iForm()$parameter == "iSpatialExtent"])
    
    observeEvent(input$iSpatialextentINFO, {
      shinyalert::shinyalert(title="Spatial extent",
                             text="Example: 
                             
If you have an indicator on forest canopy structure which is reported with unique estimates at regional levels across Norway, and which is based on area representaive monitoring data, then the spatial extent is country.
        
Regional scale: A regional scale implies the area consists of several management units (i.e. multiple governance levels). For example: Southern Norway, Agder Fylke, New York State, Australian National Parks.
           
Local scale (= sub-regional scale): A local scale contains a singel management unit, where decitions about land use can be made and directly put to action. For example: Trondheim kommune (a municipality), New York City, a national park.
           
Project scale: a scale lower than the typical administrative unit. Typically a more transient project area or a single property. If in doubt whether to use local or sub-local, think about whether this scale is likely to have it own full-time government, in which case it should be assigned local scale. If the extent is so little that the area could not be considered self-contained, or that landscape effects are of little importance for ecosystem condition, or that remotely sensed data are not generally suitable for describing ecosystem condition, then all these things should point to this being a case of Project scale. Finaly, generally chose local scale unless it is clearly sub-local. Examples: a cattle farm, a housing developement area, a small nature reserve. 
        ",
                             size="l",
                             type="info"
      )
    })
  })
  
  ## iSpatialResolution ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'iSpatialResolution',
                      choices = scale1,
                      selected = iForm()$value[iForm()$parameter == "iSpatialResolution"])
  })
  
  observeEvent(input$iSpatialresolutionINFO, {
    shinyalert::shinyalert(title="Spatial resolution",
                           text=
                             
                             "If the indicator is used for saying something about a single ecosystem, but for an entire country, the iSpatialResolution is 'country'. 
       
Regional scale: A regional scale implies the area consists of several management units (i.e. multiple governance levels). For example: Southern Norway, Agder Fylke, New York State, Australian National Parks.
           
Local scale (= sub-regional scale): A local scale contains a singel management unit, where decitions about land use can be made and directly put to action. For example: Trondheim kommune (a municipality), New York City, a national park.
           
Project scale: a scale lower than the typical administrative unit. Typically a more transient project area or a single property. This scale is also suitable for raster data with grid cells < 0.5x0.5 km (approx.).  If in doubt whether to use local or sub-local, think about whether this scale is likely to have it own full-time government, in which case it should be assigned local scale. If the extent is so little that the area could not be considered self-contained, or that landscape effects are of little importance for ecosystem condition, or that remotely sensed data are not generally suitable for describing ecosystem condition, then all these things should point to this being a case of Project scale. Finaly, generally chose local scale unless it is clearly sub-local. Examples: a cattle farm, a housing developement area, a small nature reserve. 
        ",
                           size="l",
                           type="info"
                           
    )
  })
  
  
  ## iTemporalCoverage ----
  observeEvent(input$i_populate, {
    updateNumericInput(session = session,
                      'iTemporalCoverage',
                      value = iForm()$value[iForm()$parameter == "iTemporalCoverage"])
  })
  ## iMap ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                       'iMap',
                       choices = maps,
                       selected = iForm()$value[iForm()$parameter == "iMap"])
  })
  ## iYear ----
  
  observeEvent(input$i_populate, {
    updateNumericInput(session = session,
                            'iYear',
                            value = iForm()$value[iForm()$parameter == "iYear"])
  })
  ## iBiome ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'iBiome',
                      choices = c(
                        "T1 - Tropical-subtropical forests",
                        "T2 - Temperate-boreal forests & woodlands",
                        "T3 - Shrublands & shrubby woodlands",
                        "T4 - Savannas and grasslands",
                        "T5 - Deserts and semi-deserts",
                        "T6 - Polar-alpine",
                        "T7 - Intensive land-use systems",
                        "UNK - Unknown or dont fit"),
                      selected = stringr::str_split(
                        iForm()$value[iForm()$parameter == "iBiome"],
                        " \\| ", simplify = T))
  })
  
  ## iSubIndex ----
    
    observeEvent(input$i_populate, {
      updatePickerInput(session = session,
                              'iSubIndex',
                              choices = c("No", "Yes", "Unclear"),
                              selected = iForm()$value[iForm()$parameter == "iSubIndex"])    
      })
  observeEvent(input$iSubIndexINFO, {
    shinyalert::shinyalert(title = "Sub Index", 
                           text=
                             "A sub index is defined here as being some form of aggregated metric across potentially uniqe indicators that either 
 1) span several ECTclass (i.e. the sub index cannot be attributed to an ECT class because it an aggregation across several ECT classes) 
 2) contain indicators from the same ECT class but where each indicator has a unique reference level.

\nAn exampel of the first can be a hypothetical forest integrity metric comprised of indicators on both biodiversity and forest structure components. 
And example of the second type can be the Norwegian Natur Index which is comprised of several indicators where most or all of the indicators corresponds to a species and its population trends. These indicators can be said to belong to the same ECT class B1, but each indicator has a unique, species specific reference value.
\nContrastingly, the red-list index also is comprised of data on several species, but they all have the same reference value (i.e. not to be on the red list) and so this is not a sub index.",
                           size="l",
                           type="info"
    )
  })  
  
  ## iModelling ----
    observeEvent(input$i_populate, {
      updatePickerInput(session = session,
                              'iModelling',
                              choices = c("No", "Yes", "Unclear"),
                              selected = iForm()$value[iForm()$parameter == "iModelling"])    
    })
  
  
  observeEvent(input$iModellingINFO, {
    shinyalert::shinyalert(title="Modelling",
                           text=
                             "
       Any indicator that relies on other, predictory variables, or which requires interpolation or extrapolation, is modelled. This includes most climate datasets which tend to be interpolated, but if this interpolated dataset is used as the raw data for the indicator, and reported 'as is', then you should still select 'No' here. If the indicator instead used this meterological or climate data to estimate/model drought risk (extrapolation) or something similar, then you should select 'Yes'.",
                           size="l",
                           type="info"
    )
  })
  
  ## iOriginalUnits  ----
      
    observeEvent(input$i_populate, {
      updateTextInput(session = session,
                      'iOriginalUnits',
                      value = iForm()$value[iForm()$parameter == "iOriginalUnits"])
    })
  ## iECTclass ----
    
    observeEvent(input$i_populate, {
      updatePickerInput(session = session,
                        'iECTclass',
                        choices = ECTs,
                        selected = iForm()$value[iForm()$parameter == "iECTclass"])
      })
    
  observeEvent(input$iECTclassINFO, {
    shinyalert::shinyalert(title="ECT classes",
                           text="based on the SEEA EA Ecosystem Condition Typology (ECT)

The primary Ecosystem Condition Typology (ECT) for the variable, defined as the ECT class connected to the main “intended purpose” of the variable, corresponding to the underlying ecosystem characteristic (Czucz et al. 2021).

A1: Physical state characteristics: physical descriptors of the abiotic components of the ecosystem(e.g. soil structure, water availability)
A2: Chemical state characteristics: chemical composition of abiotic ecosystem compartments (e.g. soil nutrient levels, water quality, air pollutant concentrations)
B1a: Compositional state characteristics - addressing the abundance of key species & species groups at a given location and time
B1b: Compositional state characteristics - addressing the diversity of relevant species groups
B2: Structural state characteristics: aggregate properties (e.g. mass, density) of the whole ecosystem or its main biotic components (e.g. total biomass, canopy coverage, chlorophyll content, annual maximum NDVI)
B3: Functional state characteristics: summary statistics (e.g. frequency, intensity) of the biological, chemical, and physical interactions between the main ecosystem compartments (e.g. primary productivity, community age, disturbance frequency)
C1: Landscape characteristics: metrics describing mosaics of ecosystem types at coarse (landscape, seascape) spatial scales (e.g. landscape diversity, connectivity, fragmentation) or the density of fragments of other ETs in an embedding ET (e.g. hedgerows in agricultural land)
Other: pre-aggregated indices (e.g. ecosystem integrity, naturalness); accessibility (distance to population centres, length of trails); protected areas; raw pressures (e.g. #pollutant loads, habitat loss); management intensity (e.g. grazing); abiotic / climatic characteristics (e.g. annual rainfall); certificates (e.g. blue flag (EU beaches))",
                           
                           size="l",
                           type="info",
                           closeOnClickOutside = TRUE
                           
    )})
  
  observeEvent(input$rTypeINFO, {
    shinyalert::shinyalert(title="Types of reference condition",
                           text="Taken from SEEA EA white paper table 5.8
                           
                           Undisturbed or minimally-disturbed condition of an intact ecosystem. The condition of an ecosystem with maximal ecosystem integrity with no or minimal disturbance. 
                           
Historical condition: The condition of an ecosystem at some point or period in its history that is considered to represent the stable socio ecological state (e.g., the pre-industrial period or pre-intensive agriculture) 

Least-disturbed condition: the currently best available condition of an ecosystem 

Contemporary condition: The condition of an ecosystem at a certain point or period in its recent history for which comparable data are available. 

                           Best-attainable condition: the expected condition of an ecosystem under best possible management practices and attaining a stable socio-ecological state.",
                           size="l",
                           type="info",
                           closeOnClickOutside = TRUE
                           
    )})
  
  observeEvent(input$rMethodINFO, {
    shinyalert::shinyalert(title="Method for defining or setting reference values",
                           text="Taken from SEEA EA white paper section A5.4 - A5.12.
                           1. Reference sites: If pristine or minimally-disturbed sites are available, they can be used to determine a reliable measure of the mean and statistical distribution of condition variables. Reference sites can be identified using expert or traditional knowledge but also by using statistics and artificial intelligence if long-term time series with data describing ecosystem disturbance are available. Monitoring reference sites is probably the most straightforward method for establishing reference conditions and for determining the reference levels of condition variables. Seasonal or annual variability but also long term or irreversible ecosystem changes due to climate change or invasive alien species can be factored in when determining reference levels for ecosystem condition variables. Reference sites can thus by used to determine a dynamic reference condition (Hiers et al., 2012) that can be periodically updated. 

                          2. Modelled reference conditions can be based on predictive empirical models or potential vegetation models. Models can be used to infer conditions in absence of human disturbance where representative reference sites are not available. Potential vegetation can be modelled globally and can incorporate scenarios of environmental change. A weakness is that models usually do not involve all the selected condition variables of the condition account, and often differ from measured variables. Models require assumptions to establish reference levels for condition variables, e.g., scientific debate on the role of megafauna and early humans on potential natural vegetation. 

                          3. Statistical approaches based on ambient distributions. Least-disturbed conditions or best attainable conditions can be estimated by observing the range of values from current ecosystem monitoring and by selecting a reference condition, for instance based on the 5th percentile values as criterion or by assuming that the reference condition is equal to a state with the highest species richness. Statistical approaches are data-driven and therefore pragmatic, familiar for accountants, and applicable if no reference sites are available. Methods can be applied consistently across variables, e.g., normalizing with the maximum 117 values of available data. Possible drawbacks are the arbitrary nature of the reference condition, spatial inconsistencies caused by using current datasets, a strongly shifting baseline, or a false sense of consistency. Solutions need to be proposed to scale condition variables at levels outside the range of the available data. Variables moving out of their established range (e.g., improving beyond the previous upper reference level) can cause serious complications. 

                          4. Historical observations and paleo-environmental data. This method uses historical observations or paleontological data to describe a historical reference condition (typically before 1970 when routine environmental monitoring programmes started). Historical observations refer to a description of a reference condition based on species collections in natural history museums, historical manuscripts and books that describe fauna and flora, photo archives, paintings, or other material that can be used to make inferences about the presence of species or the prevalence of certain conditions during a certain period in time. Paleo-environmental data can be used to reconstruct the physical-chemical environment, climate, vegetation and fauna of certain period in time using material that is buried in the soil. These data are often collected during archaeological studies. Examples of relevant data collections to define a historical ecosystem condition include seedbanks to reconstruct flora or remains of fish catches nearby medieval settlements to reconstruct the fish fauna or determine the presence of specific species. This method can deliver a common baseline for climate and biodiversity science, which is relevant to support more integrated climate biodiversity policies. This method can also show the magnitude of loss of biodiversity. A weakness is that not all ecosystem condition variables can be easily inferred from historical data. 

                          5. Contemporary data. This method uses contemporary data to describe a contemporary reference condition (typically after 1970 when routine environmental monitoring programmes started). For instance, the Kyoto protocol used the global atmospheric CO2 emissions recorded in 1990 as a reference against which the changes in future greenhouse gas emissions were assessed. The Living Planet Index uses species data collected in 1970 as a reference to assess changes. Similar to statistical approaches that use ambient data distributions, this is a straightforward approach to set a reference condition provided data are available. However, there are several disadvantages. The choice of year may be considered arbitrary. The reliance on contemporary data in evaluating changes can result in a shifting baseline. Appropriate dates differ for different indicators and ecosystem types. If different baseline dates are used in different regions this creates inconsistencies. Difficulties arise for scaling condition variables at levels which are higher than their reference level, e.g., when variables move out of their established range. The method is subject policy influence and contemporary baselines may diverge greatly from pre-industrial era baselines. 

                          6. Prescribed levels of a set of ecosystem condition variables can be used to construct a bottom-up reference condition. Examples of these reference levels include zero values for emissions or pollutants, a specific number of species, established sustainability or threshold levels such as critical loads for eutrophication and acidification, and target levels in terms of legislated quality measures (air and water quality). Prescribed levels of variables can have clear and straightforward management applications and provides a basis for direct policy response. This method can reflect preferences for a particular use of an ecosystem accounting for social, economic and environmental considerations. They can also describe a level quantifying an undesirable state required to define the zero end of the normalized scale, for example, where the ecosystem is no longer present or functioning. Prescribed levels are, however, not available for all variables, may be subject to policy influence and changing over time, and may not be consistently developed for all ecosystem types, variables or countries. 

                          7. Expert opinion usually consists of a narrative statement of expected reference condition. Although an expert´s opinion may be expressed semi-quantitatively, qualitative articulation is probably most common (European Commission, 2003). Several weaknesses are inherently associated with this approach. Therefore, caution should be exercised when using this approach as the sole means of establishing reference condition. 

                          8. Natural scale limits, such as absolute biophysical limits. For example, the amount of mire trenching under the reference condition is 0%, or the lower (worst case) limit for the abundance of alien species is 100%.


Combination of any of the above methods? Many of the above approaches may be used either singly or in concert for establishing and/or cross-validating reference condition. In practice, it may not be possible to use a single method to describe or quantify reference levels of ecosystem condition variables under a reference condition. For instance, the reference values of variables that describe a historical condition (for instance a pre-industrial state of an ecosystem) can be determined by combining modelling potential vegetation (method 2) based on paleo-climatic data (obtained through method 4). Statistical models and tools exist to combine methods (e.g., Bayesian networks can combine statistical distributions (method 3) and expert opinion (method 7)). Recent advancements in artificial intelligence will further improve the above mentioned methods to infer and describe a reference condition.

                           ",
                           size="l",
                           type="info",
                           closeOnClickOutside = TRUE
                           
    )})
  
  observeEvent(input$rDeliberateINFO, {
    shinyalert::shinyalert(title="What do we mean, deliberate?",
                           text="
                           Some eliable variables may be naturally bound on a range where there is a clear normative interpretation and direction 
                           (e.g. that 0 or 0% is bad and 1 or 100% is good),
                           yet these reference values may not be actively chosen. 
                           For example, a variable called Percentage of forest covered by Natura 2000 (%)
                           is naturally bound between 0 and 100% and it has a clear normative directionality. 
                           But this is just an artefact, or accident, of how the variable is calculated. 
                           The effect being that the reference values may be unrealistic and the indicator values 
                           are not easiliy comparrable to other indicators.",
                           size="l",
                           type="info",
                           closeOnClickOutside = TRUE
                           
    )})
  ## iECTsnippet ----
    observeEvent(input$i_populate, {
      updateTextInput(session = session,
                      'iECTsnippet',
                      value = iForm()$value[iForm()$parameter == "iECTsnippet"])
    })
  ## rType ----
   
    observeEvent(input$i_populate, {
      updatePickerInput(session = session,
                        'rType',
                        choices = refStates,
                        selected = iForm()$value[iForm()$parameter == "rType"])
    })
  ## rTypeSnippet ----
    observeEvent(input$i_populate, {
      updateTextInput(session = session,
                      'rTypeSnippet',
                      value = iForm()$value[iForm()$parameter == "rTypeSnippet"])
    })
  
  ## rTypeRemarks ----
    observeEvent(input$i_populate, {
      updateTextInput(session = session,
                      'rTypeRemarks',
                      value = iForm()$value[iForm()$parameter == "rTypeRemarks"])
    })
  ## rResolution ----
    
    observeEvent(input$i_populate, {
      updatePickerInput(session = session,
                        'rResolution',
                        choices = scale1,
                        selected = iForm()$value[iForm()$parameter == "rResolution"])
    })
  ## rRescalingMethod ----
      observeEvent(input$i_populate, {
        updatePickerInput(session = session,
                              'rRescalingMethod',
                              choices = rescalingMethod,
                              selected = iForm()$value[iForm()$parameter == "rRescalingMethod"])    
    })
  
  ## rMethod ----
  
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'rMethod',
                      choices = refValMethod,
                      selected = iForm()$value[iForm()$parameter == "rMethod"])
  })
  
  ## rDeliberate ----
  
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'rDeliberate',
                      choices = deliberate,
                      selected = iForm()$value[iForm()$parameter == "rDeliberate"])
  })
  
  ## rMax ----
    observeEvent(input$i_populate, {
      updateTextInput(session = session,
                      'rMax',
                      value = iForm()$value[iForm()$parameter == "rMax"])
    })
  ## rMin ----
    observeEvent(input$i_populate, {
      updateTextInput(session = session,
                      'rMin',
                      value = iForm()$value[iForm()$parameter == "rMin"])
    })
  
 
  # '-------------
  

  
# B* showNotification: validate_p ----
  
  
  
  observeEvent(input$validate_p, {
    showNotification(
      paste(
      if(nchar(pExport()$value[pExport()$parameter == "pTitle"]) < 3 | #) "MISSING: Publication title is not valid",
         nchar(pExport()$value[pExport()$parameter == "githubUser"]) < 3 | #"MISSING: Please enter GitHub user name",
         nchar(pExport()$value[pExport()$parameter == "pRayyanID"]) < 3) "MISSING: Either the Publication title, the GitHub user or the pRayyanID is missing!" else "Seems fine, but look through the preview for missing fields. Everything need to be filled out before you download the data."),
      type = "error",
      duration = 10
    )
  })
  
# B* OUTPUT: pExport   ----
  
# Compile CSVs for the publication profile
  
  
  pExport <- reactive({
    
    # shorten name
    dat <- pform()
    
    #update value column based on input
    dat$value[dat$parameter == "pRayyanID"] <- input$pRayyanID
    
    dat$value[dat$parameter == "pOrigin"] <- input$pOrigin
    
    if(input$pNew != "Edit") dat$value[dat$parameter == "pID"] <- pUUID() 
      
    dat$value[dat$parameter == "pTitle"] <- input$ptitle
    
    dat$value[dat$parameter == "pBibliography"] <- input$pbibliography

    dat$value[dat$parameter == "githubUser"] <- input$githubuser
    
    dat$value[dat$parameter == "pRedundant"] <- input$pRedundant

    dat$value[dat$parameter == "pType"] <- input$pType

    dat$value[dat$parameter == "pJournal"] <- ifelse(input$pType == "Peer-reviewed article", input$pJournal, NA)
    
    dat$value[dat$parameter == "pComment"] <- input$pComment
    
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
  
  
  
# B* OUTPUT: iExport   ----
  
  # Compile CSVs for the indicator profile
  
  
  iExport <- reactive({
    # shorten name
    dat <- iForm()
    
    if(input$iNew != "Edit") dat$value[dat$parameter == "iID"] <- iUUID() 
    dat$value[dat$parameter == "pTitle"] <- input$pubDrop2
    dat$value[dat$parameter == "iName"] <- input$iName
    dat$value[dat$parameter == "iComment"] <- input$iComment
    dat$value[dat$parameter == "githubUser"] <- input$githubuser2
    dat$value[dat$parameter == "iRedundant"] <- input$iRedundant
    dat$value[dat$parameter == "iRedundantRemarks"] <- ifelse(input$iRedundant == "Unique",
        NA,input$iRedundantRemarks)
    dat$value[dat$parameter == "iRedundantReferences"] <- ifelse(input$iRedundant == "Unique",
        NA,input$iRedundantReferences)
    dat$value[dat$parameter == "iContinent"] <- paste(input$iContinent, collapse = " | ")
    dat$value[dat$parameter == "iCountry"] <- paste(input$iCountry, collapse = " | ")
    dat$value[dat$parameter == "iLowerGeography"] <- input$iLowerGeography
    #dat$value[dat$parameter == "iLatitude"] <- input$iLatitude
    #dat$value[dat$parameter == "iLongitude"] <- input$iLongitude
    dat$value[dat$parameter == "iET"] <- paste(input$iET, collapse = " | ")
    dat$value[dat$parameter == "iETlink"] <- input$iETlink
    dat$value[dat$parameter == "dName"] <- input$dName
    dat$value[dat$parameter == "dReference"] <- input$dReference
    dat$value[dat$parameter == "dOrigin"] <- paste(input$dOrigin, collapse = " | ")
    dat$value[dat$parameter == "dSpatialCoverage"] <- input$dSpatialCoverage
    dat$value[dat$parameter == "iDescriptionSnippet"] <- input$iDescriptionSnippet
    dat$value[dat$parameter == "iDescription"] <- input$iDescription
    dat$value[dat$parameter == "iSpatialExtent"] <- input$iSpatialExtent
    dat$value[dat$parameter == "iSpatialResolution"] <- input$iSpatialResolution
    dat$value[dat$parameter == "iTemporalCoverage"] <- input$iTemporalCoverage
    dat$value[dat$parameter == "iMap"] <- input$iMap
    dat$value[dat$parameter == "iYear"] <- input$iYear
    dat$value[dat$parameter == "iBiome"] <- paste(input$iBiome, collapse = " | ")
    dat$value[dat$parameter == "iSubIndex"] <- input$iSubIndex
    dat$value[dat$parameter == "iModelling"] <- input$iModelling
    dat$value[dat$parameter == "iOriginalUnits"] <- input$iOriginalUnits
    dat$value[dat$parameter == "iECTclass"] <- input$iECTclass
    dat$value[dat$parameter == "iECTsnippet"] <- input$iECTsnippet
    dat$value[dat$parameter == "rType"] <- input$rType
    dat$value[dat$parameter == "rTypeSnippet"] <- input$rTypeSnippet
    dat$value[dat$parameter == "rTypeRemarks"] <- input$rTypeRemarks
    dat$value[dat$parameter == "rResolution"] <- input$rResolution
    dat$value[dat$parameter == "rRescalingMethod"] <- input$rRescalingMethod
    dat$value[dat$parameter == "rDeliberate"] <- input$rDeliberate
    dat$value[dat$parameter == "rMethod"] <- paste(input$rMethod, collapse = " | ")
    dat$value[dat$parameter == "rMax"] <- input$rMax
    dat$value[dat$parameter == "rMin"] <- input$rMin
    
    dat
    })

 

# B* OUTPUT previewP ----
output$previewP <- renderDT(
                      pExport(),
                      options = list(pageLength = 50, 
                                     scrollY = "600px",
                                     scollX = T))
  
# B* OUTPUT previewI ----
output$previewI <- renderDT(
  iExport(),
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
    
  # B* REACT fileName_i ----
  # Overwrite or create new csv?  
  fileName_i <- reactive({
    ifelse(input$replace_i == "Replace the uploaded file",
           ifelse(input$indDrop == "NA", "Invalid option", basename(titleToFilename_i())),
           paste0("iProfile_", 
                  gsub(":", "-", Sys.time()),
                  ".csv"))
  })
  
  
  output$newFileName_i <- renderText(
    fileName_i())
  
  
  # B* DOWNLOAD publication profile ----
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
  
  
  # B* DOWNLOAD indicator profile ----
  # download the new or edited profile as csv
  output$downloadData_i <- downloadHandler(
    filename = function() {
      fileName_i()
    },
    content = function(file) {
      write.csv(iExport(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # '-------------
  # ' ------------
  
  
  
  
  
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

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
indicatorParameters <- 
  read.csv("shinyData/indicatorProfileFields.csv", sep=";")
# Column1 (parameters) =  list of terms for the publication profiles
# Column2 (values) =  NA

# Duplicate data set for export 
#pExport <- publicationParameters

ISO3166 <- read.csv("shinyData/ISO3166.csv", sep=";")
ISO3166_v2 <- setNames(ISO3166$Alpha.2.code, ISO3166$English.short.name)


# Named lists
scale1 <- c(unknown = "0 - unknown",
            global  = "1 - global",
            continent = "2 - continent",
            country = "3 - country",
            region = "4 - region",
            local = "5 - local",
            'project area' = "6 - project-area")

refStates <- c("Undisturbed or minimally-disturbed condition" = "UND- Undisturbed or minimally-disturbed condition",
               "Historical condition" = "HIS - Historical condition",
               "Least-disturbed condition" = "LDI - Least-disturbed condition",
               "Contemporary condition" = "CON - Contemporary condition",
               "Best-attainable condition" =  "BAT - Best-attainable condition",
               "other" = "OTH - other")

rescalingMethod <- c(linear = "LIN - linear",
                     "non-linear" = "NLI - non-linear",
                     "two-sided" = "TSI - two-sided",
                     unclear = "UNC - unclear")

# UI ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤----------------------------------------------------------------------



ui <- 
  navbarPage(
    # add title and logos inside a div
    title = div(
      #"indiMAP",
      div(img(src='indimaplogo4.png',
              style="margin-top: -14px;
                               padding-right:15px;
                               padding-bottom:15px",
              height = 60)),        
                tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://www.nina.no/\"><img src=\"NINA_logo_sort_txt_engelsk_under.png\" alt=\"alt\" style=\"float:right;width:50px;padding-top:5px;padding-bottom:0px;\"> </a></div>');
header.append('<div style=\"float:right\"><a href=\"https://github.com/anders-kolstad/theIndiMap\"><img src=\"githublogo.png\" alt=\"alt\" style=\"float:right;width:50px;padding-top:5px;padding-bottom:0px;padding-right:15px;\"> </a></div>');
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
      
      # 3 INPUT pZoteroID ----
        tags$div(title = "Example: https://www.zotero.org/groups/4630169/the_rescalable_indicator_review/collections/KDCY6DCS/items/8GALH26U/collection",
        textInput("pzoteroid", 
                  "Enter the full URL for the Zotero entry", 
                  value = "")),
      
      
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
  tags$div(title = "The spatial extent of the ecosystem assessment/accounting area(s)
           
           Regional scale: A regional scale implies the area consists of several management units (i.e. multiple governance levels). For example: Southern Norway, Agder Fylke, New York State, Australian National Parks.
           
           Local scale (= sub-regional scale): A local scale contains a singel management unit, where decitions about land use can be made and directly put to action. For example: Trondheim kommune (a municipality), New York City, a national park.
           
           Project scale: a scale lower than the typical administrative unit. Typically a more transient project area or a single property. If in doubt whether to use local or sub-local, think about whether this scale is likely to have it own full-time government, in which case it should be assigned local scale. If the extent is so little that the area could not be considered self-contained, or that landscape effects are of little importance for ecosystem condition, or that remotely sensed data are not generally suitable for describing ecosystem condition, then all these things should point to this being a case of Project scale. Finaly, generally chose local scale unless it is clearly sub-local. Examples: a cattle farm, a housing developement area, a small nature reserve. 
           
           "
           
           ,
           radioGroupButtons(
             inputId = "pEAAextent",
             label = "The extent of the Ecosystem Accounting Area",
             choices = scale1,
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
    actionButton("validate_p", "Validate"),
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
                         `live-search` = TRUE))),

tags$hr(),
h4("General fields:", style="background-color:lightblue;"),

# 4 INPUT githubUser2 ----
tags$div(title = "Example: 'anders-kolstad'. \n\nNote: please update the contact info in you GitHub profile.", 
         textInput("githubuser2", 
                   "Enter your GitHub user name", 
                   value = "")),

# 4 INPUT iName ----
  tags$div(title = "Type a human readable name for the indicator. Preferably unique. For example: Tree cover Netherlands.",
         textInput("iName", 
                   "Indicator name", 
                   value = "")),

  
  

  # 4 INPUT iRedundant ----
  tags$div(title = "Is the indicator described in another reference? For example, an indicator can we presented both in a national assessment and in a stand-alone peer-reviewd paper. Selecting 'Yes' or 'Partly' here will work to flag it as potentially redundant. The following remarks field will help the analyst in making this call later. If the indicator is clearly the same as another indiator describes elsewhere (i.e. same data set, same method, same temporal scope, everything), then you may chose to only register the indiator once, using the most appropriate publication (ideally an assessment) as the source.",  
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
                     "Remarks to the above", 
                     value = "")),
  
  # 4 INPUT iRedundantReferences
  tags$div(title = "Enter a reference, preferrabluy a doi, to the duplicate resource. For example, if the paper you are reading reports an indicator that it borrows from another reference that they cite. All duplicates, with the exeption of pre-prints when published versione exist, should be processed in the app in the normal way.",
           textInput("iRedundantReferences", 
                     "Source reference", 
                     value = "")),
  
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

  # 4 INPUT iLowerGeography ----
  tags$div(title = "If relevant, type the name of any lower geography, i.e. the name of an area/place/region within a country, where the indicator has been applied.
           Example: Oslo",
         textInput("iLowerGeography", 
                   "Lower geography (if relevant)", 
                   value = "")),

  # 4 INPUT iLatitude ----
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



tags$hr(),
h4("Fields related to the underlying dataset(s):", style="background-color:lightblue;"),

  # 4 INPUT dName ----
  tags$div(title = "The name of the underlying dataset(s) (comma seperated). If there are several underlying datasets, consider mentioning only the most essential one, the one that determines the main characteristics of the resulting indicator.
           Example: The Norwegian Forest Inventory, Living Planet Index",
         textInput("dName", 
                   "Dataset name(s)", 
                   value = "")),

  # 4 INPUT dReference ----
  tags$div(title = "If possible, enter a reference (e.g. url, doi) to the dataset(s) (comma seperated) metioned above",
         textInput("dReference", 
                   "Dataset reference(s)", 
                   value = "")),

  # 4 INPUT dOrigin ----
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
                     ))),

  # 4 INPUT dSpatialCoverage ----
  tags$div(title = "Saying something about the level of area representativeness in the underlying dataset(s). If *Complete*, then the value assigned to an area is thought to be representative for that entire area. Most remotely sensed data falls in this category, but very little else. 
          If *Area representative*, the data sampling is not complete, but is arranged in such a way that they can be aggregated and become an inbiased representation of a larger area. Both random and systematic sampling designs may be eligable here. For example: climate data interpolated from meterological stations, or data from national nature monitoring programs like national forest inventories.
          If *Oppurtunistic or sporadic*, the dataset lacks a coordinated samling design, adding an unknown level of bias. For example, citizen science or crouwd sourced data, or a smaller dataset from a field study.  
           If the dataset is a combination of datasets, with a combination of categories, use the least good one (highest number)",
    pickerInput('dSpatialCoverage', 'Spatial Coverage of underlying dataset',
      choices = c("NA",
                  "1 - complete",
                  "2 - area representative",
                  "3 - oppurtunistic or sporadic",
                  "4 - unknown"))),


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
                     choices = scale1)),


  # 4 INPUT iSpatialResolution ----
  tags$div(title = "What is the finest spatial scale that this indicator has been calcuated at. Note that if the indicator is used for saying something about a single ecosystem, but for an entire country, the iSpatialExtent is still 'country'. If, on the other hand, there are unique indicator values tied to polygons or grid cells that are smaller than the average political administrative unit, then the resolution is 'sub-local'.",
         pickerInput('iSpatialResolution', 'Spatial resolution',
                     choices = scale1)),

  # 4 INPUT iTemporalCoverage ----
  tags$div(title = "The length of the time series at the time of publication (years). 
           
           Duration less than 1 year is reported as 1 year.
           
           If unknown, enter 999.
           ",
    numericInput("iTemporalCoverage", 
       "Temporal coverage (length of time series)",
       value = NA,
       min = 1,
       step = 1,
       width = '30%')),

  # 4 INPUT iMap ----
  tags$div(title = "Is the indicator presented as a map? This map may be included in the printable publication or simply linked to or made available on a digital platform.",
    radioGroupButtons(
     inputId = "iMap",
     label = "Presented as map?",
     choices = c("No", "Yes", "Not by itself, but as part of an aggregated index"),
     selected = "No"
    )),

  # 4 INPUT iYear ----
  tags$div(title = "The latest year for which the indicator value has been caluculated and reported",
         numericInput("iYear", 
                      "Year",
                      value = NA,
                      min = 1900,
                      max = 2100,
                      step = 1,
                      width = '30%')),

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

  # 4 INPUT iSubIndex ----
  tags$div(title = "Is the indicator a sub-index, made op of several variables/criteria. For example, the red-list index is composed of data on multiple species.",
         radioGroupButtons('iSubIndex', 'Is the indicator itself an index?',
          choices = c(
            "No", "Yes", "Unclear"))),

  # 4 INPUT iModelling ----
  tags$div(title = "Does the indicator (or the reference value) require modeling outside of what is included in the underlying dataset (i.e. lots of mathemtical steps)? This typically means the indicator is derived from raw data, but it is not itself the raw data.
           For example, any indicator that relies on other, preditcory variables, or which requires interpolation or extrapolation, is modelled. This includes most climate datasets which tend to be interpolated, but if this interpolated dataset is used as the raw data for the indicator, and reported 'as is', then you should still select 'No' here. If the indicator instead used this meterological or climate data to estimate/model drought risk (extrapolation) or something similar, then you should select 'Yes'.",
         radioGroupButtons('iModelling', 'Is the indicator a result from a model?',
                           choices = c(
                             "No", "Yes", "Unclear"))),

  # 4 INPUT iOriginalUnits ----
  tags$div(title = "Original unit for the variable. e.g. meters, hectares, kilograms",
         textInput("iOriginalUnits", 
                   "Original units", 
                   value = "")),


tags$hr(),
h4("Fields related to SEEA EA:", style="background-color:lightblue;"),


  # 4 INPUT iECTclass ----
  tags$div(title = "The class may not be reported, and in any case, it's is the reviewer that must assign the indicator to the correct or the most correct class.
           Examples:
A1: water quantity (e.g. hydrological flow, groundwater table)
A2: air quality (pollutants concentrations); water quality (e.g. pollutant concentrations); soil quality (e.g. soil carbon stock)
B1: birds, fish, habitats-based indices (red-list indices, LPI) 
B2: vegetation cover (e.g. shrub cover); timber stock; litter; forest age
B3: flood risk; NPP, biomass growth
C1: connectivity/fragmentation (e.g. barrier density); the presence/abundance of specific habitat (sub)types
Other: pre-aggregated indices (e.g. ecosystem integrity, naturalness); accessibility (distance to population centres, length of trails); protected areas; raw pressures (e.g. pollutant loads, habitat loss); management intensity (e.g. grazing); abiotic / climatic characteristics (e.g. annual rainfall); certificates (e.g. blue flag (EU beaches))",
         pickerInput('iECTclass', 'SEEA Ecosystem Condition Typology Class',
                     choices = c("NA",
                       "A1 Physical state characteristics",
                       "A2 Chemical state characteristics",
                       "B1 Compositional state characteristics",
                       "B2 Structural state characteristics",
                       "B3 Functional state characteristics",
                       "C1 Landscape and seascape characteristics",
                       "Other (e.g. pre-aggregated indices)")
         )),

  # 4 INPUT iECTsnippet ----
  tags$div(title = "A short excerpt from the publication (1-10 sentences) that justifies the ECT assignment. It may be the same text as what you use in 'Indicator description - snippet', but without the same technical details. Here it is more about the ecological significans of the indicator",
         textInput("iECTsnippet", 
                   "ECT snippet", 
                   value = "")),

#  # 4 INPUT iEScategory ----
#  tags$div(title = "What ecosystem service category(ies) is this indicator related to, if any? The reviewer must assign the #category regardless of what the publication itself claims. Weak or speculative links should not be reported, but empirical #relationships between the indicator and the ES category is not required. Biodiversity related indicators may fit into one or #several of the categories, depending on the species. For example, moose densities is related both to provisioning (meat) and #cultural services (hunting).",
#         radioGroupButtons('iEScategory', 'Associated ecosystem service category',
#                           choices = c(
#                             "Supporting", 
#                             "Provisioning", 
#                             "Cultural",
#                             "Regulating"))),


tags$hr(),
h4("Fields related to the reference state:", style="background-color:lightblue;"),

  # 4 INPUT rType ----
  tags$div(title = "The list of options is non-exhaustive, but chose the one you think fits best. Otherwise select 'other'.",
         pickerInput('rType', "Type of reference state.",
                     choices = refStates,
                     selected = "OTH - other"
         )),

  # 4 INPUT rTypeSnippet ----
  tags$div(title = "A short excerpt from the publication (1-10 sentences) that justifies the assignment of reference state. The text must be directly copied, but may consist of sentences that are not next to each other in the original text.",
         textInput("rTypeSnippet", 
                   "Reference state - snippet", 
                   value = "")),

  # 4 INPUT rTypeRemarks ----
  tags$div(title = "An optional field where yuo can comment on the choice of reference state. 
           Example: Year 1850 was used to define the reference state.",
         textInput("rTypeRemarks", 
                   "Comments on choice of reference state", 
                   value = "")),

# 4 INPUT rResolution ----
tags$div(title = "The finest geographical resolution of the reference value(s). The scale for the reference value should be somewhere between that of iSpatialExtent and iSpatialResolution. Is the reference value is the same across the EAA, then rResolution equals iSpatialExtent. If the reference values are unique to each indicator value (i.e. uique reference value for each grid cell), then rResolution equals iSpatialResolution.",
         pickerInput('rResolution', "Spatial resolution of the reference value(s)",
                     choices = scale1
         )),


  # 4 INPUT rRescalingMethod ----
  tags$div(title = "Pick the category that fits the bets. If a two-sided rescaling has been done (i.e. both values that are higher and those that are lower than the reference value is scaled to become indicator values lower than the maximum possible value), this should always be chosen. If the variable is normalised between two extremes (a best and worst possible condition for example), this implies a linear rescaling method.",
         radioGroupButtons('rRescalingMethod', "Rescaling method",
                     choices = rescalingMethod
         )),

  # 4 INPUT rMax ----
  tags$div(title = "A definition or description of the reference value, i.e. the maximum indicator value.
           Example: 'A species composition and a CWM similar to a reference community'",
           textInput("rMax", 
                     "Explanation of the reference value", 
                     value = "")),
  
  # 4 INPUT rMin ----
  tags$div(title = "A definition or description of the lower limit for the indicator (the porest condition).
           Example: 'Species extinct'",
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
      tabPanel("Instructions",
               
               p("This app was developed by Anders L. Kolstad with the purpuse of aiding and standardising the data entrering for a systematic review nicknamed ", tags$a(href="https://github.com/anders-kolstad/theIndiMap/", "The IndiMap"), "This is a crowd sourced review, meaning that in principle anyone can contribute. The app is (or will be made) available online.",style = "width: 500px;"),
               
               p("The data entry is hierarchical, with a one-to-many relationship between selected publications and the indicators that are reported inside these. The", 
                 tags$a(href="https://anders-kolstad.github.io/theIndiMap/", "review itself"), 
                 "will eventually explain in more detail how we selected the publications for the review, and what inclusion criteria we used, both for the publications and the indicators. Most importantly, all the indicators are rescaled (i.e. normalised according to a reference value) and developed for terrestrial ecosystems.",style = "width: 500px;"),
               
               p("Before you can start entering information about the indicators you first need to enter information about the publication. If you are starting from scratch, go to", tags$i("Register publication"), "and select", tags$i("Create new"), ". This will autogenerate a uniqe identifier. Continue filling out the form. You can alway see the updated preview of you data on the right. when done, select", tags$i("Create a new file"), "at the bottom, and then press the download botton in the lower right part of the screen, below the data preview.", tags$i("(Pro tip: you may want to read the publication before opening the app, and use your notes to quickly fill in the form all at once. This is because the app may shut down if you go for coffee!)."), "This file is a standalone cvs file, and we call this a publication profile. The file name is just a time stamp.",style = "width: 500px;"),
               
               p("If you want to edit the publication profile you just made, go back to the top and select", tags$i("Edit."), "Follow the instruction to locate the folder where you saved the csv. This should be the same folder where you keep all the publication profiles. If you have cloned the entire GitHub repo (which is the recomended workflow) then this folder should be /data/publicationProfiles. Now, because the file names are so cryptic, you need to initially select all of them and let the app extract the publication titles, which you then pick from in order to upload the correct profile. After you've selected a publication by its title, you can preview that datafile to the right, above the edited version. When you have the correct publication profile, you can populate the form using the switch. Control that the", tags$i("Publication profile"),  "on the right is updated correctly. Continue editing the file. When reaching the end, in most cases you want to overwrite the existing profile to avoid duplications. Therefore, this time select", tags$i("Replace the uploaded file."), "This will reuse the file name and overwrite the old version.",style = "width: 500px;"),
               
               p("When you have a poblication profile, you can go on to process the individual indicators that are described in it. Switch to the", tags$i("Register indicator"), "tab. Most of the workflow is similar as what you just did for the publication, except you also need to be careful to link the indicator to the correct publication.", style = "width: 500px;")
      ),
      
# 5 Instructions----

      tabPanel("Contact",
               
        p("For technical issues with the app itself, including suggestions for improvments, please look though", tags$a(href="https://github.com/anders-kolstad/theIndiMap/issues", "existing issues"), "in the GitHub repo if the issue is already raised, and if not, create a new issue there.",style = "width: 500px;"),
        
        p("For other questions or comments, either about the app or the systematic review, contact Anders L. Kolstad: anders.kolstad@nina.no",
          style = "width: 500px;")
               
               
               )

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
                      choices = unique(publicationList()$pTitle))
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
  ## iID ----
  iUUID <- reactive({
    ifelse(is.na(iForm()$value[iForm()$parameter == "iID"]), 
           uuid::UUIDgenerate(),
           iForm()$value[iForm()$parameter == "iID"])
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
                            choices = scale1,
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
  
  ## pID ----
  observeEvent(input$localPub2, {
    updatePickerInput(session = session, inputId = "pubDrop2",
                      choices = unique(publicationList2()$pTitle))
  })
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
  
  ## iLatitude ----
  observeEvent(input$i_populate, {
    updateNumericInput(session = session,
                       'iLatitude',
                       value = iForm()$value[iForm()$parameter == "iLatitude"])
  })
  
  ## iLongitude ----
    observeEvent(input$i_populate, {
      updateNumericInput(session = session,
                         'iLongitude',
                         value = iForm()$value[iForm()$parameter == "iLongitude"])
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
                      choices = c("NA",
                                  "1 - complete",
                                  "2 - area representative",
                                  "3 - oppurtunistic or sporadic",
                                  "4 - unknown"),
                      selected = iForm()$value[iForm()$parameter == "dSpatialCoverage"])
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
                      'iSpatialExtent',
                      choices = scale1,
                      selected = iForm()$value[iForm()$parameter == "iSpatialExtent"])
  })
  ## iSpatialResolution ----
  observeEvent(input$i_populate, {
    updatePickerInput(session = session,
                      'iSpatialResolution',
                      choices = scale1,
                      selected = iForm()$value[iForm()$parameter == "iSpatialResolution"])
  })
  ## iTemporalCoverage ----
  observeEvent(input$i_populate, {
    updateNumericInput(session = session,
                      'iTemporalCoverage',
                      value = iForm()$value[iForm()$parameter == "iTemporalCoverage"])
  })
  ## iMap ----
  observeEvent(input$i_populate, {
    updateRadioGroupButtons(session = session,
                       'iMap',
                       choices = c("No", "Yes", "Not by itself, but as part of an aggregated index"),
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
      updateRadioGroupButtons(session = session,
                              'iSubIndex',
                              choices = c("No", "Yes", "Unclear"),
                              selected = iForm()$value[iForm()$parameter == "iSubIndex"])    
      })
    
  ## iModelling ----
    observeEvent(input$i_populate, {
      updateRadioGroupButtons(session = session,
                              'iModelling',
                              choices = c("No", "Yes", "Unclear"),
                              selected = iForm()$value[iForm()$parameter == "iModelling"])    
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
                        choices = c("NA",
                                    "A1 Physical state characteristics",
                                    "A2 Chemical state characteristics",
                                    "B1 Compositional state characteristics",
                                    "B2 Structural state characteristics",
                                    "B3 Functional state characteristics",
                                    "C1 Landscape and seascape characteristics",
                                    "Other (e.g. pre-aggregated indices)"),
                        selected = iForm()$value[iForm()$parameter == "iECTclass"])
      })
    
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
      updateRadioGroupButtons(session = session,
                              'rRescalingMethod',
                              choices = rescalingMethod,
                              selected = iForm()$value[iForm()$parameter == "rRescalingMethod"])    
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
      if(nchar(pExport()$value[pExport()$parameter == "pTitle"]) < 3) "MISSING: Publication title is not valid",
      if(nchar(pExport()$value[pExport()$parameter == "githubUser"]) < 3) "MISSING: Please enter GitHub user name",
      if(nchar(pExport()$value[pExport()$parameter == "pZoteroID"]) < 3) "MISSING: Enter the full URL for the Zotero entry",
      if(input$pAssessment == "Assessment"){
        if(!is.numeric(pExport()$value[pExport()$parameter == "pAssessmentYear"])) {"MISSING: Publication Year is missing"}
        },
      if(input$pAssessment == "Assessment"){
        if(!is.numeric(pExport()$value[pExport()$parameter == "pAggregation"])) {"MISSING: The level of aggregetaion is missing"}
      }
      ),
      type = "error",
      duration = NA
    )
  })
  
# B* OUTPUT: pExport   ----
  
# Compile CSVs for the publication profile
  
  
  pExport <- reactive({
    
    # shorten name
    dat <- pform()
    
    #update value column based on input
    dat$value[dat$parameter == "pZoteroID"] <- input$pzoteroid
    
    dat$value[dat$parameter == "pID"] <- pUUID()
      
    dat$value[dat$parameter == "pTitle"] <- input$ptitle
    
    dat$value[dat$parameter == "pBibliography"] <- input$pbibliography

    dat$value[dat$parameter == "githubUser"] <- input$githubuser
    
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
  
  
  
# B* OUTPUT: iExport   ----
  
  # Compile CSVs for the indicator profile
  
  
  iExport <- reactive({
    # shorten name
    dat <- iForm()
    
    dat$value[dat$parameter == "iID"] <- iUUID()
    dat$value[dat$parameter == "pID"] <- input$pubDrop2
    dat$value[dat$parameter == "iName"] <- input$iName
    dat$value[dat$parameter == "githubUser"] <- input$githubuser2
    dat$value[dat$parameter == "iRedundant"] <- input$iRedundant
    dat$value[dat$parameter == "iRedundantRemarks"] <- ifelse(
      input$iRedundant == "Unique",
        NA,
        input$iRedundantRemarks)
    dat$value[dat$parameter == "iRedundantReferences"] <- ifelse(
      input$iRedundant == "Unique",
      NA,
      input$iRedundantReferences)
    dat$value[dat$parameter == "iContinent"] <- paste(input$iContinent, collapse = " | ")
    dat$value[dat$parameter == "iCountry"] <- paste(input$iCountry, collapse = " | ")
    dat$value[dat$parameter == "iLowerGeography"] <- input$iLowerGeography
    dat$value[dat$parameter == "iLatitude"] <- input$iLatitude
    dat$value[dat$parameter == "iLongitude"] <- input$iLongitude
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
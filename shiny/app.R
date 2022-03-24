library(shiny)
library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
#library(tidyverse)
#library(readxl)


# UI ----------------------------------------------------------------------



ui <- 
  navbarPage("The IndiScaleReview",
             tabPanel("Plot"),
             navbarMenu("More",
                        tabPanel("Summary"),
                        "----",
                        "Section header",
                        tabPanel("Table")
             )
  )
    
  




server <- function(input, output) {
  
  

}

shinyApp(ui = ui, server = server)
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#install.packages("shinydashboard")
#install.packages("devtools")
devtools::install_github("stefanwilhelm/ShinyRatingInput")
devtools::install_github('andrewsali/shinycssloaders')
library(shiny)
library(shinydashboard)
library(ShinyRatingInput)
library(shinycssloaders)
library(dplyr)
# Define UI for application that draws a histogram
shinyUI(
  
  dashboardPage(
    skin = "purple",
  dashboardHeader(title = "Let's get some book recommendations!",
                  titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    fluidRow(tags$head(tags$style(HTML("
                                div.actionButton {
                                      text-align: center;
                                      }
                                      div.box-header {
                                      text-align: center;
                                      }
                                      "))),
      box(width = 12, title = "Rate Some Books", background = "purple", solidHeader = TRUE, collapsible = TRUE,
          div(class = "padding-left: 5px; 
              height: 450px; 
              overflow-y: scroll;",
              uiOutput('ratings')
          )
      )
    ),
    
    fluidRow(
      box(width = 12, title = "Books You May Like", background = "purple", solidHeader = TRUE, collapsible = TRUE,
          div(style="display:inline-block;width:100%;text-align: center;",actionButton("myButton", "Get Recommendations", icon = icon("book"))),
          withSpinner(tableOutput("results"))
      )
      
    )
  )
))

library(tidyverse)
library(readxl)

collegeBasketball <- read_excel("teams.xlsx", col_names = TRUE) %>%
  rename(Team = school, Conference = conf, `Win Probability` = estimate, `Lower 95%` = lower,
         `Upper 95%` = upper) %>%
  arrange(desc(`Win Probability`), Team, Conference)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Eric's NCAA Model"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("Conference",
                       "Conference:",
                       c("All",
                         unique(as.character(collegeBasketball$Conference))))
    ),
    column(4,
           selectInput("Team",
                       "Team:",
                       c("All",
                         unique(as.character(collegeBasketball$Team))))
    )),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- collegeBasketball
    if (input$Conference != "All") {
      data <- data[data$Conference == input$Conference,]
    }
    if (input$Team != "All") {
      data <- data[data$Team == input$Team,]
    }
    data
  }))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
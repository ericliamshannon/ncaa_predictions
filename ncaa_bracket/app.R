library(data.table)
library(openxlsx)

scores2 <- read.xlsx("teams.xlsx", colNames = TRUE)
scores2 <- setnames(scores2,
                                old = c("team", "V2", "OFFENSE", "DEFENSE", "estimate", "lower", "upper"),
                                new = c("Team", "Conference", "Offense Score", "Defense Score", "Win Probability", "Lower CI", "Upper CI"))
scores2 <- scores2[complete.cases(scores2), ]
scores2 <- scores2[order(scores2$Conference, scores2$Team), ]

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
                         unique(as.character(scores2$Conference))))
    ),
    column(4,
           selectInput("Team",
                       "Team:",
                       c("All",
                         unique(as.character(scores2$Team))))
    )),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- scores2
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


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui = list(header=dashboardHeader(title="TS Dashboard", titleWidth="92%"),
          sidebar=dashboardSidebar(),
          body=dashboardBody())

# And render the UI
ui <- dashboardPage(ui$header, ui$sidebar, ui$body)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application
shinyApp(ui = ui, server = server)


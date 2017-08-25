library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI <-  dashboardPage(
    dashboardHeader(title = "IBM - Trello Report"),
    dashboardSidebar(
        textInput("key","Please provide your Key"),
        passwordInput("secret", "Please provide your secret"),
        actionButton("do", "Get Report"),
        downloadButton("downloadData", "Download Report"),
        downloadButton("downloadTP", "Download Agile Metrics")
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        verbatimTextOutput("responseText")
    )
)
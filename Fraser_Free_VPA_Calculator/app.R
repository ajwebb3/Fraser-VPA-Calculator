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
library(shinyjs)

# Define UI 
ui <- fluidPage(
  # Header
  headerPanel("Free Valproic Acid Calculator"),
  
  # Sidebar with input fields
  sidebarPanel(
    # Input fields with subtext
    numericInput("total_vpa", "Total VPA Concentration (mg/L)", value = 0, min = 0, step = 0.1),
    numericInput("albumin_value", "Albumin Value (g/L)", value = 0, min = 0, step = 0.1),
    numericInput("bun_value", "BUN Value (mg/dL)", value = 0, min = 0, step = 0.1),
    radioButtons("propofol", "Propofol", choices = c("No", "Yes"), selected = "No"),
    helpText("Select Yes if the patient received propofol within 24 hours of the level being drawn."),
    radioButtons("aspirin", "Aspirin", choices = c("No", "Yes"), selected = "No"),
    helpText("Select Yes if the patient received propofol within 24 hours of the level being drawn."),
    helpText(HTML(paste("Find more information on the Fraser equation <a href='https://journals.lww.com/ccejournal/fulltext/2023/10000/derivation_and_validation_of_a_new_equation_for.12.aspx' target='_blank'>here</a>."))),
    actionButton("calculate", "Calculate"),
    br(),
    # Output with formatting
    verbatimTextOutput("result")
  ),
  
  # Body with additional content (if needed)
  mainPanel(
    useShinyjs(), # Initialize shinyjs
    extendShinyjs(text = "shinyjs.reset = function() { Shiny.onInputChange('calculate', Math.random()); }", functions = c("foo"))
  )
)

# Define server
server <- function(input, output, session) {
  observeEvent(input$calculate, {
    # Convert radio button values to numeric
    propofol_value <- ifelse(input$propofol == "Yes", 1, 0)
    aspirin_value <- ifelse(input$aspirin == "Yes", 1, 0)
    
    # Perform calculation
    predicted_concentration <- 10.74 + 0.34 * input$total_vpa - 4.60 * input$albumin_value + 
      0.02 * input$bun_value + 2.14 * propofol_value + 1.51 * aspirin_value
    
    # Update output with formatting
    output$result <- renderText({
      paste("Predicted Free VPA Concentration: ", round(predicted_concentration, 2))
    })
  })
}

# Run the application
shinyApp(ui, server)

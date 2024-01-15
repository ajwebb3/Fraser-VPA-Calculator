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
    extendShinyjs(text = "shinyjs.reset = function() { Shiny.onInputChange('calculate', Math.random()); }", functions = c("foo")),
    plotOutput("level_plot")
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
    
      # Calculate confidence interval
      lower_ci <- ifelse(predicted_concentration - 13.6 < 0, 0, predicted_concentration - 13.6)
      upper_ci <- predicted_concentration + 14.2
      sd_value <- (13.6 + 14.2) / 3.92
      
      # Data for plotting
      library(tibble)
      plot_data <- tibble(
        pred_conc = predicted_concentration,
        upper = upper_ci,
        lower = lower_ci
      )
      
      # Plotting
      output$level_plot <- renderPlot({
        library(ggplot2)
        
        ggplot(plot_data, aes(x = pred_conc, y = 0)) +
          stat_function(fun = function(x) ifelse(x < 0 | x < lower_ci - 5 | x > upper_ci + 5, NA, 
                                                 dnorm(x, mean = predicted_concentration, sd = sd_value)), 
                        color = "blue", size = 1.5) +
          geom_errorbar(aes(xmin = lower, xmax = upper), color = "grey", position = position_dodge(width = 0.9), width = 0.001) +
          geom_point(color = "red", size = 2, shape = 15) +
          xlim(0, upper_ci + 10) +
          labs(x = "Predicted Free VPA Concentration, mg/L", title = "Estimated Free VPA Level with 95% Confidence Interval") +
          theme_bw() +
          theme(axis.title.y = element_blank(),
                axis.text.y = element_blank(),  
                axis.ticks.y = element_blank(),
                panel.grid = element_blank(),
                text = element_text(size = 16)) +
          annotate("text", x = predicted_concentration, y = 0, label = paste("Therapeutic Range: 5-15 mg/L", "\nPredicted Level: ", 
                                                                               round(predicted_concentration, 2), "mg/L \nInterpretation: ", 
                                                                               ifelse(predicted_concentration < 5, "Subtherapeutic", 
                                                                                      ifelse(predicted_concentration <= 15, "Therapeutic", "Supratherapeutic"))), 
                   hjust = 0.5, vjust = -1.5)
        
      })
  })
}

# Run the application
shinyApp(ui, server)

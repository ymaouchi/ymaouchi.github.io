# Load necessary libraries
library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  
  titlePanel("Interactive GDP Components"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Numeric inputs for GDP components
      numericInput("consumption", "Consumption:", value = 70, min = 0, max = 10000),
      numericInput("investment", "Investment:", value = 15, min = 0, max = 10000),
      numericInput("government", "Government Spending:", value = 10, min = 0, max = 10000),
      numericInput("exports", "Exports:", value = 10, min = 0, max = 10000),
      numericInput("imports", "Imports:", value = 5, min = 0, max = 10000),
      
      # Separator
      hr(),
      
      # Title for Consumption Determinants
      h3("Consumption Determinants"),
      
      # Sliders for consumption determinants
      sliderInput("YD", "Current Disposable Income:", min = 0, max = 10000, value = 5000),
      sliderInput("W", "Household Wealth:", min = 0, max = 100000, value = 50000),
      sliderInput("YF", "Expected Future Income:", min = 0, max = 10000, value = 5000),
      sliderInput("P", "The Price Level:", min = 0, max = 100, value = 50),
      sliderInput("r", "The Interest Rate (%):", min = 0, max = 20, value = 5)
      
    ),
    mainPanel(
      plotlyOutput("gdp_pie_chart_values"),
      plotlyOutput("gdp_pie_chart_percentages"),
      
      # Output for calculated consumption
      h3("Calculated Consumption:"),
      textOutput("calculated_consumption")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Coefficients for the linear consumption model
  a <- 1000  # Intercept
  b <- 0.5   # Coefficient for Current Disposable Income
  c <- 0.3   # Coefficient for Household Wealth
  d <- 0.2   # Coefficient for Expected Future Income
  e <- -10   # Coefficient for The Price Level
  f <- -20   # Coefficient for The Interest Rate
  
  reactive_consumption <- reactive({
    a + b * input$YD + c * input$W + d * input$YF + e * input$P + f * input$r
  })
  
  output$gdp_pie_chart_values <- renderPlotly({
    net_exports <- input$exports - input$imports
    net_exports_label <- ifelse(net_exports < 0, paste("-", abs(net_exports)), net_exports)
    gdp_components <- data.frame(
      Component = c("Consumption", "Investment", "Government Spending", "Net Exports"),
      Value = c(input$consumption, input$investment, input$government, abs(net_exports)),
      Label = c(input$consumption, input$investment, input$government, net_exports_label)
    )
    plot_ly(gdp_components, labels = ~Component, values = ~Value, type = 'pie', text = ~Label, textinfo = 'label+text') %>%
      layout(title = 'GDP Components ($)',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$gdp_pie_chart_percentages <- renderPlotly({
    net_exports <- input$exports - input$imports
    gdp_components <- data.frame(
      Component = c("Consumption", "Investment", "Government Spending", "Net Exports"),
      Value = c(input$consumption, input$investment, input$government, abs(net_exports))
    )
    total_gdp <- sum(gdp_components$Value)
    gdp_components$Percentage <- gdp_components$Value / total_gdp * 100
    plot_ly(gdp_components, labels = ~Component, values = ~Percentage, type = 'pie', textinfo = 'label+percent') %>%
      layout(title = 'GDP Components (Percentages)',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$calculated_consumption <- renderText({
    paste("Calculated Consumption: $", round(reactive_consumption(), 2))
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

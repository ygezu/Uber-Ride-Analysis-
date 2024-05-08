library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)
library(tidyr)
library(shiny)
library(DT)
library(leaflet.extras)
library(shinyjs)
library(rpart)
library(rpart.plot)






ui <- fluidPage(
  titlePanel("Uber Trip Data and Prediction Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput("baseInput", "Choose Base:", choices = unique(all_data$Base)),
      numericInput("hourInput", "Select Hour:", value = 12, min = 0, max = 23),
      actionButton("predict", "Predict Trips"),
      tags$hr(),
      textOutput("predictionOutput")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trips by Hour", plotOutput("plotTripsByHour")),
        tabPanel("Prediction Model", plotOutput("modelPlot"))
      )
    )
  )
)


server <- function(input, output) {
  # Load the model data
  model_data <- readRDS("model_data.rds")
  
  # Reactive expression to update and store the model
  model <- reactive({
    rpart(Trips ~ Hour + Month + Base, data = model_data, method = "anova")
  })
  
  # Observe event for prediction
  observeEvent(input$predict, {
    prediction_data <- data.frame(
      Hour = input$hourInput,
      Month = factor(format(Sys.Date(), "%B"), levels = month.name, labels = month.name, ordered = FALSE),  # Match this definition with the training data setup
      Base = input$baseInput
    )
    predicted_trips <- predict(model(), newdata = prediction_data)
    output$predictionOutput <- renderText({
      paste("Predicted trips for", input$baseInput, "at", input$hourInput, ":", round(predicted_trips, 2))
    })
  })
  
  # Plot for model (Decision Tree)
  output$modelPlot <- renderPlot({
    rpart.plot(model())
  })
  
  # Plot for trips by hour
  output$plotTripsByHour <- renderPlot({
    ggplot(model_data, aes(x = Hour, y = Trips)) +
      geom_line(group = 1) +
      labs(title = "Total Trips by Hour", x = "Hour of the Day", y = "Number of Trips")
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)

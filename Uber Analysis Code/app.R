library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)
library(tidyr)
library(shiny)
library(DT)
library(leaflet.extras)
library(shinyjs)


# Load pre-processed data for visualizations from RDS files
trips_by_hour <- readRDS("trips_by_hour.rds")
trips_by_hour_month <- readRDS("trips_by_hour_month.rds")
trips_by_day <- readRDS("trips_by_day.rds")
trips_by_day_month <- readRDS("trips_by_day_month.rds")
trips_by_base_month <- readRDS("trips_by_base_month.rds")
heatmap_hour_day <- readRDS("heatmap_hour_day.rds")
heatmap_month_day <- readRDS("heatmap_month_day.rds")
heatmap_month_week <- readRDS("heatmap_month_week.rds")
heatmap_bases_day_week <- readRDS("heatmap_bases_day_week.rds")

# Example data preprocessing for a prediction model
model_data <- all_data %>%
  group_by(Hour, Date, Month, Base) %>%
  summarise(Trips = n(), .groups = 'drop')

# Save the processed data for modeling
saveRDS(model_data, "model_data.rds")


ui <- fluidPage(
  titlePanel("Uber Trip Data Analysis"),
  tabsetPanel(
    tabPanel("Visualizations",
             tabsetPanel(
               tabPanel("Chart for Trips by Hour", plotOutput("plotTripsByHourBar"), textOutput("description1")),
               tabPanel("Trips by Hour and Month", plotOutput("plot_hour_month"), textOutput("description2")),
               tabPanel("Chart for Trips on Every Day of the Month", plotOutput("plot_trips_every_day"), textOutput("description4")),
               tabPanel("Chart by Trips by Day and Month", plotOutput("plot_trips_by_month"), textOutput("description5")),
               tabPanel("Chart Trips by Bases and Month", plotOutput("plot_base_month"), textOutput("description6")),
               tabPanel("Heat Map - Hour and Day", plotOutput("heatmap_hour_day"), textOutput("description7")),
               tabPanel("Heat Map - Month and Day", plotOutput("heatmap_month_day"), textOutput("description8")),
               tabPanel("Heat Map - Month and Week", plotOutput("heatmap_month_week"), textOutput("description9")),
               tabPanel("Heat Map - Bases and Day of Week", plotOutput("heatmap_bases_day_week"), textOutput("description10"))
             )
    ),
    tabPanel("Geo Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("zipcode", "Choose a Zip Code:", choices = c("Select" = "", "60607" = "60607", "60666" = "60666")),
                 actionButton("reset", "Reset View")
               ),
               mainPanel(
                 leafletOutput("map")
               )
             )
    ),
    tabPanel("Prediction Model",
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
                   tabPanel("Trips by Hour", plotOutput("plotTriopsByHour")),
                   tabPanel("Prediction Model", plotOutput("modelPlot"))
                 )
               )
             )
    )
  )
)



server <- function(input, output) {
  output$plotTripsByHourBar <- renderPlot({
    ggplot(trips_by_hour, aes(x = factor(Hour), y = Trips)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Trips by Hour", x = "Hour of the Day", y = "Number of Trips") +
      theme_minimal()
  })
  
  output$description1 <- renderText({
    "This graph demonstrates the distribution of Uber trips throughout the day, showing patterns that correspond to daily commuting behaviors. The number of trips begins low in the early morning hours and gradually increases, peaking at 5 p.m., which most likely reflects the evening rush hour when people are returning home from work or going out. The graph then shows a decrease in trip numbers during the late evening and night, possibly showing a lower demand for rides as the night progresses."
  })
  
  output$plot_hour_month <- renderPlot({
    ggplot(trips_by_hour_month, aes(x = Hour, y = Trips, fill = Month)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Hour and Month", x = "Hour of Day", y = "Number of Trips")
  })
  
  output$description2 <- renderText({
    "This graph shows how Uber trips were distributed throughout the day from April to September. The data consistently shows two daily peaks, which correspond to morning and evening commuting times, with the evening peak occurring around 5 PM. Additionally, September has particularly high trip volumes, peaking significantly higher than other months. This indicates a rise in demand, possibly due to seasonal activities or events."
  })
  
  output$plot_trips_every_day <- renderPlot({
    ggplot(trips_by_day, aes(x = as.Date(Date), y = Trips)) +
      geom_col() +
      labs(title = "Trips on Every Day of the Month", x = "Day", y = "Number of Trips") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  })
  
  output$description4 <- renderText({
    "This graph shows the number of Uber trips taken on each day over several months. The bars represent fluctuations in daily trip volumes, with some days seeing significantly higher demand. This variation could be correlated to specific events, weekdays versus weekends, or other factors influencing rider needs. The data shows that, while demand is relatively consistent, there are notable peaks that could indicate special events or typical high-demand days such as Fridays or Saturdays."
  })
  
  output$plot_trips_by_month <- renderPlot({
    ggplot(trips_by_day_month, aes(x = Date, y = Trips, fill = Month)) +
      geom_col() +
      labs(title = "Trips by Day and Month", x = "Day", y = "Number of Trips", fill = "Month") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  })
  
  output$description5 <- renderText({
    "The graph shows the daily volume of Uber trips over several months. Each month is represented by a different color, showing how trip numbers vary by day within a month. For instance, there is significant daily variation in trips, with some days peaking higher than others, which may align with weekends or special events. The months of May and September have significantly higher trip volumes, which could indicate seasonal patterns or events that boost ride demand."
  })
  
  output$plot_base_month <- renderPlot({
    ggplot(trips_by_base_month, aes(x = Base, y = Trips, fill = Month)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Bases and Month", x = "Base", y = "Number of Trips")
  })
  
  output$description6 <- renderText({
    "The graph compares the volume of Uber trips from various bases over different months. Each base is represented by a unique set of bars that are color-coded by month and show the number of trips that originate from that base each month. Notably, base B02617 consistently shows high trip volumes over several months, with a peak in May. In contrast, base B02764 shows a significant spike in September."
  })
  
  output$heatmap_hour_day <- renderPlot({
    ggplot(heatmap_hour_day, aes(x = Hour, y = day, fill = Trips)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Heat Map of Trips by Hour and Day", x = "Hour", y = "Day")
  })
  
  output$description7 <- renderText({
    "This heatmap shows how Uber trips are distributed throughout the day over several months. Each row represents a day, each column an hour, and the colors range from dark purple (low trips) to bright yellow (high trips). This graph shows peak demand during typical commuting hours in the mornings and evenings, whereas late-night and early-morning hours consistently show lower activity."
  })
  
  output$heatmap_month_day <- renderPlot({
    ggplot(heatmap_month_day, aes(x = Month, y = day, fill = Trips)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Heat Map of Trips by Month and Day", x = "Month", y = "Day")
  })
  
  output$description8 <- renderText({
    "This heatmap shows the number of Uber trips per day across the different months. Horizontal bands represent months, while vertical lines within each band correspond to daily trip volumes, with colors ranging from dark purple (fewer trips) to bright yellow (more trips). Notably, there are periodic spikes in color intensity, possibly reflecting increased trip demand on weekends or near the end of the month."
  })
  
  output$heatmap_month_week <- renderPlot({
    ggplot(heatmap_month_week, aes(x = Month, y = Week, fill = Trips)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Heat Map of Trips by Month and Week", x = "Month", y = "Week")
  })
  
  output$description9 <- renderText({
    "This heatmap depicts the number of Uber trips per week from April to September. Each block represents a week in a month and is colored based on the number of trips, with dark purple representing fewer trips and bright yellow representing more trips. We can notice a gradual increase in trip volume over the weeks, particularly from July to September, indicating a potential seasonal peak in demand."
  })
  
  output$heatmap_bases_day_week <- renderPlot({
    ggplot(heatmap_bases_day_week, aes(x = Base, y = Day_of_Week, fill = Trips)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Heat Map of Trips by Bases and Day of Week", x = "Base", y = "Day of Week")
  })
  
  output$description10 <- renderText({
    "This heatmap shows the number of Uber trips for various bases on different days of the week. Each row represents a day, Sunday through Saturday, and each column represents a different Uber base, with color intensity indicating trip volume. We can see a significant variation between bases, with some (such as B02617) consistently showing high trip volumes throughout the week, particularly on Fridays. In contrast, other bases, such as B02512 and B022764, have much lower activity."
  })
  
  # Map rendering
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -87.6470, lat = 41.8695, popup = "University of Illinois at Chicago") %>%
      addMarkers(lng = -87.9073, lat = 41.9742, popup = "O'Hare International Airport")
  })
  
  observe({
    zip <- input$zipcode
    if (zip != "") {
      update_map <- leafletProxy("map", session)
      if (zip == "60607") {
        update_map %>% setView(lng = -87.6515, lat = 41.8743, zoom = 14)
      } else if (zip == "60666") {
        update_map %>% setView(lng = -87.9073, lat = 41.9742, zoom = 14)
      }
    }
  })
  
  observeEvent(input$reset, {
    reset_map <- leafletProxy("map", session)
    reset_map %>% setView(lng = -87.6298, lat = 41.8781, zoom = 11)
  })
 
  
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
      paste("Predicted trips for", input$baseInput, "at", input$hourInput, "hour:", round(predicted_trips, 2))
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
      labs(title = "Total Triops by Hour", x = "Hour of the Day", y = "Number of Trips")
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)

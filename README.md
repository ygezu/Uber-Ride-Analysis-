# Uber-Ride-Analysis

# Introduction 
This project examines Uber trip data using visualizations and analysis presented in a Shiny app. By utilizing the charts and maps, we can explore trip patterns by hour, month, day, and location, gaining insights into Uber ride trends. The interface also includes a geospatial map feature for visualizing trip locations. In addition, a predictive ride model forecasts future trip demand based on historical data. This analysis will be helpful in exploring and understanding Uber trip patterns, which will ultimately help in informing decision-making and optimizing ride-sharing services.

<img src="https://github.com/ygezu/Uber-Ride-Analysis-/assets/159511253/92ff0067-e777-421c-a436-080e51963414" width="900" height="500">

# Data Dictionary 📖
Columns utilized for this analysis 

1. Date
2. Time
3. Lat
4. Lon
5. Base
7. Hour
9. Month
10. Week

# Data Cleaning 🧹

1. This code converts the Date. Time column in the all_data data frame to the POSIXct format, standardizing the date-time values to UTC.


        # Changing the date column to a date schema
        all_data$Date.Time <- as.POSIXct(all_data$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
   
3. This code creates new columns in our table to restructure time-related components into their correct formats. It extracts and formats the time, hour, date, month, and week. It also converts the Base column into a factor for better categorical analysis.

        all_data$Time <- format(all_data$Date.Time, "%H:%M:%S") # Extract Time in HH:MM:SS format
        all_data$Hour <- as.integer(substr(all_data$Time, 1, 2)) # Extract Hour directly from the newly formatted Time
        all_data$Date <- as.Date(all_data$Date.Time)
        all_data$Month <- factor(month(all_data$Date, label = TRUE, abbr = FALSE), levels = month.name, labels = month.name)
        all_data$Week <- week(all_data$Date)
        all_data$Base <- as.factor(all_data$Base)

# Preparing Aggregated data for plots
These codes get the aggregate data for the various analysis points to then plot our graphs. 
        
        # PIVOT TABLE FOR TRIPS BY THE HOUR 
        trips_by_hour <- all_data %>%
        group_by(Hour) %>%
        summarise(Trips = n(), .groups = 'drop')
        
        # CHART TRIP BY HOUR AND MONTH 
        trips_by_hour_month <- all_data %>%
          group_by(Hour, Month) %>%
          summarise(Trips = n(), .groups = 'drop')
        
        # BAR CHART TRIPS FOR EVERY HOUR
        bar_chart_trips_hour <- all_data %>%
          group_by(Hour) %>%
          summarise(Trips = n(), .groups = 'drop')
        
        # CHART TRIPS ON EVERY DAY OF THE MONTH
        trips_by_day <- all_data %>%
          group_by(Date) %>%
          summarise(Trips = n(), .groups = 'drop')
        
        # CHART BY TRIPS BY DAY AND MONTH
        trips_by_day_month <- all_data %>%
          group_by(Date, Month) %>%
          summarise(Trips = n(), .groups = 'drop')
        
        # CHART BY TRIPS BY BASES AND MONTH
        trips_by_base_month <- all_data %>%
          group_by(Base, Month) %>%
          summarise(Trips = n(), .groups = 'drop')

  # Heat Maps 
  These code snippets were used to plot the heat maps for the different time-related components 
       
        # HEAT MAP THAT DISPLAYS BY HOUR AND DAY
       heatmap_hour_day <- all_data %>%
          group_by(Hour, day = as.Date(Date.Time)) %>%
          summarise(Trips = n(), .groups = 'drop')
        
        # HEAT MAP BY MONTH AND DAY
        heatmap_month_day <- all_data %>%
          group_by(Month, day = as.Date(Date.Time)) %>%
          summarise(Trips = n(), .groups = 'drop')
        
        # HEAT MAP BY MONTH AND WEEK
        heatmap_month_week <- all_data %>%
          group_by(Month, Week) %>%
          summarise(Trips = n(), .groups = 'drop')
        
        # HEAT MAP BASES AND DAY OF WEEK
        heatmap_bases_day_week <- all_data %>%
          group_by(Base, Day_of_Week = wday(Date.Time, label = TRUE)) %>%
          summarise(Trips = n(), .groups = 'drop')

  # GeoSpatial leaflets 
  Utilizing the data table, I have created a shiny app with a leaflet. This leaflet pinpoints two specific locations- the University of Illinois at Chicago and Ohare International Airport. It also has a side drop-down option to pick from different zipcodes. 

        ui <- fluidPage(
          titlePanel("Uber Trip Data Analysis"),
          sidebarLayout(
            sidebarPanel(
              selectInput("zipcode", "Choose a Zip Code:", choices = c("Select" = "", "60607" = "60607", "60666" = "60666")),  # Example zip codes
              actionButton("reset", "Reset View")
            ),
            mainPanel(
              leafletOutput("map")
            )))


        server <- function(input, output, session) {
          
          # Map rendering
          output$map <- renderLeaflet({
            leaflet() %>%
              addTiles() %>%
              addMarkers(lng = -87.6470, lat = 41.8695, popup = "University of Illinois at Chicago") %>%
              addMarkers(lng = -87.9073, lat = 41.9742, popup = "O'Hare International Airport")
          })
          # Dynamic update of map based on zip code selection
          observe({
            zip <- input$zipcode
            if (zip != "") {
              update_map <- leafletProxy("map", session)
              if (zip == "60607") {  # Focus on the UIC area
                update_map %>% setView(lng = -87.6515, lat = 41.8743, zoom = 14)
              } else if (zip == "60666") {  # Focus on O'Hare
                update_map %>% setView(lng = -87.9073, lat = 41.9742, zoom = 14)
              }}})
          # Filter rides based on zip code and display on the map
          observe({
            if (input$zipcode != "") {
              filtered_data <- all_data %>%
                filter(ZipCode == input$zipcode)  # Assuming 'ZipCode' is a column in your data
              leafletProxy("map", session) %>%
                clearMarkers() %>%
                addMarkers(data = filtered_data, ~Lon, ~Lat, popup = ~Base)
            }})}



  # Prediction Model 
  Finally, I have created a shiny app with a prediction model. The model utilizes data from the table and trains a decision tree model that predicts the number of trips based on inputs like hour, month, and base location. It also incudes a Trips by hour graph to give context for better predictions. 

          model_data <- all_data %>%
          group_by(Hour, Date, Month, Base) %>%
          summarise(Trips = n(), .groups = 'drop')

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
        

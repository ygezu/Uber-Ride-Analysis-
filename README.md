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



  # Prediction Model 
        


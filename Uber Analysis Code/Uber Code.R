library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)
library(tidyr)
library(shiny)
library(DT)
library(leaflet.extras)
library(shinyjs)

files <- c(
  "uber-raw-data-apr14.csv",
  "uber-raw-data-aug14.csv",
  "uber-raw-data-jul14.csv",
  "uber-raw-data-jun14.csv",
  "uber-raw-data-may14.csv",
  "uber-raw-data-sep14.csv"
)

# Read and bind all CSVs into one dataframe
data <- lapply(files, read.csv)
all_data <- bind_rows(data)

# Saving as a RDS
saveRDS(all_data, "all_data.rds")


# Changing the date column to a date schema
all_data$Date.Time <- as.POSIXct(all_data$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")


all_data$Time <- format(all_data$Date.Time, "%H:%M:%S") # Extract Time in HH:MM:SS format
all_data$Hour <- as.integer(substr(all_data$Time, 1, 2)) # Extract Hour directly from the newly formatted Time
all_data$Date <- as.Date(all_data$Date.Time)
all_data$Month <- factor(month(all_data$Date, label = TRUE, abbr = FALSE), levels = month.name, labels = month.name)
all_data$Week <- week(all_data$Date)
all_data$Base <- as.factor(all_data$Base)  

# Example data preprocessing for a prediction model
model_data <- all_data %>%
  group_by(Hour, Date, Month, Base) %>%
  summarise(Trips = n(), .groups = 'drop')

# Save the processed data for modeling
saveRDS(model_data, "model_data.rds")

# PIVOT TABLE FOR TRIPS BY THE HOUR 
trips_by_hour <- all_data %>%
  dplyr::group_by(Hour) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(trips_by_hour, "trips_by_hour.rds")

# CHART TRIP BY HOUR AND MONTH 
trips_by_hour_month <- all_data %>%
  dplyr::group_by(Hour, Month) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(trips_by_hour_month, "trips_by_hour_month.rds")

# BAR CHART TRIPS FOR EVERY HOUR
bar_chart_trips_hour <- all_data %>%
  dplyr::group_by(Hour) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(bar_chart_trips_hour, "chart_trips_by_hour.rds")

# CHART TRIPS ON EVERY DAY OF THE MONTH
trips_by_day <- all_data %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(trips_by_day, "trips_by_day.rds")

# CHART BY TRIPS BY DAY AND MONTH
trips_by_day_month <- all_data %>%
  dplyr::group_by(Date, Month) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(trips_by_day_month, "trips_by_day_month.rds")

# CHART BY TRIPS BY BASES AND MONTH
trips_by_base_month <- all_data %>%
  dplyr::group_by(Base, Month) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(trips_by_base_month, "trips_by_base_month.rds")

# HEAT MAP THAT DISPLAYS BY HOUR AND DAY
heatmap_hour_day <- all_data %>%
  dplyr::group_by(Hour, day = as.Date(Date.Time)) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(heatmap_hour_day, "heatmap_hour_day.rds")

# HEAT MAP BY MONTH AND DAY
heatmap_month_day <- all_data %>%
  dplyr::group_by(Month, day = as.Date(Date.Time)) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(heatmap_month_day, "heatmap_month_day.rds")

# HEAT MAP BY MONTH AND WEEK
heatmap_month_week <- all_data %>%
  dplyr::group_by(Month, Week) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(heatmap_month_week, "heatmap_month_week.rds")

# HEAT MAP BASES AND DAY OF WEEK
heatmap_bases_day_week <- all_data %>%
  dplyr::group_by(Base, Day_of_Week = wday(Date.Time, label = TRUE)) %>%
  dplyr::summarise(Trips = dplyr::n(), .groups = 'drop')

saveRDS(heatmap_bases_day_week, "heatmap_bases_day_week.rds")

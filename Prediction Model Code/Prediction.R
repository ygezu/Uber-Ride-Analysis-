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

# Load the RDS file for use in the app to avoid re-reading CSVs
all_data <- readRDS("all_data.rds")

# Changing the date column to a date schema and preparing additional time features
all_data <- all_data %>%
  mutate(
    Date.Time = as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
    Time = format(Date.Time, "%H:%M:%S"),
    Hour = as.integer(substr(Time, 1, 2)),
    Date = as.Date(Date.Time),
    Month = factor(month(Date, label = TRUE, abbr = FALSE), levels = month.name, labels = month.name, ordered = FALSE),  # Ensure 'ordered = FALSE' is explicitly set
    Week = week(Date),
    Base = as.factor(Base)
  )

# Example data preprocessing for a prediction model
model_data <- all_data %>%
  group_by(Hour, Date, Month, Base) %>%
  summarise(Trips = n(), .groups = 'drop')

# Save the processed data for modeling
saveRDS(model_data, "model_data.rds")











library(tidyverse)
library(lubridate)

# Read csv
data <- read_csv("C:/Users/danie/Documents/Running/TrainingPlans/CHI_2025_Daniels.csv")

# Date
race_date <- lubridate::mdy("10/12/2025")
data <- data %>% 
  mutate(Date = race_date - days(Days_Out))

# Avg Pace
data <- data %>% 
  mutate(Run_time_seconds = hour(Run_Time)*3600 + minute(Run_Time)*60 + second(Run_Time),
         .after = Run_Time) %>% 
  mutate(Avg_Pace = (seconds_to_period(Run_time_seconds/Run_Dist)))





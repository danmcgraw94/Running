library(tidyverse)
library(lubridate)
library(hms)

# Read csv ---------------------------------------------------------------------
which_cpu_path <- function(cpu_name,work_cpu_path,home_cpu_path){
  work_cpu = str_detect(cpu_name,"EIWR")
  use_cpu = ifelse(work_cpu == T,work_cpu_path,home_cpu_path)
  return(use_cpu)
}

# File Names
work_cpu_path <- "D:/zz.Fun/Running/GitHub_Running/Running/TrainingPlans/csv/"
home_cpu_path <- "C:/Users/danie/Documents/Running/TrainingPlans/csv/"
file_name <- "CHI_2025_Daniels.csv"

computer_name <- Sys.info()["nodename"]
csv_path <- which_cpu_path(computer_name,work_cpu_path,home_cpu_path)

# Try tidyverse read_csv
csv_file <- paste0(csv_path,file_name)
data <- read_csv(csv_file)

# Date -------------------------------------------------------------------------
race_date <- lubridate::mdy("10/12/2025")
data <- data %>% 
  mutate(Date = race_date - days(Days_Out))

# Avg Pace ---------------------------------------------------------------------
data <- data %>% 
  mutate(Run_time_seconds = hour(Run_Time)*3600 + minute(Run_Time)*60 + second(Run_Time),
         .after = Run_Time) %>% 
  mutate(Avg_Pace = round_hms(as_hms(Run_time_seconds/Run_Dist),digits = 0),.after = Run_time_seconds) %>% 
  mutate(Avg_Pace_Seconds = Run_time_seconds/Run_Dist,.after = Avg_Pace)

# Segment Pace  ----------------------------------------------------------------
data <- data %>% 
  mutate(Seg_time_seconds = hour(Seg_Time)*3600 + minute(Seg_Time)*60 + second(Seg_Time),
         .after = Seg_Dist) %>% 
  mutate(Seg_Pace = round_hms(as_hms(Seg_time_seconds/Seg_Dist),digits = 0),.after = Seg_time_seconds) %>% 
  mutate(Seg_Pace_Seconds = Seg_time_seconds/Seg_Dist,.after = Seg_Pace)

# ID Q days --------------------------------------------------------------------
data <- data %>% 
  mutate(Q_days = ifelse(!is.na(Seg_Time),"Q","E"))

# Define Paces------------------------------------------------------------------
pace_zones <- tibble(
  Zone = c("Recovery", 
           "Easy Aerobic",
           "High Aerobic",
           "Easy",
           "Marathon", 
           "Threshold",
           "Interval", 
           "Repetition"),
  Code = c("Rec",
           "EA",
           "HA",
           "E",
           "M",
           "T",
           "I",
           "R"),
  Mile_Pace_seconds = c((8*60) + 00,
                   (7*60) + 45, 
                   (6*60) + 25,
                   (7*60) + 30,
                   (5*60) + 56,
                   (5*60) + 37,
                   (5*60) + 18,
                   (4*60) + 45))

# Quality Points ---------------------------------------------------------------
# velocity in meters per minute
jd_velo <- function(dist_miles,time_seconds){
  v = ((dist_miles*1609.344)/time_seconds)*60
  return(v)
}

percent_VDOT <- function(velocity,VDOT){
  power_factor = ((0.182258*velocity) + (0.000104*(velocity^2)) - 4.6)/VDOT
  return(power_factor)
}

intensity_points <- function(time_seconds,percent_VDOT){
  points = (time_seconds/60) * jd_points_function(percent_VDOT)
  return(points)
}

# Read points lookup file
points_file <- paste0(csv_path,"JD_points.csv")
jd_points <- read_csv(points_file)
jd_points_function <- approxfun(jd_points$P_VDOT,jd_points$Point_Value)
vdot <- 63.58

data <- data %>% 
  mutate(p_VDOT = percent_VDOT(jd_velo(Run_Dist,Run_time_seconds),vdot))

# Calculate Intensity Points from percent VDOT * minutes function
# this uses the approxfunction
intensity_points <- function(time_seconds,percent_VDOT){
  points <- (time_seconds/60) * jd_points_function(percent_VDOT)
  return(points)
}

data <- data %>%
  mutate(Intensity_Points = intensity_points(Run_time_seconds,p_VDOT))

# Set up planned intensity -----------------------------------------------------
planned_dist_breakdown <- read_csv(paste0(csv_path,"Planned_Distance_breakdown.csv"))
planned_dist_breakdown <- planned_dist_breakdown %>% add_row(Plan = "RACE")

planned_dist_breakdown <- planned_dist_breakdown %>% 
  mutate(E = ifelse(is.na(E),0,E),
         M = ifelse(is.na(M),0,M),
         `T` = ifelse(is.na(`T`),0,`T`),
         I = ifelse(is.na(I),0,I),
         R = ifelse(is.na(R),0,R)) %>% 
  mutate(Planned_Time_Seconds = E*pace_zones$Mile_Pace_seconds[pace_zones$Code == "E"] + 
           M*pace_zones$Mile_Pace_seconds[pace_zones$Code == "M"] + 
           `T`*pace_zones$Mile_Pace_seconds[pace_zones$Code == "T"] + 
           I*pace_zones$Mile_Pace_seconds[pace_zones$Code == "I"] + 
           R*pace_zones$Mile_Pace_seconds[pace_zones$Code == "R"]) %>% 
  mutate(Planned_Dist = E + M + `T` + I + R)

# Planned Intensity
planned_dist_breakdown <- planned_dist_breakdown %>% 
  mutate(Planned_Intensity = intensity_points(Planned_Time_Seconds,percent_VDOT(jd_velo(Planned_Dist,Planned_Time_Seconds),vdot)))

# Add to Data
data <- data %>%
  mutate(Planned_Intensity = planned_dist_breakdown$Planned_Intensity,.before = Intensity_Points)

# Now Set up Timelines ---------------------------------------------------------
day1 <- mdy("6-8-2025")
week_starts <- seq(126,0,-7)

data <- data %>%
  mutate(Run_Dist = ifelse(is.na(Run_Dist), 0, Run_Dist),
         p_VDOT = ifelse(is.na(p_VDOT), 0, p_VDOT),
         Intensity_Points = ifelse(is.na(Intensity_Points), 0, Intensity_Points))

# 7 Day Total Mileage
data <- data %>% 
  mutate(Plan_7day_Total = zoo::rollapply(Plan_Dist,7, sum, na.rm = T, fill = NA,align = "right"),
         Run_7day_Total = zoo::rollapply(Run_Dist,7, sum, na.rm = T, fill = NA,align = "right"))

# 7 Day Total Mileage - Moving average
data <- data %>% 
  mutate(Plan_7day_Avg = zoo::rollapply(Plan_7day_Total,7, mean, na.rm = T, fill = NA,align = "right"),
         Run_7day_Avg = zoo::rollapply(Run_7day_Total,7, mean, na.rm = T, fill = NA,align = "right"))

# 7 Day Total Points
data <- data %>% 
  mutate(Plan_7day_Intensity = zoo::rollapply(Planned_Intensity,7, sum, na.rm = T, fill = NA,align = "right"),
         Run_7day_Intensity = zoo::rollapply(Intensity_Points,7, sum, na.rm = T, fill = NA,align = "right"))

# 7 Day Total Points - Moving Average
data <- data %>% 
  mutate(Plan_7day_Avg_Intensity = zoo::rollapply(Plan_7day_Intensity,7, mean, na.rm = T, fill = NA,align = "right"),
         Run_7day_Avg_Intensity = zoo::rollapply(Run_7day_Intensity,7, mean, na.rm = T, fill = NA,align = "right"))

# Now Visualize ----------------------------------------------------------------
theme_set(theme_bw())

# Run Dist vs Plan Dist
ggplot(data) + 
  geom_abline(slope = 1, intercept = 0)+
  geom_point(aes(x = Plan_Dist, y = Run_Dist)) + 
  scale_x_continuous(breaks = seq(0,25,2), minor_breaks = seq(0,25,1)) + 
  scale_x_continuous(breaks = seq(0,25,2), minor_breaks = seq(0,25,1)) +
  labs(x = "Plan Dist", y = "Run Dist")
  #scale_x_continuous(breaks = seq(0,126,7), minor_breaks = seq(0,126,1), transform = "reverse") + 
  #coord_cartesian(ylim = c(5,22))
  
# 7 day total mileage
ggplot(data) + 
  geom_hline(yintercept = 70)+
  geom_vline(xintercept = week_starts, linetype = "dotted",alpha = 0.5)+
  geom_line(aes(x = Days_Out, y = Plan_7day_Total, color = "Planned")) + 
  geom_line(aes(x = Days_Out, y = Run_7day_Total)) + 
  scale_x_continuous(breaks = seq(0,126,7), minor_breaks = seq(0,126,1), transform = "reverse") + 
  scale_y_continuous(breaks = seq(0,100,10), minor_breaks = seq(0,100,5)) +
  coord_cartesian(ylim = c(0,100))+
  labs(x = "Days Out", y = "Distance(mi)",color = NULL) + 
  scale_color_manual(values = c("Planned" = "lightblue3"))+
  theme(legend.position = "inside",legend.position.inside = c(0.15,0.15))

# 7 day avg mileage
ggplot(data) + 
  geom_hline(yintercept = 70)+
  geom_vline(xintercept = week_starts, linetype = "dotted",alpha = 0.5)+
  geom_line(aes(x = Days_Out, y = Plan_7day_Avg, color = "Planned")) + 
  geom_line(aes(x = Days_Out, y = Run_7day_Avg)) + 
  scale_x_continuous(breaks = seq(0,126,7), minor_breaks = seq(0,126,1), transform = "reverse") + 
  scale_y_continuous(breaks = seq(0,100,10), minor_breaks = seq(0,100,5)) +
  coord_cartesian(ylim = c(0,100))+
  labs(x = "Days Out", y = "Distance(mi)",color = NULL) + 
  scale_color_manual(values = c("Planned" = "lightblue3"))+
  theme(legend.position = "inside",legend.position.inside = c(0.15,0.15))

# 7 Day Intensity Total
ggplot(data) + 
  geom_hline(yintercept = c(50,100,150,200), color = c("green3","orange2","red3","brown4"),linetype = "dashed")+
  geom_vline(xintercept = week_starts, linetype = "dotted",alpha = 0.5)+
  geom_line(aes(x = Days_Out, y = Plan_7day_Intensity, color = "Planned")) + 
  geom_line(aes(x = Days_Out, y = Run_7day_Intensity)) + 
  scale_x_continuous(breaks = seq(0,126,7), minor_breaks = seq(0,126,1), transform = "reverse") + 
  scale_y_continuous(breaks = seq(0,200,25), minor_breaks = seq(0,200,5)) +
  labs(x = "Days Out", y = "Intensit Points",color = NULL) + 
  scale_color_manual(values = c("Planned" = "lightblue3"))+
  theme(legend.position = "inside",legend.position.inside = c(0.15,0.15))

# 7 Day Intensity Avg
ggplot(data) + 
  geom_hline(yintercept = c(50,100,150,200), color = c("green3","orange2","red3","brown4"),linetype = "dashed")+
  geom_vline(xintercept = week_starts, linetype = "dotted",alpha = 0.5)+
  geom_line(aes(x = Days_Out, y = Plan_7day_Avg_Intensity, color = "Planned")) + 
  geom_line(aes(x = Days_Out, y = Run_7day_Avg_Intensity)) + 
  scale_x_continuous(breaks = seq(0,126,7), minor_breaks = seq(0,126,1), transform = "reverse") + 
  scale_y_continuous(breaks = seq(0,200,25), minor_breaks = seq(0,200,5)) +
  labs(x = "Days Out", y = "Intensit Points",color = NULL) + 
  scale_color_manual(values = c("Planned" = "lightblue3"))+
  theme(legend.position = "inside",legend.position.inside = c(0.15,0.15))

# 7 Day Intensity Avg - DELTA
ggplot(data) + 
  geom_vline(xintercept = week_starts, linetype = "dotted",alpha = 0.5)+
  geom_line(aes(x = Days_Out, y = (Run_7day_Avg_Intensity - Plan_7day_Avg_Intensity))) +
  scale_x_continuous(breaks = seq(0,126,7), minor_breaks = seq(0,126,1), transform = "reverse") + 
  scale_y_continuous(breaks = seq(-50,50,5), minor_breaks = seq(-50,50,5)) +
  labs(x = "Days Out", y = "Delta Intensity",color = NULL) + 
  theme(legend.position = "inside",legend.position.inside = c(0.15,0.15))

# Lastly - Power Curves
# Intensity Points vs Time
ggplot(data) + 
  geom_point(aes(x = Run_time_seconds, y = Intensity_Points)) +
  scale_x_continuous(breaks = seq(0,10800,3600)) + 
  scale_y_continuous(breaks = seq(-50,50,5), minor_breaks = seq(-50,50,5)) +
  labs(x = "Days Out", y = "Delta Intensity",color = NULL) + 
  theme(legend.position = "inside",legend.position.inside = c(0.15,0.15))

# Pace vs Time
ggplot(data) + 
  geom_point(aes(x = Run_time_seconds/60, y = Avg_Pace_Seconds/60)) +
  scale_x_continuous(breaks = seq(0,126,7), minor_breaks = seq(0,126,1), transform = "reverse") + 
  scale_y_continuous(breaks = seq(-50,50,5), minor_breaks = seq(-50,50,5)) +
  labs(x = "Days Out", y = "Delta Intensity",color = NULL) + 
  theme(legend.position = "inside",legend.position.inside = c(0.15,0.15))



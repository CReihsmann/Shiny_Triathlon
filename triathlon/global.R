library(tidyverse)
library(shiny)
library(httr)
library(rjson)
library(lubridate)
library(ggridges)
library(plotly)
library(tibbletime)
library(RColorBrewer)

triathlon_data <- read_rds("data/cleaned_data.RDS")

#triathlon_data <- triathlon_data %>% 
#    mutate(age_group = if_else(athlete_age>= 16 & athlete_age<=19, "16-19", 
#                               if_else(athlete_age>= 20 & athlete_age<=24, "20-24", 
#                                       if_else(athlete_age >=  25 & athlete_age<=29, "25-29", 
#                                               if_else(athlete_age >=  30 & athlete_age<=34, "30-34", 
#                                                       if_else(athlete_age >=  35 & athlete_age<=38, "35-38",
#                                                               if_else(athlete_age >=  39 & athlete_age<=42, "39-42", "NA")))))))

average_line <- triathlon_data

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                          

triathlon_data <- triathlon_data %>% 
    filter(event_title != "2014 ITU World Triathlon Cape Town" & event_title != "2015 ITU World Triathlon Cape Town")


limit1 <- as.POSIXct(strptime(c("2000-01-01 01:45"), format = "%Y-%m-%d %H:%M"))
limit2 <- as.POSIXct(strptime(c("2000-01-01 02:10"), format = "%Y-%m-%d %H:%M"))
limit3 <- as.POSIXct(strptime(c("2000-01-01 0:50"), format = "%Y-%m-%d %H:%M"))
limit4 <- as.POSIXct(strptime(c("2000-01-01 1:10"), format = "%Y-%m-%d %H:%M"))

box_limit1 <- as.POSIXct(strptime(c("2000-01-01 01:40"), format = "%Y-%m-%d %H:%M"))
box_limit2 <- as.POSIXct(strptime(c("2000-01-01 02:25"), format = "%Y-%m-%d %H:%M"))
box_limit3 <- as.POSIXct(strptime(c("2000-01-01 00:48"), format = "%Y-%m-%d %H:%M"))
box_limit4 <- as.POSIXct(strptime(c("2000-01-01 01:15"), format = "%Y-%m-%d %H:%M"))
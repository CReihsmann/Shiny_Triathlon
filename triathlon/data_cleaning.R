library(tidyverse)
library(shiny)
library(httr)
library(rjson)
library(lubridate)
library(ggridges)
library(plotly)
library(tibbletime)
library(RColorBrewer)


triathlon_data <- read_rds("data/triathlon_data.RDS")

triathlon_data <- triathlon_data %>% 
    mutate(athlete_yob = as.numeric(athlete_yob))

triathlon_data <- ungroup(triathlon_data)

triathlon_data <- triathlon_data %>% 
    mutate(temp_air = (temp_air*(9/5)+32),
           temp_water = (temp_water*(9/5)+32))

date_constant <- as.POSIXct("2000-01-01")
triathlon_data <- triathlon_data %>% 
    mutate(total_time = hms::as_hms(total_time),
           swim_time = hms::as_hms(swim_time),
           bike_time = hms::as_hms(bike_time),
           run_time = hms::as_hms(run_time)) %>% 
    mutate(total_time = date_constant + total_time,
           swim_time = date_constant + swim_time,
           bike_time = date_constant + bike_time,
           run_time = date_constant + run_time) 


sprint <- triathlon_data %>%  
    filter(cat_name == "Standard") %>% 
    arrange(total_time)%>% 
    as_tbl_time(total_time) %>% 
    filter_time("2000-01-01 00:30:00" ~ "2000-01-01 01:10:00")  

mislabled_events <- unique(sprint$event_title)
mislabled_events <- as.list(mislabled_events)

triathlon_data %>% 
    filter(event_title == mislabled_events)

for (i in mislabled_events) {
    triathlon_data <- triathlon_data %>%  
        mutate(cat_name = replace(cat_name, event_title == i, "Sprint"))
}

standard <- triathlon_data %>%  
    filter(cat_name == "Sprint")%>% 
    arrange(total_time) %>% 
    as_tbl_time(total_time) %>% 
    filter_time("2000-01-01 01:40:00" ~ "2000-01-01 02:20:00")  

mislabled_events_2 <- unique(standard$event_title)
mislabled_events_2 <- as.list(mislabled_events_2)

triathlon_data %>% 
    filter(event_title == mislabled_events_2)

for (i in mislabled_events_2) {
    triathlon_data <- triathlon_data %>%  
        mutate(cat_name = replace(cat_name, event_title == i, "Standard"))
}

write_csv2(triathlon_data, "data/cleaned_data.csv")
write_rds(triathlon_data, "data/cleaned_data.RDS")

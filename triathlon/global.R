library(tidyverse)
library(shiny)
library(httr)
library(rjson)
library(lubridate)
library(ggridges)
library(plotly)
library(tibbletime)

triathlon_data <- read_rds("data/triathlon_data.RDS")

triathlon_data <- triathlon_data %>% 
    mutate(athlete_yob = as.numeric(athlete_yob))

triathlon_data <- ungroup(triathlon_data)

triathlon_data <- triathlon_data %>% 
    mutate(total_time = hms::as_hms(total_time))

sprint <- triathlon_data %>%  
    filter(cat_name == "Standard")%>% 
    arrange(total_time) %>% 
    as_tbl_time(total_time) %>% 
    filter_time("00:30:00" ~ "01:10:00")  

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
    filter_time("01:40:00" ~ "02:20:00")  

mislabled_events_2 <- unique(standard$event_title)
mislabled_events_2 <- as.list(mislabled_events_2)

triathlon_data %>% 
    filter(event_title == mislabled_events_2)

for (i in mislabled_events_2) {
    triathlon_data <- triathlon_data %>%  
        mutate(cat_name = replace(cat_name, event_title == i, "Standard"))
}


                          
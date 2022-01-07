library(tidyverse)
library(plotly)
library(ggridges)

triathlon_data <- read_csv("../data/cleaned_data.csv")

triathlon_data %>% 
    filter(athlete_age >= 20 , athlete_age <= 25)

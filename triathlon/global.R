library(tidyverse)
library(plotly)
library(ggridges)

triathlon_data <- read_csv("../data/cleaned_data.csv")

min(triathlon_data$athlete_age)

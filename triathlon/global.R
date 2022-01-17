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


average_line <- triathlon_data
                          



limit1 <- as.POSIXct(strptime(c("2000-01-01 01:45"), format = "%Y-%m-%d %H:%M"))
limit2 <- as.POSIXct(strptime(c("2000-01-01 02:10"), format = "%Y-%m-%d %H:%M"))
limit3 <- as.POSIXct(strptime(c("2000-01-01 0:50"), format = "%Y-%m-%d %H:%M"))
limit4 <- as.POSIXct(strptime(c("2000-01-01 1:10"), format = "%Y-%m-%d %H:%M"))

box_limit1 <- as.POSIXct(strptime(c("2000-01-01 01:40"), format = "%Y-%m-%d %H:%M"))
box_limit2 <- as.POSIXct(strptime(c("2000-01-01 02:25"), format = "%Y-%m-%d %H:%M"))
box_limit3 <- as.POSIXct(strptime(c("2000-01-01 00:48"), format = "%Y-%m-%d %H:%M"))
box_limit4 <- as.POSIXct(strptime(c("2000-01-01 01:15"), format = "%Y-%m-%d %H:%M"))


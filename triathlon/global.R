library(tidyverse)
library(shiny)
library(httr)
library(rjson)
library(lubridate)
library(ggridges)
library(plotly)
library(tibbletime)
library(shinythemes)
library(markdown)
library(maps)

triathlon_data <- read_rds("data/cleaned_data.RDS")


average_line <- triathlon_data
                          

limit1_m_oly <- as.POSIXct(strptime(c("2000-01-01 01:44"), format = "%Y-%m-%d %H:%M"))
limit2_m_oly <- as.POSIXct(strptime(c("2000-01-01 02:05"), format = "%Y-%m-%d %H:%M"))
limit1_f_oly <- as.POSIXct(strptime(c("2000-01-01 01:52"), format = "%Y-%m-%d %H:%M"))
limit2_f_oly <- as.POSIXct(strptime(c("2000-01-01 02:15"), format = "%Y-%m-%d %H:%M"))

limit1_m_spr <- as.POSIXct(strptime(c("2000-01-01 00:51"), format = "%Y-%m-%d %H:%M"))
limit2_m_spr <- as.POSIXct(strptime(c("2000-01-01 01:03"), format = "%Y-%m-%d %H:%M"))
limit1_f_spr <- as.POSIXct(strptime(c("2000-01-01 00:55"), format = "%Y-%m-%d %H:%M"))
limit2_f_spr <- as.POSIXct(strptime(c("2000-01-01 01:14"), format = "%Y-%m-%d %H:%M"))


box_limit1 <- as.POSIXct(strptime(c("2000-01-01 01:40"), format = "%Y-%m-%d %H:%M"))
box_limit2 <- as.POSIXct(strptime(c("2000-01-01 02:25"), format = "%Y-%m-%d %H:%M"))
box_limit3 <- as.POSIXct(strptime(c("2000-01-01 00:48"), format = "%Y-%m-%d %H:%M"))
box_limit4 <- as.POSIXct(strptime(c("2000-01-01 01:15"), format = "%Y-%m-%d %H:%M"))


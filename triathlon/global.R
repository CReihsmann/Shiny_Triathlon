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
library(sf)
library(leaflet)

triathlon_data <- read_rds("data/cleaned_data.RDS")

average_line <- triathlon_data

#------choropleth data sets
top_overall_perc <- read_rds("data/top_overall_perc.rds")
top_swim_perc <- read_rds("data/top_swim_perc.rds")
top_bike_perc <- read_rds("data/top_bike_perc.rds")
top_run_perc <- read_rds("data/top_run_perc.rds")

bottom_overall_perc <- read_rds("data/bottom_overall_perc.rds")
bottom_swim_perc <- read_rds("data/bottom_swim_perc.rds")
bottom_bike_perc <- read_rds("data/bottom_bike_perc.rds")
bottom_run_perc <- read_rds("data/bottom_run_perc.rds")

#------variables for top 10% choropleth
mytext_top_overall <- paste(
    "Country: ", top_overall_perc$NAME, "<br/>",
    "Total Competitors: ", top_overall_perc$numb_per_country_total,"<br/>",
    "In Top 10%: ", round(top_overall_perc$percent_total, digits = 2), "%", "<br/>") %>% 
    lapply(htmltools::HTML)

mytext_top_swim <- paste(
    "Country: ", top_swim_perc$NAME, "<br/>",
    "Total Competitors: ", top_swim_perc$numb_per_country_total,"<br/>",
    "In Top 10%: ", round(top_swim_perc$percent_total, digits = 2), "%", "<br/>") %>% 
    lapply(htmltools::HTML)

mytext_top_bike <- paste(
    "Country: ", top_bike_perc$NAME, "<br/>",
    "Total Competitors: ", top_bike_perc$numb_per_country_total,"<br/>",
    "In Top 10%: ", round(top_bike_perc$percent_total, digits = 2), "%", "<br/>") %>% 
    lapply(htmltools::HTML)

mytext_top_run <- paste(
    "Country: ", top_run_perc$NAME, "<br/>",
    "Total Competitors: ", top_run_perc$numb_per_country_total,"<br/>",
    "In Top 10%: ", round(top_run_perc$percent_total, digits = 2), "%", "<br/>") %>% 
    lapply(htmltools::HTML)

mybins_top <- c(0,1,3,6,10,15,20,30,60)

#------variables for bottom 10% choropleth
mytext_bottom_overall <- paste(
    "Country: ", bottom_overall_perc$NAME, "<br/>",
    "Total Competitors: ", bottom_overall_perc$numb_per_country_total,"<br/>",
    "In Bottom 10%: ", round(bottom_overall_perc$percent_total, digits = 2), "%", "<br/>") %>% 
    lapply(htmltools::HTML)

mytext_bottom_swim <- paste(
    "Country: ", bottom_swim_perc$NAME, "<br/>",
    "Total Competitors: ", bottom_swim_perc$numb_per_country_total,"<br/>",
    "In Bottom 10%: ", round(bottom_swim_perc$percent_total, digits = 2), "%", "<br/>") %>% 
    lapply(htmltools::HTML)

mytext_bottom_bike <- paste(
    "Country: ", bottom_bike_perc$NAME, "<br/>",
    "Total Competitors: ", bottom_bike_perc$numb_per_country_total,"<br/>",
    "In Bottom 10%: ", round(bottom_bike_perc$percent_total, digits = 2), "%", "<br/>") %>% 
    lapply(htmltools::HTML)

mytext_bottom_run <- paste(
    "Country: ", bottom_run_perc$NAME, "<br/>",
    "Total Competitors: ", bottom_run_perc$numb_per_country_total,"<br/>",
    "In Bottom 10%: ", round(bottom_run_perc$percent_total, digits = 2), "%", "<br/>") %>% 
    lapply(htmltools::HTML)

mybins_bottom <- c(0,1,3,6,10,15,20,30,60,100)

#------choropleth pallets_tops

mypalette_overall_top <- colorBin(palette="Greens", domain=top_overall_perc$percent_total, na.color="transparent", bins=mybins_top)
mypalette_swim_top <- colorBin(palette="Greens", domain=top_swim_perc$swim_percent_total, na.color="transparent", bins=mybins_top)
mypalette_bike_top <- colorBin(palette="Greens", domain=top_bike_perc$bike_percent_total, na.color="transparent", bins=mybins_top)
mypalette_run_top <- colorBin(palette="Greens", domain=top_run_perc$run_percent_total, na.color="transparent", bins=mybins_top)

#------choropleth pallets_bottoms

mypalette_overall_bottom <- colorBin(palette="Reds", domain=bottom_overall_perc$percent_total, na.color="transparent", bins=mybins_bottom)
mypalette_swim_bottom <- colorBin(palette="Reds", domain=bottom_swim_perc$swim_percent_total, na.color="transparent", bins=mybins_bottom)
mypalette_bike_bottom <- colorBin(palette="Reds", domain=bottom_bike_perc$bike_percent_total, na.color="transparent", bins=mybins_bottom)
mypalette_run_bottom <- colorBin(palette="Reds", domain=bottom_run_perc$run_percent_total, na.color="transparent", bins=mybins_bottom)




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


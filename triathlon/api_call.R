library(tidyverse)
library(shiny)
library(httr)
library(rjson)
library(lubridate)

#API call for unique event and program ids -------------------

#pulls api key from data folder
result <- fromJSON(file = "data/data.json")
api_key <- result[["api_key"]]

#makes API request for all unique events
url_2 = "https://api.triathlon.org/v1/statistics/results?analysis=count_unique&target_property=event.name&group_by=event.name|event.id|program.id|program.name"

response_event_list <- GET(url_2,
                           add_headers(.headers = 
                                           c(Accept= "application/json",
                                             apikey = api_key)
                           )
)

#parsing api call
event_list <- content(response_event_list, as = "parsed")

#pulls out embedded list and converts to tibble
indiv_events <- do.call(rbind, event_list$data[[1]])
indiv_events <- as_tibble(indiv_events)

#makes list of key/value pairs of event.id and program.id 
key_value <- indiv_events %>% 
    pivot_longer(c(program.id, event.id), names_to = "key", values_to = "value")

event_id <- indiv_events$event.id
event_id <- as.character(event_id)
program_id <- indiv_events$program.id
program_id <- as.character(program_id)
names(event_id) <- program_id



#-------------function for parsing api call in next step-------------------
    
data_parse <- function(result) {
    event <- content(result, as = "parsed")
    
    indiv <- do.call(rbind, event$data[[7]])
    indiv <- as_tibble(indiv)
    
    athlete_results <- indiv %>% 
        subset(select = -c(athlete_edit_date, 
                           athlete_profile_image, 
                           athlete_listing, 
                           athlete_flag, 
                           athlete_api_listing, 
                           athlete_categories,
                           validated,
                           athlete_slug,
                           start_num)) %>% 
        filter(position != "DNF") %>%
        unnest(splits) %>% 
        group_by(athlete_id)%>% 
        mutate(col=seq_along(athlete_id)) %>% 
        spread(key = col, value=splits) %>% 
        rename(swim_time = "1", 
               t1_time = "2", 
               bike_time = "3", 
               t2_time = "4", 
               run_time = "5") %>% 
        mutate(prog_id = event$data$prog_id, 
               event_id = event$data$event_id, 
               prog_name = event$data$prog_name, 
               prog_date = event$data$prog_date, 
               prog_time = event$data$prog_time, 
               prog_notes = event$data$prog_notes,
               event_title = event$data$event$event_title,
               event_venue = event$data$event$event_venue,
               event_country = event$data$event$event_country,
               event_latitude = event$data$event$event_latitude,
               event_longitude = event$data$event$event_longitude,
               event_date = event$data$event$event_date,
               event_country_isoa2 = event$data$event$event_country_isoa2,
               event_country_noc = event$data$event$event_country_noc,
               event_region_id = event$data$event$event_region_id,
               event_country_id = event$data$event$event_country_id,
               event_region_name = event$data$event$event_region_name,
               cat_name = event$data$event$event_specifications[[2]][1],
               temp_water = event$data$meta$temperature_water,
               temp_air = event$data$meta$temperature_air,
               wetsuit = event$data$meta$wetsuit,
               athlete_id = as.numeric(athlete_id),
               athlete_country_id = as.numeric(athlete_country_id),
               result_id = as.numeric(result_id),
               position = as.numeric(position),
               athlete_title = as.character(athlete_title),
               athlete_first = as.character(athlete_first),
               athlete_last = as.character(athlete_last),
               athlete_gender = as.character(athlete_gender),
               athlete_yob = as.character(athlete_yob),
               athlete_noc = as.character(athlete_noc),
               athlete_country_name = as.character(athlete_country_name),
               athlete_country_isoa2 = as.character(athlete_country_isoa2),
               total_time = as.character(total_time),
               swim_time = as.character(swim_time),
               t1_time = as.character(t1_time),
               bike_time = as.character(bike_time),
               t2_time = as.character(t2_time),
               run_time = as.character(run_time),
               cat_name = as.character(cat_name)) %>% 
        filter(!is.na(position))
    
    athlete_results <- athlete_results %>% 
        filter(swim_time != "00:00:00",
               bike_time != "00:00:00",
               run_time != "00:00:00")
    
    athlete_results <- athlete_results %>% 
        arrange(hms::as_hms(swim_time)) %>% 
        add_column(swim_position = 1:nrow(athlete_results), .after = "swim_time") %>% 
        arrange(hms::as_hms(bike_time)) %>% 
        add_column(bike_position = 1:nrow(athlete_results), .after = "bike_time") %>% 
        arrange(hms::as_hms(run_time)) %>% 
        add_column(run_position = 1:nrow(athlete_results), .after = "run_time")
    
    
    athlete_results <- athlete_results %>% 
        mutate(prog_year = year(prog_date),
               prog_month = month(prog_date), 
               athlete_age = prog_year - as.numeric(athlete_yob),
               position_perc = position/nrow(athlete_results)*100,
               swim_position_perc = swim_position/nrow(athlete_results)*100,
               bike_position_perc = bike_position/nrow(athlete_results)*100,
               run_position_perc = run_position/nrow(athlete_results)*100)#,
              # swim_time = hms::as_hms(swim_time),
               #bike_time = hms::as_hms(bike_time),
               #run_time = hms::as_hms(run_time),
               #total_time = hms::as.hms(total_time))
    
}    


#-------------API call for event data -------------------   

url = 'https://api.triathlon.org/'

params = list(
    'event_id' = '90162',
    'prog_id' = '270564'
)


response_header <- GET(modify_url(url, path = paste("v1/","events/", 
                                             params["event_id"], 
                                             "/programs/", 
                                             params["prog_id"], 
                                             "/results",
                                             sep = "")), 
                add_headers(.headers = 
                                c(Accept= "application/json",
                                  apikey = api_key)
                )
)

athlete_results_header <- data_parse(response_header)
all_results <- athlete_results_header[0,]

#ol_names <- colnames(athlete_results_header)

#all_results <- setNames(rep("",length(col_names)), col_names)
#all_results <- as_tibble(t(all_results))[0, ]

for(key in names(event_id)){
    value <- event_id[key]
    
    response <- GET(modify_url(url, path = paste("v1/","events/", 
                                                 value, 
                                                 "/programs/", 
                                                 key, 
                                                 "/results",
                                                 sep = "")), 
                    add_headers(.headers = 
                                    c(Accept= "application/json",
                                      apikey = api_key)
                    )
    )
    single_event <- data_parse(response)
    
   all_results <- bind_rows(all_results, single_event)
}

test <- all_results %>% 
    filter(prog_id == 4818)

    mutate(swim_time_zscore = as.numeric((swim_time - mean(swim_time, na.rm = T)))/sd(as.double(swim_time)))

write_csv2(all_results, "data/triathlon_data.csv")
write_rds(all_results, "data/triathlon_data.RDS")

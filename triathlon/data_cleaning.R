library(tidyverse)
library(shiny)
library(httr)
library(rjson)
library(lubridate)
library(ggridges)
library(plotly)
library(tibbletime)
library(RColorBrewer)


triathlon_called <- read_rds("data/triathlon_data.RDS")

triathlon_called <- ungroup(triathlon_called)

triathlon_called <- triathlon_called %>% 
    mutate(athlete_yob = as.numeric(athlete_yob),
           temp_air = (temp_air*(9/5)+32),
           temp_water = (temp_water*(9/5)+32),
           total_time = hms::as_hms(total_time),
           swim_time = hms::as_hms(swim_time),
           bike_time = hms::as_hms(bike_time),
           run_time = hms::as_hms(run_time))

event_countries <- as.list(unique(triathlon_called$event_country))
triathlon_data <- triathlon_called[0, ]
triathlon_oly_m <- triathlon_called[0, ]
triathlon_oly_f <- triathlon_called[0, ]
triathlon_spr_m <- triathlon_called[0, ]
triathlon_spr_f <- triathlon_called[0, ]
for (i in event_countries) {
    country <- triathlon_called %>% 
        filter(event_country == i, athlete_gender == "male", cat_name == "Standard") %>% 
        mutate(znorm_total = as.numeric((total_time - mean(total_time))/sd(total_time)),
               znorm_swim = as.numeric((swim_time - mean(swim_time))/sd(swim_time)),
               znorm_bike = as.numeric((bike_time - mean(bike_time))/sd(bike_time)),
               znorm_run = as.numeric((run_time - mean(run_time))/sd(run_time)))
    triathlon_oly_m <- bind_rows(triathlon_oly_m, country)
}
for (i in event_countries) {
    country <- triathlon_called %>% 
        filter(event_country == i, athlete_gender == "female", cat_name == "Standard") %>% 
        mutate(znorm_total = as.numeric((total_time - mean(total_time))/sd(total_time)),
               znorm_swim = as.numeric((swim_time - mean(swim_time))/sd(swim_time)),
               znorm_bike = as.numeric((bike_time - mean(bike_time))/sd(bike_time)),
               znorm_run = as.numeric((run_time - mean(run_time))/sd(run_time)))
    triathlon_oly_f <- bind_rows(triathlon_oly_f, country)
}
for (i in event_countries) {
    country <- triathlon_called %>% 
        filter(event_country == i, athlete_gender == "male", cat_name == "Sprint") %>% 
        mutate(znorm_total = as.numeric((total_time - mean(total_time))/sd(total_time)),
               znorm_swim = as.numeric((swim_time - mean(swim_time))/sd(swim_time)),
               znorm_bike = as.numeric((bike_time - mean(bike_time))/sd(bike_time)),
               znorm_run = as.numeric((run_time - mean(run_time))/sd(run_time)))
    triathlon_spr_m <- bind_rows(triathlon_spr_m, country)
}
for (i in event_countries) {
    country <- triathlon_called %>% 
        filter(event_country == i, athlete_gender == "female", cat_name == "Sprint") %>% 
        mutate(znorm_total = as.numeric((total_time - mean(total_time))/sd(total_time)),
               znorm_swim = as.numeric((swim_time - mean(swim_time))/sd(swim_time)),
               znorm_bike = as.numeric((bike_time - mean(bike_time))/sd(bike_time)),
               znorm_run = as.numeric((run_time - mean(run_time))/sd(run_time)))
    triathlon_spr_f <- bind_rows(triathlon_spr_f, country)
}

triathlon_data <- bind_rows(triathlon_oly_m, triathlon_oly_f, triathlon_spr_m, triathlon_spr_f)
    

date_constant <- as.POSIXct("2000-01-01")
triathlon_data <- triathlon_data %>% 
    filter(event_title != "2014 ITU World Triathlon Cape Town" & event_title != "2015 ITU World Triathlon Cape Town") %>%  
    mutate(total_time = date_constant + total_time,
           swim_time = date_constant + swim_time,
           bike_time = date_constant + bike_time,
           run_time = date_constant + run_time) %>% 
    mutate(age_group = if_else(athlete_age>= 16 & athlete_age<=19, "16-19", 
                               if_else(athlete_age>= 20 & athlete_age<=24, "20-24", 
                                       if_else(athlete_age >=  25 & athlete_age<=29, "25-29", 
                                               if_else(athlete_age >=  30 & athlete_age<=34, "30-34", 
                                                       if_else(athlete_age >=  35 & athlete_age<=38, "35-38",
                                                               if_else(athlete_age >=  39 & athlete_age<=42, "39-42", "NA"))))))) 



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

country_codes <- c("ABW",	"AFG",	"AGO",	"AIA",	"ALA",	"ALB",	"AND",	"ARE",	"ARG",	"ARM",
                   "ASM",	"ATA",	"ATF",	"ATG",	"AUS",	"AUT",	"AZE",	"BDI",	"BEL",	"BEN",	
                   "BES",	"BFA",	"BGD",	"BGR",	"BHR",	"BHS",	"BIH",	"BLM",	"BLR",	"BLZ",	
                   "BMU",	"BOL",	"BRA",	"BRB",	"BRN",	"BTN",	"BVT",	"BWA",	"CAF",	"CAN",	
                   "CCK",	"CHE",	"CHL",	"CHN",	"CIV",	"CMR",	"COD",	"COG",	"COK",	"COL",	
                   "COM",	"CPV",	"CRI",	"CUB",	"CUW",	"CXR",	"CYM",	"CYP",	"CZE",	"DEU",	
                   "DJI",	"DMA",	"DNK",	"DOM",	"DZA",	"ECU",	"EGY",	"ERI",	"ESH",	"ESP",	
                   "EST",	"ETH",	"FIN",	"FJI",	"FLK",	"FRA",	"FRO",	"FSM",	"GAB",	"GBR",	
                   "GEO",	"GGY",	"GHA",	"GIB",	"GIN",	"GLP",	"GMB",	"GNB",	"GNQ",	"GRC",	
                   "GRD",	"GRL",	"GTM",	"GUF",	"GUM",	"GUY",	"HKG",	"HMD",	"HND",	"HRV",	
                   "HTI",	"HUN",	"IDN",	"IMN",	"IND",	"IOT",	"IRL",	"IRN",	"IRQ",	"ISL",	
                   "ISR",	"ITA",	"JAM",	"JEY",	"JOR",	"JPN",	"KAZ",	"KEN",	"KGZ",	"KHM",	
                   "KIR",	"KNA",	"KOR",	"KWT",	"LAO",	"LBN",	"LBR",	"LBY",	"LCA",	"LIE",	
                   "LKA",	"LSO",	"LTU",	"LUX",	"LVA",	"MAC",	"MAF",	"MAR",	"MCO",	"MDA",	
                   "MDG",	"MDV",	"MEX",	"MHL",	"MKD",	"MLI",	"MLT",	"MMR",	"MNE",	"MNG",	
                   "MNP",	"MOZ",	"MRT",	"MSR",	"MTQ",	"MUS",	"MWI",	"MYS",	"MYT",	"NAM",	
                   "NCL",	"NER",	"NFK",	"NGA",	"NIC",	"NIU",	"NLD",	"NOR",	"NPL",	"NRU",	
                   "NZL",	"OMN",	"PAK",	"PAN",	"PCN",	"PER",	"PHL",	"PLW",	"PNG",	"POL",	
                   "PRI",	"PRK",	"PRT",	"PRY",	"PSE",	"PYF",	"QAT",	"REU",	"ROU",	"RUS",	
                   "RWA",	"SAU",	"SDN",	"SEN",	"SGP",	"SGS",	"SHN",	"SJM",	"SLB",	"SLE",	
                   "SLV",	"SMR",	"SOM",	"SPM",	"SRB",	"SSD",	"STP",	"SUR",	"SVK",	"SVN",	
                   "SWE",	"SWZ",	"SXM",	"SYC",	"SYR",	"TCA",	"TCD",	"TGO",	"THA",	"TJK",	
                   "TKL",	"TKM",	"TLS",	"TON",	"TTO",	"TUN",	"TUR",	"TUV",	"TWN",	"TZA",	
                   "UGA",	"UKR",	"UMI",	"URY",	"USA",	"UZB",	"VAT",	"VCT",	"VEN",	"VGB",	
                   "VIR",	"VNM",	"VUT",	"WLF",	"WSM",	"YEM",	"ZAF",	"ZMB",	"ZWE")

countries <- c("Aruba",	"Afghanistan",	"Angola",	"Anguilla",	"Åland Islands",	"Albania",	
               "Andorra",	"United Arab Emirates",	"Argentina",	"Armenia",	"American Samoa",
               "Antarctica",	"French Southern Territories",	"Antigua and Barbuda",	"Australia",
               "Austria",	"Azerbaijan",	"Burundi",	"Belgium",	"Benin",
               "Bonaire, Sint Eustatius and Saba",	"Burkina Faso",	"Bangladesh",	
               "Bulgaria",	"Bahrain",	"Bahamas",	"Bosnia and Herzegovina",	"Saint Barthélemy",
               "Belarus",	"Belize",	"Bermuda",	"Bolivia (Plurinational State of)",	"Brazil",
               "Barbados",	"Brunei Darussalam",	"Bhutan",	"Bouvet Island",	"Botswana",	
               "Central African Republic",	"Canada",	"Cocos (Keeling) Islands",	"Switzerland",
               "Chile",	"China",	"Côte d'Ivoire",	"Cameroon",
               "Congo, Democratic Republic of the",	"Congo",	"Cook Islands",	"Colombia",
               "Comoros",	"Cabo Verde",	"Costa Rica",	"Cuba",	"Curaçao",	"Christmas Island",
               "Cayman Islands",	"Cyprus",	"Czech Republic",	"Germany",	"Djibouti",	"Dominica",
               "Denmark",	"Dominican Republic",	"Algeria",	"Ecuador",	"Egypt",	"Eritrea",
               "Western Sahara",	"Spain",	"Estonia",	"Ethiopia",	"Finland",	"Fiji",
               "Falkland Islands (Malvinas)",	"France",	"Faroe Islands",
               "Micronesia (Federated States of)",	"Gabon",	"Great Britain",	
               "Georgia",	"Guernsey",	"Ghana",	"Gibraltar",	"Guinea",	"Guadeloupe",	"Gambia",	
               "Guinea-Bissau",	"Equatorial Guinea",	"Greece",	"Grenada",	"Greenland",	"Guatemala",	
               "French Guiana",	"Guam",	"Guyana",	"Hong Kong China",	"Heard Island and McDonald Islands",	
               "Honduras",	"Croatia",	"Haiti",	"Hungary",	"Indonesia",	"Isle of Man",	"India",	
               "British Indian Ocean Territory",	"Ireland",	"Iran (Islamic Republic of)",	"Iraq",	"Iceland",	
               "Israel",	"Italy",	"Jamaica",	"Jersey",	"Jordan",	"Japan",	"Kazakhstan",	"Kenya",	
               "Kyrgyzstan",	"Cambodia",	"Kiribati",	"Saint Kitts and Nevis",	"Korea, South",	"Kuwait",
               "Lao People's Democratic Republic",	"Lebanon",	"Liberia",	"Libya",	"Saint Lucia",	
               "Liechtenstein",	"Sri Lanka",	"Lesotho",	"Lithuania",	"Luxembourg",	"Latvia",	
               "Macao China",	"Saint Martin (French part)",	"Morocco",	"Monaco",	"Moldova, Republic of",	
               "Madagascar",	"Maldives",	"Mexico",	"Marshall Islands",	"North Macedonia",	"Mali",	"Malta",	
               "Myanmar",	"Montenegro",	"Mongolia",	"Northern Mariana Islands",	"Mozambique",	"Mauritania",
               "Montserrat",	"Martinique",	"Mauritius",	"Malawi",	"Malaysia",	"Mayotte",	"Namibia",	
               "New Caledonia",	"Niger",	"Norfolk Island",	"Nigeria",	"Nicaragua",	"Niue",	"Netherlands",
               "Norway",	"Nepal",	"Nauru",	"New Zealand",	"Oman",	"Pakistan",	"Panama",	"Pitcairn",	
               "Peru",	"Philippines",	"Palau",	"Papua New Guinea",	"Poland",	"Puerto Rico",	
               "Korea (Democratic People's Republic of)",	"Portugal",	"Paraguay",	"Palestine, State of",	
               "French Polynesia",	"Qatar",	"Réunion",	"Romania",	"Russia",	"Rwanda",	
               "Saudi Arabia",	"Sudan",	"Senegal",	"Singapore",	
               "South Georgia and the South Sandwich Islands",	"Saint Helena, Ascension and Tristan da Cunha",	
               "Svalbard and Jan Mayen",	"Solomon Islands",	"Sierra Leone",	"El Salvador",	"San Marino",	
               "Somalia",	"Saint Pierre and Miquelon",	"Serbia",	"South Sudan",	"Sao Tome and Principe",
               "Suriname",	"Slovakia",	"Slovenia",	"Sweden",	"Eswatini",	"Sint Maarten (Dutch part)",	
               "Seychelles",	"Syrian Arab Republic",	"Turks and Caicos Islands",	"Chad",	"Togo",	"Thailand",	
               "Tajikistan",	"Tokelau",	"Turkmenistan",	"Timor-Leste",	"Tonga",	"Trinidad and Tobago",	
               "Tunisia",	"Turkey",	"Tuvalu",	"Chinese Taipei",	"Tanzania, United Republic of",	"Uganda",	
               "Ukraine",	"United States Minor Outlying Islands",	"Uruguay",	"United States",	
               "Uzbekistan",	"Holy See",	"Saint Vincent and the Grenadines",	"Venezuela (Bolivarian Republic of)",
               "Virgin Islands (British)",	"Virgin Islands (U.S.)",	"Viet Nam",	"Vanuatu",	"Wallis and Futuna",	
               "Samoa",	"Yemen",	"South Africa",	"Zambia",	"Zimbabwe"
)

country_tibble <- tibble(FIPS = country_codes,
                         athlete_country_name = countries)
triathlon_data <- left_join(triathlon_data, country_tibble, by = "athlete_country_name")

triathlon_data <- triathlon_data %>% 
    select(-c(athlete_id, athlete_title,athlete_first,athlete_last, athlete_noc,
              athlete_country_isoa2,result_id, prog_notes, prog_name, prog_time,
              event_country_isoa2,event_country_id,event_region_name, event_country_noc, 
              prog_date, event_date, athlete_yob, event_venue))

write_csv2(triathlon_data, "data/cleaned_data.csv")
write_rds(triathlon_data, "data/cleaned_data.RDS")

#--------------choropleth data sets--------------

library(sf)
library(leaflet)

#------geometry file
world_map <- st_read("data/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp")
world_map <- world_map %>% 
    select(-c(FIPS, ISO2))

#------variables 

all_perc <- triathlon_data %>% 
    count(FIPS, name = "numb_per_country_total")

#------overall
#top
filtered_top_overall <- triathlon_data %>%
    filter(position_perc <= 10) %>% 
    count(FIPS, name = "numb_per_country_filtered")


top_overall_perc <- left_join(filtered_top_overall, all_perc, by = "FIPS")
top_overall_perc <- right_join(world_map, top_overall_perc, by = c("ISO3" = "FIPS"))

top_overall_perc <- top_overall_perc %>% 
    mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)

write_rds(top_overall_perc, "data/top_overall_perc.rds")

#bottom
filtered_bottom_overall <- triathlon_data %>%
    filter(position_perc >= 90) %>% 
    count(FIPS, name = "numb_per_country_filtered")


bottom_overall_perc <- left_join(filtered_bottom_overall, all_perc, by = "FIPS")
bottom_overall_perc <- right_join(world_map, bottom_overall_perc, by = c("ISO3" = "FIPS"))

bottom_overall_perc <- bottom_overall_perc %>% 
    mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)

write_rds(bottom_overall_perc, "data/bottom_overall_perc.rds")

#------swim
#top
filtered_top_swim <- triathlon_data %>%
    filter(swim_position_perc <= 10) %>% 
    count(FIPS, name = "numb_per_country_filtered")

top_swim_perc <- left_join(filtered_top_swim, all_perc, by = "FIPS")
top_swim_perc <- right_join(world_map, top_swim_perc, by = c("ISO3" = "FIPS"))

top_swim_perc <- top_swim_perc %>% 
    mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)

write_rds(top_swim_perc, "data/top_swim_perc.rds")

#bottom
filtered_bottom_swim <- triathlon_data %>%
    filter(swim_position_perc >= 90) %>% 
    count(FIPS, name = "numb_per_country_filtered")

bottom_swim_perc <- left_join(filtered_bottom_swim, all_perc, by = "FIPS")
bottom_swim_perc <- right_join(world_map, bottom_swim_perc, by = c("ISO3" = "FIPS"))

bottom_swim_perc <- bottom_swim_perc %>% 
    mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)

write_rds(bottom_swim_perc, "data/bottom_swim_perc.rds")

#------bike
#top
filtered_top_bike <- triathlon_data %>%
    filter(bike_position_perc <= 10) %>% 
    count(FIPS, name = "numb_per_country_filtered")

top_bike_perc <- left_join(filtered_top_bike, all_perc, by = "FIPS")
top_bike_perc <- right_join(world_map, top_bike_perc, by = c("ISO3" = "FIPS"))

top_bike_perc <- top_bike_perc %>% 
    mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)

write_rds(top_bike_perc, "data/top_bike_perc.rds")

#bottom
filtered_bottom_bike <- triathlon_data %>%
    filter(bike_position_perc >= 90) %>% 
    count(FIPS, name = "numb_per_country_filtered")

bottom_bike_perc <- left_join(filtered_bottom_bike, all_perc, by = "FIPS")
bottom_bike_perc <- right_join(world_map, bottom_bike_perc, by = c("ISO3" = "FIPS"))

bottom_bike_perc <- bottom_bike_perc %>% 
    mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)

write_rds(bottom_bike_perc, "data/bottom_bike_perc.rds")

#------run
#top
filtered_top_run <- triathlon_data %>%
    filter(run_position_perc <= 10) %>% 
    count(FIPS, name = "numb_per_country_filtered")

top_run_perc <- left_join(filtered_top_run, all_perc, by = "FIPS")
top_run_perc <- right_join(world_map, top_run_perc, by = c("ISO3" = "FIPS"))

top_run_perc <- top_run_perc %>% 
    mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)

write_rds(top_run_perc, "data/top_run_perc.rds")

#bottom
filtered_bottom_run <- triathlon_data %>%
    filter(run_position_perc >= 90) %>% 
    count(FIPS, name = "numb_per_country_filtered")

bottom_run_perc <- left_join(filtered_bottom_run, all_perc, by = "FIPS")
bottom_run_perc <- right_join(world_map, bottom_run_perc, by = c("ISO3" = "FIPS"))

bottom_run_perc <- bottom_run_perc %>% 
    mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)

write_rds(bottom_run_perc, "data/bottom_run_perc.rds")

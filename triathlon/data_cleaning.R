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
    filter(event_title != "2014 ITU World Triathlon Cape Town" & event_title != "2015 ITU World Triathlon Cape Town") %>% 
    mutate(total_time = hms::as_hms(total_time),
           swim_time = hms::as_hms(swim_time),
           bike_time = hms::as_hms(bike_time),
           run_time = hms::as_hms(run_time)) %>% 
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

write_csv2(triathlon_data, "data/cleaned_data.csv")
write_rds(triathlon_data, "data/cleaned_data.RDS")

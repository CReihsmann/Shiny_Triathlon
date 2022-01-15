#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Triathlon: Three Times the Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3,
                     radioButtons("race_type",
                                  "Race Type: ",
                                  choices = list("Olympic" = "Standard",
                                                 "Sprint" = "Sprint"),
                                  selected = "Standard"
                     ),
                     checkboxGroupInput("sex",
                                        "Sex: ",
                                        list("Male" = "male",
                                             "Female" = "female"),
                                        selected = c("male", "female")
                     ),
                     sliderInput("slider_age",
                                 "Age Range: ",
                                 min = 15,
                                 max = 45,
                                 value = c(15, 45)
                     ),
                     
                     selectizeInput("athlete_country",
                                    "Athletes' Country of Origin: ",
                                    choices = unique(triathlon_data$athlete_country_name),
                                    multiple = TRUE
                                    
                     ),
                     sliderInput("slider_position",
                                 "Finishing Percentile (fastest to slowest): ",
                                 min = 1,
                                 max = 100,
                                 value = c(1, 100)
                     )
                     
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Overall Race",
                         fluidRow(
                             column(width = 12,
                                    plotlyOutput("linePlot"))),
                         fixedRow(
                             column(width = 5,
                                    plotlyOutput("totaltime_boxPlot")),
                             column(width = 7,
                                    align = "right",
                                    fluidRow(
                                        plotOutput("ridgePlot")),
                                    fluidRow(
                                        sliderInput("slider_year",
                                                    "Year ",
                                                    min = 2009,
                                                    max = 2019,
                                                    value = c(2009, 2019),
                                                    sep = ""
                                        )
                                    )
                             )
                         )
                ),
                tabPanel("Race Components",
                         tabsetPanel(
                             tabPanel("Swim",
                                      fluidRow(
                                          column(width = 3,
                                                 radioButtons("wetsuit",
                                                              "Wetsuit: ",
                                                              list("Yes" = TRUE,
                                                                   "No" = FALSE),
                                                              selected = FALSE),
                                                 sliderInput("water_temp",
                                                             "Water Temperature (F): ",
                                                             min = 52,
                                                             max = 86,
                                                             value = c(52, 86)),
                                                 sliderInput("air_temp",
                                                             "Air Temperature (F): ",
                                                             min = 45,
                                                             max = 95,
                                                             value = c(45, 95))),
                                          column(width = 9,
                                                 plotOutput("swim_pointPlot"))
                                      ),
                                      fluidRow(
                                          column(width = 12,
                                                 plotOutput("swim_densityPlot"))
                                      )
                             ),
                             tabPanel("Bike",
                                      fluidRow(
                                          column(width = 3,
                                                 sliderInput("water_temp_bike",
                                                             "Water Temperature (F): ",
                                                             min = 52,
                                                             max = 86,
                                                             value = c(52, 86)),
                                                 sliderInput("air_temp_bike",
                                                             "Air Temperature (F): ",
                                                             min = 45,
                                                             max = 95,
                                                             value = c(45, 95))),
                                          column(width = 9,
                                                 plotOutput("bike_pointPlot"))
                                      ),
                                      fluidRow(
                                          column(width = 12,
                                                 plotOutput("bike_densityPlot"))
                                      )
                             ),
                             tabPanel("Run",
                                      fluidRow(
                                          column(width = 3,
                                                 sliderInput("water_temp_run",
                                                             "Water Temperature (F): ",
                                                             min = 52,
                                                             max = 86,
                                                             value = c(52, 86)),
                                                 sliderInput("air_temp_run",
                                                             "Air Temperature (F): ",
                                                             min = 45,
                                                             max = 95,
                                                             value = c(45, 95))),
                                          column(width = 9,
                                                 plotOutput("run_pointPlot"))
                                      ),
                                      fluidRow(
                                          column(width = 12,
                                                 plotOutput("run_densityPlot"))
                                      )
                             )
                         )),
                tabPanel("Map Exploration")
            )
        )
    )
))

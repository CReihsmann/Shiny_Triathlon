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
shinyUI(fluidPage(theme = shinytheme("superhero"),
                  navbarPage(strong("Triathlons: Three Times the Data"),
                             tabPanel("Plots",
                                      sidebarLayout(
                                          sidebarPanel(width = 2,
                                                       radioButtons("race_type",
                                                                    "Race Type: ",
                                                                    choices = list("Olympic" = "Standard",
                                                                                   "Sprint" = "Sprint"),
                                                                    selected = "Standard"
                                                       ),
                                                       radioButtons("sex",
                                                                    "Gender: ",
                                                                    list("Male" = "male",
                                                                         "Female" = "female"),
                                                                    selected = "male"
                                                       ),
                                                       radioButtons("race_port",
                                                                    "Race Portion: ",
                                                                    choices = list("Overall" = "total_time",
                                                                                   "Swim" = "swim_time",
                                                                                   "Bike" = "bike_time",
                                                                                   "Run" = "run_time"),
                                                                    selected = "total_time"),
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
                                                       ),
                                                       sliderInput("slider_year",
                                                                   "Year (event locations & age groups): ",
                                                                   min = 2009,
                                                                   max = 2019,
                                                                   value = c(2009, 2019),
                                                                   sep = ""
                                                       )
                                                       
                                                       
                                          ),
                                          
                                          # Show a plot of the generated distribution
                                          mainPanel("Overall Race",
                                                           fluidRow(
                                                               tabsetPanel(
                                                                   tabPanel("Average Race Times",
                                                                            fluidRow(h4("Average Race Times by Year", align = 'center'),
                                                                                     column(width = 12,
                                                                                            plotlyOutput("linePlot"),
                                                                                            br())
                                                                            )),
                                                                   tabPanel("Time Distributions",
                                                                            fluidRow(h4("Time Distributions by Year", align = 'center'),
                                                                                     column(width = 12,
                                                                                            plotlyOutput("totaltime_boxPlot"),
                                                                                            br())
                                                                            )))),
                                                           fluidRow(
                                                               column(width = 7, h5("Time Distributions by Location", align = 'center'),
                                                                      style = 'padding:20px;',
                                                                      plotOutput("ridgePlot")),
                                                               column(width = 5, h5("Race Results by Age Group", align = 'center'),
                                                                      style = 'padding:20px;',
                                                                      plotlyOutput("age_groupPlot")
                                                               )
                                                           )
                                                  ))),
                             tabPanel("Disciplines",
                                      sidebarLayout(
                                          sidebarPanel(width = 2,
                                                       radioButtons("race_type2",
                                                                    "Race Type: ",
                                                                    choices = list("Olympic" = "Standard",
                                                                                   "Sprint" = "Sprint"),
                                                                    selected = "Standard"
                                                       ),
                                                       radioButtons("sex2",
                                                                    "Gender: ",
                                                                    list("Male" = "male",
                                                                         "Female" = "female"),
                                                                    selected = "male"
                                                       ),
                                                       sliderInput("slider_age2",
                                                                   "Age Range: ",
                                                                   min = 15,
                                                                   max = 45,
                                                                   value = c(15, 45)
                                                       ),
                                                       
                                                       selectizeInput("athlete_country2",
                                                                      "Athletes' Country of Origin: ",
                                                                      choices = unique(triathlon_data$athlete_country_name),
                                                                      multiple = TRUE
                                                                      
                                                       ),
                                                       sliderInput("slider_position2",
                                                                   "Finishing Percentile (fastest to slowest): ",
                                                                   min = 1,
                                                                   max = 100,
                                                                   value = c(1, 100)
                                                       )
                                      ),
                                      mainPanel(
                                          tabsetPanel(
                                              tabPanel("Swim",
                                                       fluidRow(
                                                           column(width = 3,
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
                                                                  h4("Swim Position vs Final Position", align = 'center'),
                                                                  plotOutput("swim_pointPlot"),
                                                                  br(),
                                                                  br())
                                                       ),
                                                       fluidRow(
                                                           column(width = 12,
                                                                  h4("Time Distributions", align = 'center'),
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
                                                                  h4("Bike Position vs Final Position", align = 'center'),
                                                                  plotOutput("bike_pointPlot"),
                                                                  br(),
                                                                  br())
                                                       ),
                                                       fluidRow(
                                                           column(width = 12,
                                                                  h4("Time Distributions", align = 'center'),
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
                                                                  h4("Run Position vs Final Position", align = 'center'),
                                                                  plotOutput("run_pointPlot"),
                                                                  br(),
                                                                  br())
                                                       ),
                                                       fluidRow(
                                                           column(width = 12,
                                                                  h4("Time Distributions", align = 'center'),
                                                                  plotOutput("run_densityPlot"))
                                                       )
                                              )
                                          )
                                      )
                                      )),
                             tabPanel("Maps",
                                      sidebarLayout(
                                          sidebarPanel(width = 2,
                                                       radioButtons("top_bottomperc",
                                                                    "Result Percentile: ",
                                                                    choices = c("Top 10%" = 10,
                                                                                "Bottom 10%" = 90),
                                                                    selected = 10)),
                                          mainPanel(
                                              fluidRow(
                                                  tabsetPanel(
                                                      tabPanel("Overall",
                                                               fluidRow(
                                                                   column(width = 12,
                                                                          plotlyOutput("overall_choropleth"))
                                                               )),
                                                      tabPanel("Swim",
                                                               fluidRow(
                                                                   column(width = 12,
                                                                          plotlyOutput("swim_choropleth"))
                                                               )),
                                                      tabPanel("Bike",
                                                               fluidRow(
                                                                   column(width = 12,
                                                                          plotlyOutput("bike_choropleth"))
                                                               )),
                                                      tabPanel("Run",
                                                               fluidRow(
                                                                   column(width = 12,
                                                                          plotlyOutput("run_choropleth"))
                                                               ))
                                                  )
                                              ))
                                      )
                             ))))
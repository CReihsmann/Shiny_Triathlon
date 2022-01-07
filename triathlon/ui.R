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
        sidebarPanel(width = 4,
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
                                 value = c(25, 35)
                     ),
                     
                     selectizeInput("athlete_country",
                                    "Athletes' Country of Origin: ",
                                    choices = unique(triathlon_data$athlete_country_name),
                                    multiple = TRUE
                                    
                     ),
                     selectizeInput("race_region",
                                    "Region of Event: ",
                                    choices = unique(triathlon_data$event_region_name),
                                    multiple = TRUE
                                    
                     ),
                     sliderInput("slider_position",
                                 "Finishing Position: ",
                                 min = 1,
                                 max = 70,
                                 value = c(1, 70)
                     )
                     
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Overall Race",
                         fluidRow(
                             column(width = 7,
                                    plotOutput("linePlot")),
                             column(width = 5,
                                    plotOutput("barPlot"))
                         ),
                         fluidRow(
                             column(width = 5,
                                    plotOutput("totaltime_boxPlot")),
                             (column(width = 7,
                                     fluidRow(
                                         plotOutput("ridgePlot")),
                                     fluidRow(
                                         sliderInput("slider_year",
                                                     "Year ",
                                                     min = 2009,
                                                     max = 2019,
                                                     value = c(2009, 20219)
                                         )
                                         
                                     )
                             )
                             )
                         )
                ),
                tabPanel("Race Components"),
                tabPanel("Map Exploration")
            )
        )
    )
))

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    base_filter <- reactive({
        if (is.null(input$athlete_country)) {
            triathlon_data %>% 
                filter(cat_name == input$race_type,
                       athlete_age >= input$slider_age[1] , athlete_age <= input$slider_age[2],
                       position >= input$slider_position[1], position <= input$slider_position[2],
                       athlete_gender == input$sex)
        }
        else if (!is.null(input$athlete_country)) {
            triathlon_data %>% 
                filter(cat_name == input$race_type,
                       athlete_age >= input$slider_age[1] , athlete_age <= input$slider_age[2],
                       position >= input$slider_position[1], position <= input$slider_position[2],
                       athlete_gender == input$sex,
                       athlete_country_name %in% c(input$athlete_country))
        }
    })

    output$linePlot <- renderPlot({
        
        line_schematics <- function(data){
            data %>% 
            group_by(prog_year) %>% 
                summarize(mean_time = mean(total_time, na.rm = TRUE))
        }
        
        average_line <- average_line %>%
            filter(cat_name == input$race_type) %>% 
            line_schematics() %>% 
            mutate(line_type = "Average")
        
        filtered_line <- base_filter() %>% 
            line_schematics() %>% 
            mutate(line_type = "Filtered")
        
        average_line <- bind_rows(average_line, filtered_line)
        
        average_line%>% 
            ggplot(aes(x = prog_year, y = mean_time, color = line_type)) +
            geom_line(size = 1.2) +
            geom_point(size = 4) +
            scale_x_continuous(breaks = unique(triathlon_data$prog_year)) +
            scale_y_time() 
        
        
    })
    
    output$totaltime_boxPlot <- renderPlot({
        base_filter() %>% 
            mutate(prog_year = as.character(prog_year)) %>% 
            ggplot(aes(y = total_time, x = prog_year, group=prog_year, fill = prog_year)) +
            geom_boxplot() +
            geom_jitter(color = "black", size = 0.4, alpha = 0.6) +
            scale_x_discrete(breaks = unique(triathlon_data$prog_year)) +
            scale_y_time(labels = function(x) strftime(x, "%H:%M")) +
            theme(legend.position = "none")
    })
    
    output$ridgePlot <- renderPlot({
        
        base_filter() %>% 
            filter(between(prog_year, input$slider_year[1], input$slider_year[2])) %>% 
            ggplot(aes(x = total_time, y = event_country)) +
            geom_density_ridges() 
        
    })

})

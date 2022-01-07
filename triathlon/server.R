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
        if (is.null(input$athlete_country) & is.null(input$race_region)) {
            triathlon_data %>% 
                filter(cat_name == input$race_type,
                       athlete_age >= input$slider_age[1] , athlete_age <= input$slider_age[2],
                       position >= input$slider_position[1], position <= input$slider_position[2],
                       athlete_gender == input$sex)
        }
        else if (input$athlete_country == "" & input$race_region != "") {
            triathlon_data %>% 
                filter(cat_name == input$race_type,
                       athlete_age >= input$slider_age[1] , athlete_age <= input$slider_age[2],
                       position >= input$slider_position[1], position <= input$slider_position[2],
                       athlete_gender == input$sex,
                       event_region_name == input$race_region)
        }
        else if (input$athlete_country != "" & input$race_region == "") {
            triathlon_data %>% 
                filter(cat_name == input$race_type,
                       athlete_age >= input$slider_age[1] , athlete_age <= input$slider_age[2],
                       position >= input$slider_position[1], position <= input$slider_position[2],
                       athlete_gender == input$sex,
                       athlete_country_name == input$athlete_country)
        }
        else if (input$athlete_country != "" & input$race_region != ""){
            filter(cat_name == input$race_type,
                   athlete_age >= input$slider_age[1] , athlete_age <= input$slider_age[2],
                   position >= input$slider_position[1], position <= input$slider_position[2],
                   athlete_gender == input$sex,
                   athlete_country_name == input$athlete_country,
                   event_region_name == input$race_region)
        }
    })

    output$linePlot <- renderPlot({
        
        #line_schematics <- function({
            
        })
        
        base_filter() %>% 
            group_by(prog_year) %>% 
            summarize(mean_time = mean(total_time, na.rm = TRUE)) %>% 
            ggplot(aes(x = prog_year, y = mean_time)) +
            geom_line() +
            geom_point() +
            scale_x_continuous(breaks = unique(triathlon_data$prog_year)) 
        
        
    })
    
    output$barPlot <- renderPlot({
        
       base_filter() %>% 
            pivot_longer(swim_time:run_time, names_to = "splits", values_to = "split_times") %>%
            mutate(splits = fct_relevel(splits, "run_time", "t2_time", "bike_time", "t1_time", "swim_time")) %>%
            mutate(split_perc = as.numeric(split_times/as.numeric(total_time))) %>% 
            group_by(prog_year, splits) %>% 
            summarize(mean_splits_perc = mean(split_perc, na.rm = TRUE)) %>%
            ggplot(aes(y = mean_splits_perc, x = prog_year, fill = fct_inorder(splits))) +
            geom_bar(position = "fill", stat = "identity") +
            coord_flip() +
            scale_x_continuous(breaks = unique(triathlon_data$prog_year))
        
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
            ggplot(aes(x = total_time, y = event_region_name)) +
            geom_density_ridges()
        
    })

})

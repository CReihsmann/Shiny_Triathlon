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
                       between(athlete_age, input$slider_age[1], input$slider_age[2]),
                       between(position_perc, input$slider_position[1], input$slider_position[2]),
                       athlete_gender == input$sex)
        }
        else if (!is.null(input$athlete_country)) {
            triathlon_data %>% 
                filter(cat_name == input$race_type,
                       between(athlete_age, input$slider_age[1], input$slider_age[2]),
                       between(position_perc, input$slider_position[1], input$slider_position[2]),
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
        
        #y_scale <- reactive({
        #   if(input$race_type == "Standard") {
        #      scale_y_time(limits = hms::as_hms(c("00:40:00", "01:30:00")))
        # }
        #else if (input$race_type == "Sprint"){
        #   scale_y_time(limits = hms::as_hms(c("01:30:00", "02:20:00")))
        #   }
        # })
        
        average_line <- triathlon_data %>%
            filter(cat_name == input$race_type) %>% 
            line_schematics() %>% 
            mutate(line_type = "Average")
        
        filtered_line <- base_filter() %>% 
            line_schematics() %>% 
            mutate(line_type = "Filtered")
        
        average_line <- bind_rows(average_line, filtered_line)
        
        average_line %>%
            ggplot(aes(x = prog_year, y = mean_time, color = line_type)) +
            geom_line(size = 1.2) +
            geom_point(size = 4) +
            scale_x_continuous(breaks = unique(triathlon_data$prog_year)) +
            scale_y_time(labels = function(t) strftime(t, '%H:%M')) +
            theme_minimal() +
            labs(title = 'Average Race Times by year',
                 x = 'Year',
                 y = 'Average Time (hour:minute)') +
            theme(legend.position = c(0.87, 0.9),
                  legend.background = element_rect(fill="white", color = "white"),
                  legend.title = element_blank(),
                  axis.text = element_text(size = 14),
                  axis.text.y = element_text(hjust = 0.5),
                  axis.title = element_text(size = 16),
                  legend.text = element_text(size = 16),
                  plot.title = element_text(size = 20))
        # y_scale()
        
        
    })
    
    output$totaltime_boxPlot <- renderPlot({
        base_filter() %>%  
            ggplot(aes(y = total_time, x = prog_year, group=prog_year)) +
            geom_boxplot(fill = "lightgray") +
            geom_jitter(color = "black", size = 0.4, alpha = 0.2) +
            scale_x_continuous(breaks = unique(triathlon_data$prog_year)) +
            scale_y_time(labels = function(x) strftime(x, "%H:%M")) +
            theme(legend.position = "none")+
            theme_minimal() +
            labs(title = 'Spread of Race Times by Year',
                 x = "Year", 
                 y = "Time (hour:minute") +
            theme(axis.text.x = element_text(angle = 45),
                  axis.text = element_text(size = 12),
                  plot.title = element_text(size = 18),
                  axis.title = element_text(size = 14))
    })
    
    output$ridgePlot <- renderPlot({
        
        base_filter() %>% 
            filter(between(prog_year, input$slider_year[1], input$slider_year[2])) %>% 
            ggplot(aes(x = total_time, y = event_country)) +
            geom_density_ridges() +
            theme_minimal()
        
    })
    
    output$swim_densityPlot <- renderPlot({ 
        filtered_swim <- base_filter() %>% 
            filter(!is.na(temp_water), 
                   wetsuit == input$wetsuit, 
                   swim_time > hms::as_hms("00:00:00")) %>% 
            filter(between(temp_water, input$water_temp[1], input$water_temp[2]),
                   between(temp_air, input$air_temp[1], input$air_temp[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_swim <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   swim_time > hms::as_hms("00:00:00"), 
                   cat_name == input$race_type,
                   athlete_gender == input$sex) %>% 
            mutate(time_cat = "Average")
        
        average_swim <- bind_rows(filtered_swim, average_swim)
        
        average_swim %>% 
            ggplot(aes(x=swim_time, fill = time_cat))+
            geom_density(alpha = 0.8)
    })
    
    output$swim_pointPlot <- renderPlot({
        
        base_filter() %>% 
            filter(!is.na(temp_water), 
                   wetsuit == input$wetsuit, 
                   swim_time > hms::as_hms("00:00:00"),
                   between(temp_water, input$water_temp[1], input$water_temp[2]),
                   between(temp_air, input$air_temp[1], input$air_temp[2])) %>% 
            ggplot(aes(x = position_perc, y = swim_position_perc)) +
            geom_point(size = 1.5, alpha = 0.05)+
            geom_smooth(method = "lm", size = 1.5)+
            theme_bw()
    })
    
    output$bike_densityPlot <- renderPlot({ 
        
        filtered_bike <- base_filter() %>% 
            filter(!is.na(temp_air), 
                   bike_time > hms::as_hms("00:00:00"),
                   between(temp_water, input$water_temp_bike[1], input$water_temp_bike[2]),
                   between(temp_air, input$air_temp_bike[1], input$air_temp_bike[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_bike <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   bike_time > hms::as_hms("00:00:00"), 
                   cat_name == input$race_type,
                   athlete_gender == input$sex) %>% 
            mutate(time_cat = "Average")
        
        average_bike <- bind_rows(filtered_bike, average_bike)
        
        average_bike %>% 
            ggplot(aes(x=bike_time, fill = time_cat))+
            geom_density(alpha = 0.8)
    })
    
    output$bike_pointPlot <- renderPlot({
        
        base_filter() %>% 
            filter(!is.na(temp_air),  
                   bike_time > hms::as_hms("00:00:00"),
                   between(temp_water, input$water_temp_bike[1], input$water_temp_bike[2]),
                   between(temp_air, input$air_temp_bike[1], input$air_temp_bike[2])) %>% 
            ggplot(aes(x = position_perc, y = bike_position_perc)) +
            geom_point(size = 1.5, alpha = 0.05)+
            geom_smooth(method = "lm", size = 1.5)+
            theme_bw()
    })
    
    output$run_densityPlot <- renderPlot({ 
        
        filtered_run <- base_filter() %>% 
            filter(!is.na(temp_air), 
                   run_time > hms::as_hms("00:00:00"),
                   between(temp_water, input$water_temp_run[1], input$water_temp_run[2]),
                   between(temp_air, input$air_temp_run[1], input$air_temp_run[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_run <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   run_time > hms::as_hms("00:00:00"), 
                   cat_name == input$race_type,
                   athlete_gender == input$sex) %>% 
            mutate(time_cat = "Average")
        
        average_run <- bind_rows(filtered_run, average_run)
        
        average_run %>% 
            ggplot(aes(x=run_time, fill = time_cat))+
            geom_density(alpha = 0.8)
        
    })
    
    output$run_pointPlot <- renderPlot({
        
        base_filter() %>% 
            filter(!is.na(temp_air),  
                   run_time > hms::as_hms("00:00:00"),
                   between(temp_water, input$water_temp_run[1], input$water_temp_run[2]),
                   between(temp_air, input$air_temp_run[1], input$air_temp_run[2])) %>% 
            ggplot(aes(x = position_perc, y = run_position_perc)) +
            geom_point(size = 1.5, alpha = 0.05)+
            geom_smooth(method = "lm", size = 1.5)+
            theme_bw()
    })
    
})

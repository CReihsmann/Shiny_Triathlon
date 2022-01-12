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
        # y_scale()
        
        
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
    
    output$swim_densityPlot <- renderPlot({ 
        filtered_swim <- base_filter() %>% 
            filter(!is.na(temp_water), 
                   wetsuit == input$wetsuit, 
                   swim_time > hms::as_hms("00:00:00"),
                   event_title != "2014 ITU World Triathlon Cape Town") %>% 
            filter(between(temp_water, input$water_temp[1], input$water_temp[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_swim <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   swim_time > hms::as_hms("00:00:00"), 
                   cat_name == input$race_type,
                   athlete_gender == input$sex,
                   event_title != "2014 ITU World Triathlon Cape Town") %>% 
            mutate(time_cat = "Average")
        
        swim_comp <- bind_rows(filtered_swim, average_swim)
        
        swim_comp %>% 
            ggplot(aes(x=swim_time, fill = time_cat))+
            geom_density(alpha = 0.8)
    })
    
    output$swim_pointPlot <- renderPlot({
        
        base_filter() %>% 
            filter(!is.na(temp_water), 
                   wetsuit == input$wetsuit, 
                   swim_time > hms::as_hms("00:00:00"),
                   event_title != "2014 ITU World Triathlon Cape Town" & event_title != "2015 ITU World Triathlon Cape Town") %>% 
            ggplot(aes(x = position_perc, y = swim_position_perc)) +
            geom_point(size = 1.5, alpha = 0.05)+
            geom_smooth(method = "lm", size = 1.5)+
            theme_bw()
    })
    
})

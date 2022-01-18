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
    
    choropleth_color <- function(data, inputs){
        if (inputs == 10) {
            add_trace(data,
                      z = ~percent_total, color = ~percent_total, locations = ~FIPS, colors = 'Blues',
                      zmax = 30, zmin = 0) %>% 
                layout(title = "Top 10% Finishers by Country",
                       geo = list(showframe = F))
        }
        else if(inputs == 90) {
            add_trace(data,
                      z = ~percent_total, color = ~percent_total, locations = ~FIPS, colors = 'Reds',
                      zmax = 30, zmin = 0) %>% 
                layout(title = "Bottom 10% Finishers by Country",
                       geo = list(showframe = F))
        }
    }
    
    time_scale_line <-function(data, input1, input2){
        if(input1 == "Standard" & input2 == "male") {
            layout(data, title = "Average Race Times by year",
                   xaxis = list(title = 'Year',
                                dtick = 1), 
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE,
                                range = c(limit1_m_oly, limit2_m_oly)),
                   legend = list(x=0.8, y=0.9))
            
        }
        else if(input1 == "Standard" & input2 == "female") {
            layout(data, title = "Average Race Times by year",
                   xaxis = list(title = 'Year',
                                dtick = 1), 
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE,
                                range = c(limit1_f_oly, limit2_f_oly)),
                   legend = list(x=0.8, y=0.9))
        }
        else if(input1 == "Sprint" & input2 == "male") {
            layout(data,title = "Average Race Times by year",
                   xaxis = list(title = 'Year',
                                dtick = 1), 
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE,
                                range = c(limit1_m_spr, limit2_m_spr)),
                   legend = list(x=0.8, y=0.9))
        }
        else if(input1 == "Sprint" & input2 == "female") {
            layout(data,title = "Average Race Times by year",
                   xaxis = list(title = 'Year',
                                dtick = 1), 
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE,
                                range = c(limit1_f_spr, limit2_f_spr)),
                   legend = list(x=0.8, y=0.9))
        }
    }
    
    time_scale_box <-reactive({
        if(input$race_type == "Standard") {
            scale_y_datetime(labels = function(t) strftime(t, '%H:%M'), 
                             limits = c(box_limit1, box_limit2))
        }
        else if(input$race_type == "Sprint") {
            scale_y_datetime(labels = function(t) strftime(t, '%H:%M'), 
                             limits = c(box_limit3, box_limit4))
        }
    })
    
    output$linePlot <- renderPlotly({
        
        line_schematics <- function(data){
            data %>% 
                group_by(prog_year) %>% 
                summarize(mean_time = mean(total_time, na.rm = TRUE))
        }
        
        average_line <- triathlon_data %>%
            filter(cat_name == input$race_type,
                   athlete_gender == input$sex) %>% 
            line_schematics() %>% 
            mutate(line_type = "Average")
        
        filtered_line <- base_filter() %>% 
            line_schematics() %>% 
            mutate(line_type = "Filtered")
        
        average_line <- bind_rows(average_line, filtered_line)
        
        average_line %>%
            plot_ly(x = ~prog_year, 
                    y = ~mean_time, mode = "lines+markers", 
                    linetype = ~line_type, 
                    line = list(width = 3),
                    marker = list(size = 8)) %>% 
            time_scale_line(input$race_type, input$sex)
        
    })
    
    output$totaltime_boxPlot <- renderPlotly({
        base_filter() %>% 
            plot_ly(x = ~prog_year, y = ~total_time, 
                    type = "violin", 
                    box = list(visible = T), 
                    jitter = 0.3) %>% 
            layout(title = "Average Race Times by year",
                   xaxis = list(title = 'Year',
                                dtick = 1),
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE))
    })
    
    output$ridgePlot <- renderPlot({
        
        average_ridge <- triathlon_data %>% 
            filter(cat_name == input$race_type,
                   athlete_gender == input$sex,
                   between(prog_year, input$slider_year[1], input$slider_year[2])) %>% 
            mutate(ridge_type = "All")
        
        filtered_ridge <- base_filter() %>% 
            filter(between(prog_year, input$slider_year[1], input$slider_year[2])) %>% 
            mutate(ridge_type = "Filtered")
        
        average_ridge <- bind_rows(average_ridge, filtered_ridge)
        
        average_ridge %>% 
            ggplot(aes(x = total_time, y = event_country, fill = ridge_type)) +
            geom_density_ridges(alpha = 0.7) +
            theme_minimal() +
            labs(title = 'Race Time Distributions by Location',
                 x = 'Total Time',
                 y = 'Event Country') +
            theme(legend.position = c(0.9, 0.8),
                  legend.background = element_rect(fill = "white", color = "white"),
                  legend.title = element_blank(),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  plot.title = element_text(size = 20),
                  legend.text = element_text(size = 14))
        
    })
    
    output$age_groupPlot <-renderPlotly({
        
        triathlon_data %>% 
            filter(athlete_gender == input$sex, 
                   prog_year == input$slider_year,
                   cat_name == input$race_type) %>% 
            plot_ly(x = ~age_group, y = ~position_perc, type = "box") %>% 
            layout(title = "Race Results by Age Group",
                   xaxis = list(title = "Age Group"),
                   yaxis = list(title = "Percentile Finish (%)"))
        
    })
    
    output$swim_densityPlot <- renderPlot({ 
        filtered_swim <- base_filter() %>% 
            filter(!is.na(temp_water), 
                   wetsuit != T, 
                   swim_time > hms::as_hms("00:00:00")) %>% 
            filter(between(temp_water, input$water_temp[1], input$water_temp[2]),
                   between(temp_air, input$air_temp[1], input$air_temp[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_swim <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   wetsuit != T,
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
                   wetsuit != T, 
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
                   wetsuit != T, 
                   bike_time > hms::as_hms("00:00:00"),
                   between(temp_water, input$water_temp_bike[1], input$water_temp_bike[2]),
                   between(temp_air, input$air_temp_bike[1], input$air_temp_bike[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_bike <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   bike_time > hms::as_hms("00:00:00"), 
                   cat_name == input$race_type, 
                   wetsuit != T,
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
                   wetsuit != T,  
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
                   wetsuit != T,
                   between(temp_water, input$water_temp_run[1], input$water_temp_run[2]),
                   between(temp_air, input$air_temp_run[1], input$air_temp_run[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_run <- triathlon_data %>% 
            filter(!is.na(temp_water),  
                   wetsuit != T,
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
    
    output$overall_choropleth <- renderPlotly({
        
        percentile <- function(data, inputs){ 
            if (inputs == 10){
                
                filter(data, position_perc <= 10)
            }
            else if(inputs ==90){
                
                filter(data, position_perc >= 90)
            }
            
        }
        
        filtered_perc <- triathlon_data %>%
            percentile(input$top_bottomperc) %>% 
            count(FIPS, name = "numb_per_country_filtered")
        
        
        all_perc <- triathlon_data %>% 
            count(FIPS, name = "numb_per_country_total")
        comb_perc <- left_join(filtered_perc, all_perc, by = "FIPS")
        
        comb_perc <- comb_perc %>% 
            mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)
        
        comb_perc %>% 
            plot_geo() %>% 
            choropleth_color(input$top_bottomperc) %>% 
            colorbar(title = "% of Country Total")
        
    })
    
    output$swim_choropleth <- renderPlotly({
        
        percentile <- function(data, inputs){ 
            if (inputs == 10){
                
                filter(data, swim_position_perc <= 10)
            }
            else if(inputs ==90){
                
                filter(data, swim_position_perc >= 90)
            }
            
        }
        
        filtered_perc <- triathlon_data %>%
            percentile(input$top_bottomperc) %>% 
            count(FIPS, name = "numb_per_country_filtered")
        
        
        all_perc <- triathlon_data %>% 
            count(FIPS, name = "numb_per_country_total")
        comb_perc <- left_join(filtered_perc, all_perc, by = "FIPS")
        
        comb_perc <- comb_perc %>% 
            mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)
        
        comb_perc %>% 
            plot_geo() %>% 
            choropleth_color(input$top_bottomperc) %>% 
            colorbar(title = "% of Country Total")
        
    })
    
    output$bike_choropleth <- renderPlotly({
        
        percentile <- function(data, inputs){ 
            if (inputs == 10){
                
                filter(data, bike_position_perc <= 10)
            }
            else if(inputs ==90){
                
                filter(data, bike_position_perc >= 90)
            }
            
        }
        
        filtered_perc <- triathlon_data %>%
            percentile(input$top_bottomperc) %>% 
            count(FIPS, name = "numb_per_country_filtered")
        
        
        all_perc <- triathlon_data %>% 
            count(FIPS, name = "numb_per_country_total")
        comb_perc <- left_join(filtered_perc, all_perc, by = "FIPS")
        
        comb_perc <- comb_perc %>% 
            mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)
        
        comb_perc %>% 
            plot_geo() %>% 
            choropleth_color(input$top_bottomperc) %>% 
            colorbar(title = "% of Country Total")
        
    })
    
    output$run_choropleth <- renderPlotly({
        
        percentile <- function(data, inputs){ 
            if (inputs == 10){
                
                filter(data, run_position_perc <= 10)
            }
            else if(inputs ==90){
                
                filter(data, run_position_perc >= 90)
            }
            
        }
        
        filtered_perc <- triathlon_data %>%
            percentile(input$top_bottomperc) %>% 
            count(FIPS, name = "numb_per_country_filtered")
        
        
        all_perc <- triathlon_data %>% 
            count(FIPS, name = "numb_per_country_total")
        comb_perc <- left_join(filtered_perc, all_perc, by = "FIPS")
        
        comb_perc <- comb_perc %>% 
            mutate(percent_total = numb_per_country_filtered/numb_per_country_total*100)
        
        comb_perc %>% 
            plot_geo() %>% 
            choropleth_color(input$top_bottomperc) %>% 
            colorbar(title = "% of Country Total")
        
    })
    
})

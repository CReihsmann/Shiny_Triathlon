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
    
    base_filter2 <- reactive({
        if (is.null(input$athlete_country)) {
            triathlon_data %>% 
                filter(cat_name == input$race_type2,
                       between(athlete_age, input$slider_age2[1], input$slider_age2[2]),
                       between(position_perc, input$slider_position2[1], input$slider_position2[2]),
                       athlete_gender == input$sex2)
        }
        else if (!is.null(input$athlete_country)) {
            triathlon_data %>% 
                filter(cat_name == input$race_type2,
                       between(athlete_age, input$slider_age2[1], input$slider_age2[2]),
                       between(position_perc, input$slider_position2[1], input$slider_position2[2]),
                       athlete_gender == input$sex2,
                       athlete_country_name %in% c(input$athlete_country2))
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
            layout(data,xaxis = list(title = 'Year',
                                dtick = 1), 
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE,
                                range = c(limit1_m_oly, limit2_m_oly)),
                   legend = list(x=0.8, y=0.9))
            
        }
        else if(input1 == "Standard" & input2 == "female") {
            layout(data,xaxis = list(title = 'Year',
                                dtick = 1), 
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE,
                                range = c(limit1_f_oly, limit2_f_oly)),
                   legend = list(x=0.8, y=0.9))
        }
        else if(input1 == "Sprint" & input2 == "male") {
            layout(data,xaxis = list(title = 'Year',
                                dtick = 1), 
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE,
                                range = c(limit1_m_spr, limit2_m_spr)),
                   legend = list(x=0.8, y=0.9))
        }
        else if(input1 == "Sprint" & input2 == "female") {
            layout(data,xaxis = list(title = 'Year',
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
                summarize(mean_time = mean(!!as.name(input$race_port), na.rm = TRUE))
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
            layout(xaxis = list(title = 'Year',
                                dtick = 1,
                                fixedrange = T), 
                   yaxis = list(title = 'Average Time (hr:min)',
                                type = "date",
                                tickformat = '%H:%M',
                                fixedrange = TRUE),
                   legend = list(x=0.8, y=0.9))
        
         
#            time_scale_line(input$race_type, input$sex)
        
    })
    
    output$totaltime_boxPlot <- renderPlotly({
        
        race_port_filter <- function(data, inputs) {
            if (inputs == "total_time") {
                plot_ly(data, x = ~prog_year, y = ~total_time, 
                        type = "violin", 
                        box = list(visible = T), 
                        jitter = 0.3)
            }
            else if (inputs == "swim_time") {
                plot_ly(data, x = ~prog_year, y = ~swim_time, 
                        type = "violin", 
                        box = list(visible = T), 
                        jitter = 0.3)
            }
            else if (inputs == "bike_time") {
                plot_ly(data, x = ~prog_year, y = ~bike_time, 
                        type = "violin", 
                        box = list(visible = T), 
                        jitter = 0.3)
            }
            else if (inputs == "run_time") {
                plot_ly(data, x = ~prog_year, y = ~run_time, 
                        type = "violin", 
                        box = list(visible = T), 
                        jitter = 0.3)
            }
        }
        
        base_filter() %>% 
            race_port_filter(input$race_port) %>% 
            layout(xaxis = list(title = 'Year',
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
            ggplot(aes(x = !!as.name(input$race_port), y = event_country, fill = ridge_type)) +
            geom_density_ridges(alpha = 0.7) +
            theme_minimal() +
            labs(x = 'Total Time') +
            theme(legend.position = c(0.9, 0.8),
                  legend.background = element_rect(fill = "white", color = "white"),
                  legend.title = element_blank(),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  plot.title = element_text(size = 20),
                  legend.text = element_text(size = 14),
                  axis.title.y = element_blank())
        
    })
    
    output$age_groupPlot <-renderPlotly({
        
        disc_perc <- function(data, inputs){
            if(inputs == "total_time"){
                plot_ly(data, x = ~age_group, y = ~position_perc, type = "box")
            }
            else if (inputs == "swim_time"){
                plot_ly(data, x = ~age_group, y = ~swim_position_perc, type = "box")
            }
            else if (inputs == "bike_time"){
                plot_ly(data, x = ~age_group, y = ~bike_position_perc, type = "box")
            }
            else if (inputs == "run_time"){
                plot_ly(data, x = ~age_group, y = ~run_position_perc, type = "box")
            }
        }
        
        triathlon_data %>% 
            filter(athlete_gender == input$sex,
                   between(prog_year, input$slider_year[1], input$slider_year[2]),
                   cat_name == input$race_type) %>% 
            disc_perc(input$race_port) %>% 
            layout(xaxis = list(title = "Age Group"),
                   yaxis = list(title = "Percentile Finish (%)"))
        
    })
    
    output$swim_densityPlot <- renderPlot({ 
        filtered_swim <- base_filter2() %>% 
            filter(!is.na(temp_water), 
                   wetsuit != T, 
                   swim_time > hms::as_hms("00:00:00"),
                   between(temp_water, input$water_temp[1], input$water_temp[2]),
                   between(temp_air, input$air_temp[1], input$air_temp[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_swim <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   wetsuit != T, 
                   cat_name == input$race_type,
                   athlete_gender == input$sex) %>% 
            mutate(time_cat = "All")
        
        average_swim <- bind_rows(filtered_swim, average_swim)
        
        average_swim %>% 
            ggplot(aes(x=znorm_swim, fill = time_cat))+
            geom_density(alpha = 0.8) +
            theme_minimal()+
            theme(legend.position = c(0.8, 0.8),
                  legend.title = element_blank(),
                  legend.background = element_rect(fill = "white", linetype = "solid", colour = "white"),
                  axis.title = element_blank())
    })
    
    output$swim_pointPlot <- renderPlot({
        
        base_filter2() %>% 
            filter(!is.na(temp_water),
                   between(temp_water, input$water_temp[1], input$water_temp[2]),
                   between(temp_air, input$air_temp[1], input$air_temp[2])) %>% 
            ggplot(aes(y = position_perc, x = swim_position_perc)) +
            geom_point(size = 1.5, alpha = 0.05)+
            geom_smooth(method = "lm", size = 1.5)+
            theme_bw()
    })
    
    output$bike_densityPlot <- renderPlot({ 
        
        filtered_bike <- base_filter2() %>% 
            filter(!is.na(temp_air),
                   between(temp_water, input$water_temp_bike[1], input$water_temp_bike[2]),
                   between(temp_air, input$air_temp_bike[1], input$air_temp_bike[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_bike <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   cat_name == input$race_type, 
                   athlete_gender == input$sex) %>% 
            mutate(time_cat = "All")
        
        average_bike <- bind_rows(filtered_bike, average_bike)
        
        average_bike %>% 
            ggplot(aes(x=znorm_bike, fill = time_cat))+
            geom_density(alpha = 0.8) +
            theme_minimal()+
            theme(legend.position = c(0.8, 0.8),
                  legend.title = element_blank(),
                  legend.background = element_rect(fill = "white", linetype = "solid", colour = "white"),
                  axis.title = element_blank())
    })
    
    output$bike_pointPlot <- renderPlot({
        
        base_filter2() %>% 
            filter(!is.na(temp_air),
                   between(temp_water, input$water_temp_bike[1], input$water_temp_bike[2]),
                   between(temp_air, input$air_temp_bike[1], input$air_temp_bike[2])) %>% 
            ggplot(aes(y = position_perc, x = bike_position_perc)) +
            geom_point(size = 1.5, alpha = 0.05)+
            geom_smooth(method = "lm", size = 1.5)+
            theme_bw()
    })
    
    output$run_densityPlot <- renderPlot({ 
        
        filtered_run <- base_filter2() %>% 
            filter(!is.na(temp_air), 
                   between(temp_water, input$water_temp_run[1], input$water_temp_run[2]),
                   between(temp_air, input$air_temp_run[1], input$air_temp_run[2])) %>% 
            mutate(time_cat = "Filtered")
        
        average_run <- triathlon_data %>% 
            filter(!is.na(temp_water), 
                   cat_name == input$race_type,
                   athlete_gender == input$sex) %>% 
            mutate(time_cat = "All")
        
        average_run <- bind_rows(filtered_run, average_run)
        
        average_run %>% 
            ggplot(aes(x=znorm_run, fill = time_cat))+
            geom_density(alpha = 0.8)+
            theme_minimal() +
            theme(legend.position = c(0.8, 0.8),
                  legend.title = element_blank(),
                  legend.background = element_rect(fill = "white", linetype = "solid", colour = "white"),
                  axis.title = element_blank())
        
    })
    
    output$run_pointPlot <- renderPlot({
        
        base_filter2() %>% 
            filter(!is.na(temp_air),  
                   run_time > hms::as_hms("00:00:00"),
                   between(temp_water, input$water_temp_run[1], input$water_temp_run[2]),
                   between(temp_air, input$air_temp_run[1], input$air_temp_run[2])) %>% 
            ggplot(aes(y = position_perc, x = run_position_perc)) +
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



library(shiny)
library(plotly)
library(leaflet)
library(plyr)
library(dplyr)
library(rgdal)
library(shinyWidgets)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(sf)
library(rgeos)
library(httr)
library(jsonlite)
library(raster)
library(data.table)
library(DT)



########### Load boundary data ----

boundary <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/cities_boundaries.geojson",
                    quiet = TRUE)


########### Load amenity data ----

amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/amenity_exposure_lst.csv",
                                encoding = "UTF-8")



# define filter values ----

# compute average heat of all amenities
city_amenity_avg_heat = mean(amenity_exposure_lst$exposure_lst_mean)
amenity_exposure_lst$heat_dev_from_amenities = amenity_exposure_lst$exposure_lst_mean - city_amenity_avg_heat

# define available cities
available_cities = unique(amenity_exposure_lst$city_name)
available_cities = c("PHL-Makati", "CHL-Vitacura", "AUS-Hobart")
# define available sectors
available_amenity_sectors = unique(amenity_exposure_lst$gcom_sector_name)
available_amenity_sectors = available_amenity_sectors[!available_amenity_sectors %in% c("Residential","Other")]
# define available population categories
available_pop_categories = c("All", "Young (<20)", "Elderly (>60)", "Men", "Women")
available_pop_categories = available_amenity_sectors

# define slider threshold values
slider_min_heat = round(min(amenity_exposure_lst$exposure_lst_mean))
slider_max_heat = round(max(amenity_exposure_lst$exposure_lst_mean))
slider_value_heat = round(mean(amenity_exposure_lst$exposure_lst_mean))
slider_step = 1

# function to check value
check_value = function(value){
    if(value >= 0){sign = "+"}
    else if(value < 0){sign = "-"}
    
    return(sign)
}

# check_value_color = function(value){
#     if(value >= 0){color = "#FC1A03"}
#     else if(value < 0){color = "#0309FC"}
#     
#     return(color)
# }

### read land surface temperature ----

read.lst = function(selected_city, city_boundary, data_source){
    
    if(data_source == "s3"){
        # define path s3
        city_lst_path = paste("/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/hazards/heat/land_surface_temperature/lst_mean_lst_",
                              selected_city,
                              ".tif",
                              sep = "")
    } else if(data_source == "local"){
        # define path local
        city_lst_path = paste("./data/heat/lst_mean_lst_",
                              selected_city,
                              ".tif",
                              sep = "")
    }
    
    
    # collect raster data
    city_lst = raster(city_lst_path)
    # mask raster based on administrative boundaries
    city_lst_mask = raster::mask(city_lst,
                                 city_boundary)
    
}

# read population ----

read.pop.category =  function(pop_category, city_boundary, selected_city, data_source){
    
    if(pop_category == "All"){
        data_path = "pop_"
    } else if(pop_category == "Children"){
        data_path = "pop_children_"
    } else if(pop_category == "Elderly"){
        data_path = "pop_elderly_"
    } else if(pop_category == "Women"){
        data_path = "pop_women_"
    }
    
    
    if(data_source == "s3"){
        # define path s3
        city_pop_path = paste("/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/population/",
                              data_path,
                              selected_city,
                              ".tif",
                              sep = "")
    } else if(data_source == "local"){
        # define path local
        city_pop_path = paste("./data/population/",
                              data_path,
                              selected_city,
                              ".tif",
                              sep = "")
    }
    
    
    # collect raster data
    city_pop = raster(city_pop_path)
    city_pop[is.na(city_pop)] <- 0
    # mask raster based on administrative boundaries
    city_pop_mask = raster::mask(city_pop,city_boundary)
    
    
    
    return(city_pop_mask)
}


# UI ------------

ui = navbarPage("City Climate Hazard Exposure Tool",
                id = "active_tab",
                
                ### Amenity exposure panel ----
                tabPanel("Amenity exposure",
                         
                         ### First row ----
                         fluidRow(
                             ### Specify filters ----
                             column(3,
                                    
                                    
                                    ### Specify scale 
                                    selectInput(inputId = "City", 
                                                label = "Select city", 
                                                choices = available_cities,
                                                selected = "PHL-Makati",
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Hazard 
                                    selectInput(inputId = "Hazard", 
                                                label = "Select hazard", 
                                                choices = "Heat",
                                                selected = "Heat",
                                                multiple = FALSE,
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Hazard 
                                    selectInput(inputId = "Period", 
                                                label = "Select period of interest", 
                                                choices = "2020-2021",
                                                selected = "2020-2021",
                                                multiple = FALSE,
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Amenity sectors 
                                    selectInput(inputId = "Sector", 
                                                label = "Select sectors of interest", 
                                                choices = available_amenity_sectors,
                                                selected = available_amenity_sectors,
                                                multiple = TRUE,
                                                width = '100%'
                                    ),
                                    
                                    # Main indicators
                                    
                                    h5("Average site temperature (selected sectors):"),
                                    htmlOutput("selected_amenities_avg_heat"),
                                    
                                    h5("Selected sector(s) temperature deviation from city average:"),
                                    htmlOutput("selected_amenities_deviation_heat_value"),
                                    
                                    # h5("Selected sector(s) temperature deviation ratio from city average:"),
                                    htmlOutput("selected_amenities_deviation_heat_ratio"),
                                    
                                    
                             ),
                             ### Specify plots ----
                             column(8, 
                                    div(style = "background-color: red; width: 100%; height: 100%;"),
                                    # tab outputs
                                    tabsetPanel(type = "tabs",
                                                ### Amenity exposure Map plot 
                                                tabPanel("Site exposure", 
                                                         leafletOutput("Map_plot", height = 550)),
                                                ### sector exposure bar plot 
                                                tabPanel("Sector exposure", 
                                                         plotlyOutput("Sector_plot", height = 550)),
                                                ### Narrative summary 
                                                tabPanel("Amenity summary", 
                                                         htmlOutput("amenity_exposure_narrative", height = 550),
                                                         h1("-"))
                                    ),
                                    # slider
                                    sliderInput(inputId = 'heat_threshold', 
                                                label = 'Customize heat value threshold (default value = average amenities heat)', 
                                                min = slider_min_heat, 
                                                max = slider_max_heat, 
                                                value = slider_value_heat, 
                                                step = slider_step,
                                                width = '100%')
                             )
                         ),
                         
                         
                         ## Second row ----
                         fluidRow(
                             column(12, offset = 0, h4("Amenity Sector Details"), DT::dataTableOutput("table_amenity")),
                             
                             downloadButton(outputId = "download_amenity_exposure", 
                                            label = "Download amenity exposure data"),
                             
                             downloadButton(outputId = "download_sector_exposure", 
                                            label = "Download sectors' exposure data")
                         )
                ),
                ## Population exposure panel ----
                tabPanel("Population exposure",
                         
                         ### First row ----
                         fluidRow(
                             ### Specify filters ----
                             column(3,
                                    
                                    ### Specify population of interest
                                    selectInput(inputId = "Category_pop",
                                                 label = "Select population category",
                                                 choices = c("All", "Elderly", "Children", "Women"),
                                                 selected = "All"
                                    ),
                                    
                                    
                                    h4("Percent of exposed population:"),
                                    htmlOutput("pop_exposure_ratio"),
                                    
                                    
                             ),
                             ### Specify poulation exposure plots ----
                             column(8,
                                    div(style = "background-color: red; width: 100%; height: 100%;"),
                                    tabsetPanel(type = "tabs",
                                                ### Map plot
                                                tabPanel("Residential exposure", 
                                                         leafletOutput("Map_plot_pop", 
                                                                       height = 600)),
                                                ### timeseirs plot
                                                tabPanel("Category exposure", 
                                                         plotlyOutput("Sector_plot_pop",
                                                                      height = 600)),
                                                ### Narrative summary 
                                                tabPanel("Population summary", 
                                                         htmlOutput("pop_exposure_narrative", 
                                                                    height = 550),
                                                         h1("-"))
                                    ),
                                    # slider
                                    sliderInput(inputId = 'heat_threshold_pop', 
                                                label = 'Customize heat value threshold', 
                                                min = slider_min_heat, 
                                                max = slider_max_heat, 
                                                value = slider_value_heat, 
                                                step = slider_step,
                                                width = '100%')
                             )
                         ),
                         
                         
                         # ## Second row ----
                         fluidRow(
                             column(12,
                                    offset = 0, 
                                    # h4("Observations"), 
                                    DT::dataTableOutput("table_population"))
                         )
                )
)


server <- function(input, output, session) {
    
    
    ### leaflet map definition ----
    
    # Reactive expression for the data subsetted to what the user selected
    filtereData <- reactive({
        amenity_exposure_lst[amenity_exposure_lst$city_id == input$City & amenity_exposure_lst$gcom_sector_name %in% input$Sector, ]
    })
    
    
    # output map centering amenity map
    output$Map_plot <- renderLeaflet({
        leaflet(filtereData()) %>% 
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })
    
    # output map centering population map
    output$Map_plot_pop <- renderLeaflet({
        leaflet(filtereData()) %>%
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })
    
    # # #update sector list depending on selected city
    observeEvent(
        input$City,
        updateSliderInput(session,
                          inputId = "heat_threshold",
                          value = round(mean(amenity_exposure_lst[amenity_exposure_lst$city_id == input$City, "exposure_lst_mean"])),
                          min = round(min(amenity_exposure_lst[amenity_exposure_lst$city_id == input$City, "exposure_lst_mean"])),
                          max = round(max(amenity_exposure_lst[amenity_exposure_lst$city_id == input$City, "exposure_lst_mean"])),
                          step = 1
        )
    )
    
    observeEvent(
        input$City,
        updateSliderInput(session,
                          inputId = "heat_threshold_pop",
                          value = round(mean(amenity_exposure_lst[amenity_exposure_lst$city_id == input$City, "exposure_lst_mean"])),
                          min = round(min(amenity_exposure_lst[amenity_exposure_lst$city_id == input$City, "exposure_lst_mean"])),
                          max = round(max(amenity_exposure_lst[amenity_exposure_lst$city_id == input$City, "exposure_lst_mean"])),
                          step = 1
        )
    )
    
    
    
    ### reactive plots definition ----
    observe({
        
        
        # update panel data when the panel is selected
        input$active_tab
        
        ### Amenity level ----
        
        
        # filter data
        selected_city = input$City
        selected_sector = input$Sector
        
        # filter boundary data
        city_boundary = boundary %>% 
            filter(city_id == selected_city)
        
        # filter amenity
        city_amenity = amenity_exposure_lst %>%
            filter(city_id == selected_city,
                   gcom_sector_name %in% selected_sector) 

        
        # get heat threshold
        heat_threshold_value = input$heat_threshold
        
        # compute deviation from threshold
        city_amenity$deviation_from_threshold = round((city_amenity$exposure_lst_mean/heat_threshold_value*100)-100,2) 
        vec_deviation_from_threshold = city_amenity$deviation_from_threshold
        
        
        city_amenity[city_amenity$deviation_from_threshold <= 0,"exposure_class" ] = "0-Low"
        city_amenity[city_amenity$deviation_from_threshold > 0 & city_amenity$deviation_from_threshold <= 10,"exposure_class" ] = "1-Moderate"
        city_amenity[city_amenity$deviation_from_threshold > 10,"exposure_class" ] = "2-High"
        vec_exposure_class = city_amenity$exposure_class
        
        ### read land surface temperature ----
        
        city_lst_mask = read.lst(city_boundary = city_boundary,
                                 selected_city = selected_city,
                                 data_source = "local")
        
        
        # prepare map plot ----
        
        # define numerical color palette
        pal_amenity <- colorNumeric("RdYlBu", 
                                    domain = city_amenity$exposure_lst_mean,
                                    reverse = TRUE)
        
        # define class color palette
        pal_amenity_class <- colorFactor(c("green","yellow","red"),
                                         levels =c("0-Low","1-Moderate","2-High"))
        
        
        # define ratser color palette for LST data
        pal_Grid <- colorNumeric("RdYlBu", 
                                 values(city_lst_mask),
                                 na.color = "transparent",
                                 reverse = TRUE)
        
        # define labels information
        labels_amenity <- sprintf("<strong>%s</strong> %s <br/><strong>%s:</strong> %s %s<br/><strong>%s:</strong> %s<br/><strong>%s:</strong> %s",
                                  "Name",city_amenity$city_name, 
                                  "Land surface temp", round(city_amenity$exposure_lst_mean, 2), "celsus",
                                  "Amenity type", city_amenity$feature_value,
                                  "Amenity sector", city_amenity$gcom_sector_name) %>% 
            lapply(htmltools::HTML)
        
        
        # plot map ----
        leafletProxy(mapId = "Map_plot", data = filtereData())  %>%
            # Base groups
            addTiles(group = "OSM (default)") %>%
            addTiles() %>%
            clearControls() %>%
            clearShapes() %>%
            # Plot amenities with heat value
            addCircleMarkers(~longitude, ~latitude,
                             radius = 4,
                             color = "black",
                             fillColor = ~pal_amenity(exposure_lst_mean),
                             stroke = TRUE,
                             fillOpacity = 0.7,
                             weight = 1,
                             popup = ~as.character(city_name),
                             label = labels_amenity,
                             group = "Amenity exposure value") %>%
            # Legend for amenity heat values
            addLegend(pal = pal_amenity, 
                      values = ~exposure_lst_mean, 
                      opacity = 0.9,
                      title = "Amenity heat value (°C)",
                      position = "bottomright",
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>%
            # Plot amenities with heat deviation ratio from selected threshold class
            addCircleMarkers(~longitude, ~latitude,
                             radius = 4,
                             color = "black",
                             fillColor = ~pal_amenity_class(vec_exposure_class),
                             stroke = TRUE,
                             fillOpacity = 0.7,
                             weight = 1,
                             popup = ~as.character(city_name),
                             label = labels_amenity,
                             group = "Amenity exposure class") %>%
            # Legend for amenity heat deviation ratio from threshold class
            addLegend(colors = c("green","yellow","red"),
                      labels = c("0-Low (< 0%)","1-Moderate (0 < +10%)","2-High (> +10%)"),
                      opacity = 0.9,
                      title = "Amenity exposure level</br>(Devitaion ratio from heat threshold %)",
                      position = "topright") %>%
            # plot boundary
            addPolygons(data = city_boundary,
                        group = "Administrative boundaries",
                        stroke = TRUE, color = "black", weight = 3,dashArray = "3",
                        smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.3,
                            bringToFront = TRUE),
                        label = city_boundary$city_name,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            # plot Heat distribution raster
            addRasterImage(city_lst_mask, 
                           colors = pal_Grid, 
                           opacity = 0.7,
                           group = "Land Surface Temperature",
                           maxBytes = 8 * 1024 * 1024) %>%
            # Legend for amenitu heat values
            addLegend(pal = pal_amenity, 
                      values = ~exposure_lst_mean, 
                      opacity = 0.9,
                      title = "Land Surface Temperature (°C)",
                      position = "bottomleft",
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>% 
            addLayersControl(
                overlayGroups = c("Amenity exposure value", 
                                  "Land Surface Temperature",
                                  "Amenity exposure class"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        ### Population ----
        
        
        # filter heat threshold
        city_lst_mask_threshold = city_lst_mask
        
        # get heat threshold
        heat_threshold_value = input$heat_threshold_pop
        
        values(city_lst_mask_threshold)[values(city_lst_mask_threshold) <= heat_threshold_value] = 0
        values(city_lst_mask_threshold)[values(city_lst_mask_threshold) > heat_threshold_value] = 1
        
        
        # filter population category
        selected_population = input$Category_pop
        
        # get filtered population category
        city_pop_mask = read.pop.category(pop_category = selected_population,
                                          city_boundary = city_boundary,
                                          selected_city = selected_city,
                                          data_source = "local")
        
        # create exposed population raster
        city_pop_heat_exposure = city_pop_mask * city_lst_mask_threshold
        
        # pop exposure classification
        city_pop_heat_exposure_dist = values(city_pop_heat_exposure)[!is.na(values(city_pop_heat_exposure))]
        city_pop_heat_exposure_dist = city_pop_heat_exposure_dist[city_pop_heat_exposure_dist>0]
        
        city_pop_heat_exposure_median = median(city_pop_heat_exposure_dist)
        
        # pop_category_median_heat = values(city_pop_mask)[!is.na(values(city_pop_mask))]
        # pop_category_median_heat = median(pop_category_median_heat)
        # print(paste("pop_category_median_heat", pop_category_median_heat, sep = " "))
        
        # create classification matrix
        m <- cbind(from = c(-Inf, 0, city_pop_heat_exposure_median),
                   to = c(0, city_pop_heat_exposure_median, Inf),
                   becomes = c(0, 1, 2))
        
        city_pop_heat_exposure_class = reclassify(city_pop_heat_exposure, m)
        
        city_pop_heat_exposure_class = raster::mask(city_pop_heat_exposure_class,city_boundary)
        
        
        # prepare map
        
        pal_Grid <- colorNumeric("RdYlBu",
                                 values(city_lst_mask),
                                 na.color = "transparent",
                                 reverse = TRUE)
        
        pal_Grid_pop <- colorNumeric("RdYlBu",
                                     values(city_pop_mask),
                                     na.color = "transparent",
                                     reverse = TRUE)
        
        pal_Grid_pop_exposure <- colorNumeric("RdYlBu",
                                              values(city_pop_heat_exposure),
                                              na.color = "transparent",
                                              reverse = TRUE)
        
        pal_Grid_pop_exposure_class <- colorFactor(c("green","yellow","red"),
                                                   levels = c(0,1,2),
                                                   na.color = "transparent")
        
        # plot map
        leafletProxy(mapId = "Map_plot_pop", data = filtereData())  %>%
            # Base groups
            addTiles(group = "OSM (default)") %>%
            addTiles() %>%
            clearControls() %>%
            clearShapes() %>%
            # plot boundary
            addPolygons(data = city_boundary,
                        group = "Administrative boundaries",
                        stroke = TRUE, color = "black", weight = 3,dashArray = "3",
                        smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.3,
                            bringToFront = TRUE),
                        label = city_boundary$city_name,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            # plot Population distribution raster
            addRasterImage(city_pop_mask,
                           colors = pal_Grid_pop,
                           opacity = 0.7,
                           group = "Population count",
                           maxBytes = 8 * 1024 * 1024) %>%
            # Legend for population count
            addLegend(pal = pal_Grid_pop,
                      values = ~values(city_pop_mask),
                      opacity = 0.9,
                      title = "Population count (per km2)",
                      position = "bottomright") %>%
            # plot Heat distribution raster
            addRasterImage(city_lst_mask,
                           colors = pal_Grid,
                           opacity = 0.7,
                           group = "Land Surface Temperature",
                           maxBytes = 8 * 1024 * 1024) %>%
            # Legend for Land surface temperature raster
            addLegend(pal = pal_Grid,
                      values = ~values(city_lst_mask),
                      opacity = 0.9,
                      title = "Land Surface Temperature (°C)",
                      position = "bottomleft",
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>%
        # plot Population exposure class raster
        addRasterImage(city_pop_heat_exposure_class,
                       colors = pal_Grid_pop_exposure_class,
                       opacity = 1,
                       group = "Population exposure",
                       maxBytes = 8 * 1024 * 1024) %>%
            # Legend for population exposure class
            addLegend(colors = c("green", "yellow", "red"),
                      labels = c("Low", "Moderate", "High"),
                      # pal = pal_Grid_pop_exposure_class,
                      # values = c(0,1,2),
                      opacity = 0.7,
                      title = "Population exposure",
                      position = "topright") %>%
            addLayersControl(
                overlayGroups = c("Population count",
                                  "Land Surface Temperature",
                                  "Population exposure"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        ### Sector plot ----
        
        city_amenity_sector_exposure = city_amenity %>% 
            group_by(gcom_sector_name) %>% 
            summarise(nb_amenities = n(),
                      lst_min = min(exposure_lst_mean),
                      lst_mean = mean(exposure_lst_mean),
                      lst_max =  max(exposure_lst_mean),
                      # nb_exposed_amenities_amenity_threshold = length(exposure_lst_mean[exposure_lst_mean>heat_threshold_value]),
                      nb_exposed_amenities_amenity_threshold = length(deviation_from_threshold[deviation_from_threshold>0]),
                      deviation_amenity_threshold = mean(deviation_from_threshold))  
            # mutate(exposure_class =
            #            case_when(deviation_amenity_threshold <= 0 ~ "Low", 
            #                      deviation_amenity_threshold <= 5 ~ "Moderate",
            #                      deviation_amenity_threshold > 5 ~ "High")
            # ) %>% 
            # mutate(exposure_color =
            #            case_when(exposure_class == "Low" ~ "green", 
            #                      exposure_class == "Moderate" ~ "orange",
            #                      exposure_class == "High" ~ "red")
            # ) %>% 
            # arrange(desc(deviation_amenity_threshold))
        
        city_amenity_sector_exposure = city_amenity_sector_exposure %>% 
            mutate(percent_exposed_amenities =round((nb_exposed_amenities_amenity_threshold/nb_amenities)*100,2)) %>% 
            mutate(exposure_class =
                       case_when(percent_exposed_amenities <= 50 ~ "Low", 
                                 percent_exposed_amenities < 75 ~ "Moderate",
                                 percent_exposed_amenities >= 75 ~ "High")
            ) %>% 
            mutate(exposure_color =
                       case_when(exposure_class == "Low" ~ "green", 
                                 exposure_class == "Moderate" ~ "yellow",
                                 exposure_class == "High" ~ "red")
            ) %>% 
            arrange(desc(percent_exposed_amenities))
        
        # deviation 
        # city_amenity_sector_exposure$gcom_sector_name <- factor(city_amenity_sector_exposure$gcom_sector_name, 
        #                                                         levels = unique(city_amenity_sector_exposure$gcom_sector_name)[order(city_amenity_sector_exposure$deviation_amenity_threshold, decreasing = TRUE)])
        # percent of exposed amenities
        city_amenity_sector_exposure$gcom_sector_name <- factor(city_amenity_sector_exposure$gcom_sector_name, 
                                                                levels = unique(city_amenity_sector_exposure$gcom_sector_name)[order(city_amenity_sector_exposure$percent_exposed_amenities, decreasing = TRUE)])
        
        
        exposure_color = city_amenity_sector_exposure$exposure_color
        
        print(city_amenity_sector_exposure)
        
        output$Sector_plot <- renderPlotly({
            
            # fig = city_amenity_sector_exposure %>%
            #     arrange(desc(deviation_amenity_threshold)) %>%
            #     plot_ly() %>%
            #     add_trace(x = ~gcom_sector_name,
            #               y = ~deviation_amenity_threshold,
            #               type = "bar",
            #               orientation = "v",
            #               marker = list(color = exposure_color),
            #               text = ~paste("Deviation ratio: ", deviation_amenity_threshold, '<br>Sector name:', gcom_sector_name)) %>%
            #     layout(yaxis = list(title = 'Heat deviation ratio from all amenities (%)'),
            #            xaxis = list(title = ''),
            #            barmode = 'stack',
            #            legend = list(orientation = 'h', x = 0.2, y = -0.5))
            
            fig = city_amenity_sector_exposure %>%
              arrange(desc(percent_exposed_amenities)) %>%
              plot_ly() %>%
              add_trace(x = ~gcom_sector_name,
                        y = ~percent_exposed_amenities,
                        type = "bar",
                        orientation = "v",
                        marker = list(color = exposure_color),
                        text = ~paste("Percent of exposed amenities: ", percent_exposed_amenities, 
                                      '<br>Sector name:', gcom_sector_name)) %>%
              layout(yaxis = list(title = 'Percent of exposed amenities (%)'),
                     xaxis = list(title = ''),
                     # barmode = 'stack',
                     legend = list(orientation = 'h'))
            
            
            fig
        })
        
        # population category exposure statistics table ----
        
        pop_categories = c("Children","Elderly","Women")
        
        pop_exposure_stat = data.frame(
          Selected.city = rep(selected_city,3),
          Population.category = pop_categories)
        
        # All population exposure stat
        
        city_pop_all = read.pop.category(pop_category = "All",
                                         city_boundary = city_boundary,
                                         selected_city = selected_city,
                                         data_source = "local")
        city_pop_all[is.na(city_pop_all)] <- 0
        
        city_population_sum = sum(values(city_pop_all))
        
        # population exposure stat by populualtion category
        
        for(i in 1:length(pop_categories)){
          pop_category = pop_categories[i]
          
          city_pop_category = read.pop.category(pop_category = pop_category,
                                                city_boundary = city_boundary,
                                                selected_city = selected_city,
                                                data_source = "local")
          city_pop_category[is.na(city_pop_category)] <- 0
          
          pop_category_count = sum(values(city_pop_category))
          pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Population count"] = round(pop_category_count,0)
          pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Population percent"] = round(sum(values(city_pop_category))/city_population_sum * 100,2)
          
          
          # create exposed population raster
          city_pop_category_exposure = city_pop_category * city_lst_mask_threshold
          city_pop_category_exposure[is.na(city_pop_category_exposure)] <- 0
          city_pop_category_exposed_count = sum(values(city_pop_category_exposure))
          pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Number of exposed population"] = round(city_pop_category_exposed_count,0)
          pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Percent of exposed population from population category"] = round(city_pop_category_exposed_count/pop_category_count * 100,2)
          pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Percent of exposed population from all population"] = round(city_pop_category_exposed_count/city_population_sum * 100,2)
        }
        
        # # population group plot ----
        output$Sector_plot_pop <- renderPlotly({

          # fig = pop_exposure_stat %>% 
          #   arrange(desc(`Percent of exposed population from all population`)) %>% 
          #   plot_ly() %>% 
          #   add_trace(x = ~Population.category,
          #             y = ~`Percent of exposed population from all population`, 
          #             type = "bar",
          #             orientation = "v",
          #             # marker = list(color = 'blue'),
          #             text = ~paste("Population percent: ",`Population percent`, 
          #                           '<br>Percent of exposed population from all population:', `Percent of exposed population from all population`)) %>% 
          #   layout(yaxis = list(title = 'Percent of exposed persons by category (%)'),
          #          xaxis = list(title = 'Population categories'))
          
          fig = pop_exposure_stat %>% 
              arrange(desc(`Percent of exposed population from all population`)) %>% 
              plot_ly() %>% 
              add_trace(x = ~Population.category,
                        y = ~`Percent of exposed population from all population`, 
                        type = "bar",
                        orientation = "v",
                        name = 'Percent of exposed persons from <br> (compared to all population)',
                        text = ~paste("Population percent: ",`Population percent`, 
                                      '<br>Percent of exposed population from all population:', `Percent of exposed population from all population`)) %>% 
              add_trace(x = ~Population.category,
                        y = ~`Percent of exposed population from population category`, 
                        type = "bar",
                        orientation = "v",
                        name = 'Percent of exposed population <br> (compared to population category)',
                        text = ~paste("Population percent: ",`Population percent`, 
                                      '<br>Percent of exposed population from all population:', `Percent of exposed population from all population`)) %>% 
              layout(yaxis = list(title = 'Percent of exposed persons by category (%)'),
                     xaxis = list(title = 'Population categories'),
                     barmode = 'group')
          
          fig
        })
        
        ### Main indicators ----
        
        selected_city_lst_value = round(unique(city_amenity$city_lst_avg),2)
        
        
        # Amenity average heat value ----
        selected_amenities_avg_heat = round(mean(city_amenity$exposure_lst_mean),2)
        output$selected_amenities_avg_heat <- renderText({ 
            paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", selected_amenities_avg_heat, "°C")
        })
        
        # Selected amenities deviation heat ----
        selected_amenities_deviation_heat_value = round(mean(city_amenity$heat_dev_from_amenities),2) 
        
        deviation_sign = check_value(value= selected_amenities_deviation_heat_value)
        
        output$selected_amenities_deviation_heat_value <- renderText({ 
            paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", deviation_sign, selected_amenities_deviation_heat_value, "°C")
        })
        
        # Selected amenities deviation ratio ----
        selected_amenities_deviation_heat_ratio = round((selected_amenities_avg_heat/city_amenity_avg_heat * 100)-100,2)
        
        ratio_sign = check_value(value= selected_amenities_deviation_heat_ratio)
        
        output$selected_amenities_deviation_heat_ratio <- renderText({ 
            paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>","(",ratio_sign ,selected_amenities_deviation_heat_ratio, "%)")
        })
        
        # percent of exposed population ----
        pop_exposure_ratio = round(cellStats(city_pop_heat_exposure, sum)/cellStats(city_pop_mask, sum),2)*100
        output$pop_exposure_ratio <- renderText({
            paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", pop_exposure_ratio, "%")
        })
        
        # amenity exposure narrative summary ----
        
        # remove "residential" and "others" from sector exposure table
        city_amenity_sector_exposure = city_amenity_sector_exposure %>% 
            filter(!gcom_sector_name %in% c("Other","Residential")) %>% 
            mutate(percent_exposed_amenities = round(nb_exposed_amenities_amenity_threshold/nb_amenities,2)*100)
            


        # plot narrative 
        output$amenity_exposure_narrative <- renderText({
            paste("<center>","<font size=5px; weight=300; color=\"#454545\"><b>",
                  "<font color=\"#1E90FF\"><b>", " ", "<br>",
                  "<font color=\"#454545\"><b>","In",
                  "<font color=\"#1E90FF\"><b>", selected_city,
                  "<font color=\"#454545\"><b>",",",
                  "<font color=\"#1E90FF\"><b>", round(mean(city_amenity_sector_exposure$percent_exposed_amenities),2), 
                  "<font color=\"#454545\"><b>","% of sites of the selected amenity sector(s)",
                  "<font color=\"#454545\"><b>","are situated in area of above the selected heat threshold", 
                  "<font color=\"#1E90FF\"><b>", "(",input$heat_threshold, "°C)", "<br>",
                  "<font color=\"#1E90FF\"><b>", " ", "<br>",
                  "<font color=\"#454545\"><b>","The most heat exposed sectors are",
                  "<font color=\"#1E90FF\"><b>", city_amenity_sector_exposure$gcom_sector_name[1], 
                  "<font color=\"#454545\"><b>","and", 
                  "<font color=\"#1E90FF\"><b>", city_amenity_sector_exposure$gcom_sector_name[2],
                  "<font color=\"#454545\"><b>","with", 
                  "<font color=\"#1E90FF\"><b>", city_amenity_sector_exposure[1, "percent_exposed_amenities"],"%",
                  "<font color=\"#454545\"><b>","and",
                  "<font color=\"#1E90FF\"><b>", city_amenity_sector_exposure[2, "percent_exposed_amenities"],"%",
                  "<font color=\"#454545\"><b>","of sites, respectively, exposed to heat above this temperature.")
        })
        
        
        
        ### plot table ----
        
        city_amenity_sector_exposure_plot = city_amenity_sector_exposure %>% 
            mutate(lst_min = round(lst_min,2),
                   lst_mean = round(lst_mean,2),
                   lst_max = round(lst_max,2),
                   deviation_amenity_threshold = round(deviation_amenity_threshold,2)) %>% 
            dplyr::select("Sector name" = gcom_sector_name,
                          "Number of amenities" = nb_amenities,
                          "Min heat value" = lst_min,
                          "Average heat value" = lst_mean,
                          "Max heat value" = lst_max,
                          "Number of exposed amenities" = nb_exposed_amenities_amenity_threshold,
                          "Percent of exposed amenties" = percent_exposed_amenities,
                          "average deviation ratio" = deviation_amenity_threshold,
                          "Exposure category" = exposure_class)
        
        print("city_amenity_sector_exposure_plot")
        plot(city_amenity_sector_exposure_plot)
        
        # amenity sectors exposure table ----
        output$table_amenity <- DT::renderDataTable(
            DT::datatable(city_amenity_sector_exposure_plot, 
                          options = list(pageLength = 25)) %>% formatStyle(
                              # "average deviation ratio", target = "row", 
                              # backgroundColor = styleInterval(c(0, 5,20), c("lightgreen", "yellow", "orange", "red")),
                              "Percent of exposed amenties", target = "row", 
                              # backgroundColor = styleInterval(c(0, 50, 75), c("lightgreen", "yellow", "orange", "red")),
                              backgroundColor = styleInterval(c(50, 74.9), c("green", "yellow", "red")),
                              fontWeight = 'bold')
            
        )
        
        # output data to download
        output$download_amenity_exposure <- downloadHandler(
            filename = function() {
                paste(selected_city,"-data-amenity-exposure-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
                write.csv(city_amenity, file)
            }
        )
        output$download_sector_exposure <- downloadHandler(
            filename = function() {
                paste(selected_city,"-data-sectors-exposure-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
                write.csv(city_amenity_sector_exposure_plot, file)
            }
        )

        
        # population category exposure table ----

        output$table_population <- DT::renderDataTable(
            DT::datatable(pop_exposure_stat))
        
        # population exposure narrative summary ----
        output$pop_exposure_narrative <- renderText({
          paste("<center>","<font size=5px; weight=300; color=\"#454545\"><b>",
                "<font color=\"#1E90FF\"><b>", " ", "<br>",
                "<font color=\"#1E90FF\"><b>", pop_exposure_ratio,  
                "<font color=\"#454545\"><b>","% of",
                "<font color=\"#1E90FF\"><b>", input$Category_pop,
                "<font color=\"#454545\"><b>","residential population in",
                "<font color=\"#1E90FF\"><b>", selected_city, 
                "<font color=\"#454545\"><b>","are situated in areas with extreme temperature above", 
                "<font color=\"#1E90FF\"><b>", heat_threshold_value, "°C", "<br>",
                "<font color=\"#1E90FF\"><b>", " ", "<br>")
        })
        
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)

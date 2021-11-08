#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

# define path
# city_boundary_path = paste("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/boundary_CHL_vitacura_mapped_geom.geojson")
city_boundary_path = paste("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/PHL_makati.geojson")
# city_boundary_path = paste("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/boundary_AUS_hobart_mapped.geojson")

# read the data
boundary <- st_read(city_boundary_path,
                    quiet = TRUE)

city_boundary = boundary

########### Load amenity data ----

# amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/amenity_exposure_heat.csv", 
#                                 encoding = "UTF-8")
amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/PHL-Makati/amenity_exposure_heat.csv",
                                encoding = "UTF-8")
# amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/AUS-Hobart/amenity_exposure_heat.csv",
#                                 encoding = "UTF-8")

amenity_exposure_lst = amenity_exposure_lst %>% 
    filter(exposure_lst_mean > -10)

# recode gcom sectors
amenity_exposure_lst =amenity_exposure_lst  %>% 
    mutate(gcom_sector_name=recode(gcom_sector_name, 
                                   ` Law & Order`="Law & Order",
                                   `Law & Order`="Law & Order")) 

amenity_exposure_lst =amenity_exposure_lst  %>% 
    mutate(gcom_sector_name=recode(gcom_sector_name, 
                                   ` Society/community & culture`="Society/community & culture",
                                   `Society/community & culture`="Society/community & culture"))

# filter amenities located within city boundary

amenity_points <- st_as_sf(x = amenity_exposure_lst, 
                           coords = c("longitude", "latitude"),
                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

amenity_within_boundary <- st_intersection(city_boundary, amenity_points)

amenity_exposure_lst = amenity_exposure_lst %>% 
    filter(id %in% amenity_within_boundary$id)

# compute average heat of all amenities
city_amenity_avg_heat = mean(amenity_exposure_lst$exposure_lst_mean)
amenity_exposure_lst$heat_dev_from_amenities = amenity_exposure_lst$exposure_lst_mean - city_amenity_avg_heat


# define available cities
available_cities = unique(amenity_exposure_lst$city_name)
# define avialble sectors
available_amenity_sectors = unique(amenity_exposure_lst$gcom_sector_name)
# define avilable population categories
available_pop_categories = c("All", "Young (<20)", "Elderly (>60)", "Men", "Women")
available_pop_categories = available_amenity_sectors

# define slider threshold values
slider_min_heat = round(min(amenity_exposure_lst$exposure_lst_mean))
slider_max_heat = round(max(amenity_exposure_lst$exposure_lst_mean))
slider_value_heat = round(mean(amenity_exposure_lst$exposure_lst_mean))
slider_step = 1

# selected_city_id = "CHL-Vitacura"
selected_city_id = "PHL-Makati"
# selected_city_id = "AUS-Hobart"

### read land surface temperature ----

# define path
city_lst_path = "/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/hazards/heat/land_surface_temperature/lst_mean_lst_PHL-Makati.tif"

# collect raster data
city_lst = raster(city_lst_path)
# mask raster based on administrative boundaries
city_lst_mask = raster::mask(city_lst,
                             city_boundary)

# read population

read.pop.category =  function(pop_category){
    
    if(pop_category == "All"){
        data_path = "pop"
    } else if(pop_category == "Children"){
        data_path = "pop_children"
    } else if(pop_category == "Elderly"){
        data_path = "pop_elderly"
    } else if(pop_category == "Women"){
        data_path = "pop_women"
    }
    
    # define path
    city_pop_path = paste("/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/population/",
                          data_path,
                          "_PHL-Makati.tif",
                          sep = "")
    
    print(city_pop_path)
    # collect raster data
    city_pop = raster(city_pop_path)
    city_pop[is.na(city_pop)] <- 0
    # mask raster based on administrative boundaries
    city_pop_mask = raster::mask(city_pop,city_boundary)
    
    
    
    return(city_pop_mask)
}

# define path
city_pop_path = "/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/population/pop_PHL-Makati.tif"
# collect raster data
city_pop = raster(city_pop_path)
# mask raster based on administrative boundaries
city_pop_mask = raster::mask(city_pop,city_boundary)

# define path
city_pop_path = "/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/population/pop_children_PHL-Makati.tif"
# collect raster data
city_pop = raster(city_pop_path)
# mask raster based on administrative boundaries
city_pop_children_mask = raster::mask(city_pop,city_boundary)

# define path
city_pop_path = "/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/population/pop_elderly_PHL-Makati.tif"
# collect raster data
city_pop = raster(city_pop_path)
# mask raster based on administrative boundaries
city_pop_elderly_mask = raster::mask(city_pop,city_boundary)

# define path
city_pop_path = "/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/population/pop_women_PHL-Makati.tif"
# collect raster data
city_pop = raster(city_pop_path)
# mask raster based on administrative boundaries
city_pop_women_mask = raster::mask(city_pop,city_boundary)



ui = navbarPage("City-Dashboard",
                id = "active_tab",
                
                ### Amenity exposure panel ----
                tabPanel("Amenity exposure",
                         
                         ### First row ----
                         fluidRow(
                             ### Specify filters ----
                             column(3,
                                    
                                    
                                    ### Specify scale 
                                    selectInput(inputId = "City", 
                                                label = "Select your city", 
                                                choices = available_cities,
                                                selected = "Vitacura",
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
                                    
                                    ### Specify Datasource 
                                    selectInput(inputId = "Sector", 
                                                label = "Select your sectors of interest", 
                                                choices = available_amenity_sectors,
                                                selected = available_amenity_sectors,
                                                multiple = TRUE,
                                                width = '100%'
                                    ),
                                    
                                    # h4("City average heat value"),
                                    ### plot kpi
                                    # textOutput("selected_city")
                                    # span(textOutput("selected_city"), style="color:red")
                                    # verbatimTextOutput("selected_city")
                                    
                                    sliderInput(inputId = 'heat_threshold', 
                                                label = 'Select heat value', 
                                                min = slider_min_heat, 
                                                max = slider_max_heat, 
                                                value = slider_value_heat, 
                                                step = slider_step,
                                                width = '100%'),
                                    
                                    # htmlOutput("selected_city_heat_value"),
                                    
                                    h4("Amenity average heat value:"),
                                    htmlOutput("selected_amenities_avg_heat"),
                                    
                                    h4("Selected amenities average deviation:"),
                                    htmlOutput("selected_amenities_deviation_heat_value"),
                                    
                                    h4("Selected amenities deviation ratio:"),
                                    htmlOutput("selected_amenities_deviation_heat_ratio"),
                                    
                                    
                                    # htmlOutput("selected_heat_threshold")
                                    
                                    
                                    
                             ),
                             ### Specify plots ----
                             column(8, 
                                    div(style = "background-color: red; width: 100%; height: 100%;"),
                                    tabsetPanel(type = "tabs",
                                                ### Map plot 
                                                tabPanel("Amenities exposure", leafletOutput("Map_plot", height = 600)),
                                                ### timeseirs plot 
                                                tabPanel("Sector exposure", plotlyOutput("Sector_plot", height = 600))
                                    )
                             )
                         ),
                         
                         
                         ## Second row ----
                         fluidRow(
                             column(12, offset = 0, h4("Observations"), DT::dataTableOutput("table_amenity"))
                         )
                ),
                ## Population exposure panel ----
                tabPanel("Population exposure",
                         
                         ### First row ----
                         fluidRow(
                             ### Specify filters ----
                             column(3,
                                    
                                    
                                    ### Specify scale
                                    selectInput(inputId = "City_pop",
                                                label = "Select your city",
                                                choices = available_cities,
                                                selected = "Makati",
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Hazard
                                    selectInput(inputId = "Hazard_pop",
                                                label = "Select hazard",
                                                choices = "Heat",
                                                selected = "Heat",
                                                multiple = FALSE,
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Hazard
                                    selectInput(inputId = "Period_pop",
                                                label = "Select period of interest",
                                                choices = "2020-2021",
                                                selected = "2020-2021",
                                                multiple = FALSE,
                                                width = '100%'
                                    ),
                                    
                                    ### Specify population of interest
                                    radioButtons(inputId = "Category_pop",
                                                 label = "Select population category",
                                                 choices = c("All", "Elderly", "Children", "Women"),
                                                 selected = "All"
                                    ),
                                    
                                    ### Specify Datasource
                                    # selectInput(inputId = "Sector_pop",
                                    #             label = "Select population category",
                                    #             choices = available_pop_categories,
                                    #             selected = "All",
                                    #             multiple = FALSE,
                                    #             width = '100%'
                                    # ),
                                    
                                    sliderInput(inputId = 'heat_threshold_pop',
                                                label = 'Select heat value',
                                                min = slider_min_heat,
                                                max = slider_max_heat,
                                                value = slider_value_heat,
                                                step = slider_step,
                                                width = '100%'),
                                    
                                    # htmlOutput("selected_city_heat_value"),
                                    
                                    # h4("City average heat value:"),
                                    # htmlOutput("selected_amenities_avg_heat"),
                                    # 
                                    # h4("Selected category average deviation:"),
                                    # htmlOutput("selected_amenities_deviation_heat_value"),
                                    # 
                                    # h4("Selected category deviation ratio:"),
                                    # htmlOutput("selected_amenities_deviation_heat_ratio")
                                    
                                    
                                    # htmlOutput("selected_heat_threshold")
                                    
                                    
                                    
                             ),
                             ### Specify plots ----
                             column(8,
                                    div(style = "background-color: red; width: 100%; height: 100%;"),
                                    tabsetPanel(type = "tabs",
                                                ### Map plot
                                                tabPanel("population exposure map", leafletOutput("Map_plot_pop", height = 600)),
                                                ### timeseirs plot
                                                tabPanel("Vulnerability", plotlyOutput("Sector_plot_pop", height = 600))
                                    )
                             )
                         ),
                         
                         
                         # ## Second row ----
                         # fluidRow(
                         #     column(12, offset = 0, h4("Observations"), DT::dataTableOutput("table_population"))
                         # )
                )
)


server <- function(input, output, session) {
    
    
    ### leaflet map definition ----
    ### Country
    # output$Map_plot <- renderLeaflet({
    #     leaflet() %>%
    #         setView(lng = 2.846874031249995, lat = 46.99079193796217, zoom = 1)
    # })
    
    # selected_city = input$City
    # selected_sector = input$Sector
    # 
    # amenity_city_selected = amenity_exposure_lst %>% 
    #     filter(city_name == selected_city,
    #            sector_name %in% selected_sector)
    
    # Reactive expression for the data subsetted to what the user selected
    filtereData <- reactive({
        amenity_exposure_lst[amenity_exposure_lst$city_name == input$City & amenity_exposure_lst$gcom_sector_name %in% input$Sector, ]
    })
    
    
    
    # output$Map_plot <- renderLeaflet({
    #     # Use leaflet() here, and only include aspects of the map that
    #     # won't need to change dynamically (at least, not unless the
    #     # entire map is being torn down and recreated).
    #     leaflet(filtereData()) %>% addTiles() %>%
    #         fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    # })
    
    output$Map_plot <- renderLeaflet({
        leaflet(filtereData()) %>% 
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })
    
    output$Map_plot_pop <- renderLeaflet({
        leaflet(filtereData()) %>% 
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })
    
    # output$Raster_plot <- renderLeaflet({
    #     # Use leaflet() here, and only include aspects of the map that
    #     # won't need to change dynamically (at least, not unless the
    #     # entire map is being torn down and recreated).
    #     leaflet(filtereData()) %>% addTiles() %>%
    #         fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    # })
    
    
    #update sector list depending on selected city
    # observeEvent(
    #     input$City,
    #     updateSelectInput(session, "Sector", "Sector",
    #                       choices = unique(amenity_exposure_lst[amenity_exposure_lst$city_name == input$City, "sector_name"]) 
    #                       # choices = amenity_exposure_lst %>%
    #                       #     filter(city_name == input$City) %>%
    #                       #     distinct(sector_name) %>%
    #                       #     pull(sector_name)
    #     )
    # )
    
    ### reactive plots definition ----
    observe({
        
        ### Amenity level ----
        
        
        # filter data
        selected_city = input$City
        selected_sector = input$Sector
        
        # filter boundary data
        city_boundary = boundary %>% 
            filter(city_id == selected_city_id)
        
        # filter amenity
        city_amenity = amenity_exposure_lst %>%
            filter(city_name == selected_city,
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
        
        pal_amenity_class <- colorFactor(c("green","yellow","red"),
                                         levels =c("0-Low","1-Moderate","2-High"))
        
        # prepqre plot
        pal_amenity <- colorNumeric("RdYlBu", 
                                    domain = city_amenity$exposure_lst_mean,
                                    reverse = TRUE)
        
        labels_amenity <- sprintf("<strong>%s</strong> %s <br/><strong>%s:</strong> %s %s<br/><strong>%s:</strong> %s<br/><strong>%s:</strong> %s",
                                  "Name",city_amenity$city_name, 
                                  "Land surface temp", round(city_amenity$exposure_lst_mean, 2), "celsus",
                                  "Amenity type", city_amenity$feature_value,
                                  "Amenity sector", city_amenity$gcom_sector_name) %>% 
            lapply(htmltools::HTML)
        
        pal_amenity_heat_deviation <- colorNumeric("RdYlBu", 
                                                   domain = city_amenity$heat_dev_from_amenities,
                                                   reverse = TRUE)
        
        pal_amenity_heat_deviation_threshold <- colorNumeric("RdYlBu", 
                                                             domain = vec_deviation_from_threshold,
                                                             reverse = TRUE)
        
        pal_Grid <- colorNumeric("RdYlBu", 
                                 values(city_lst_mask),
                                 na.color = "transparent",
                                 reverse = TRUE)
        
        # plot map
        leafletProxy(mapId = "Map_plot", data = filtereData())  %>%
            # Base groups
            addTiles(group = "OSM (default)") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
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
            # Plot amenities with heat value
            addCircleMarkers(~longitude, ~latitude,
                             radius = 4,
                             color = "black",
                             fillColor = ~pal_amenity_heat_deviation(heat_dev_from_amenities),
                             stroke = TRUE,
                             fillOpacity = 0.7,
                             weight = 1,
                             popup = ~as.character(city_name),
                             label = labels_amenity,
                             group = "Amenity heat deviation") %>%
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
            addLegend(pal = pal_amenity, values = ~exposure_lst_mean, opacity = 0.9,
                      title = "Land Surface Temperature",
                      position = "bottomright",
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>% 
            # Legend for amenity heat deviation
            # addLegend(pal = pal_amenity_heat_deviation, 
            #           values = ~heat_dev_from_amenities, 
            #           opacity = 0.9,
            #           title = "Amenity heat deviation",
            #           position = "bottomleft",
            #           labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>%
            # Legend for amenity heat deviation ratio from threshold class
            addLegend(pal = pal_amenity_class,
                      values = ~vec_exposure_class,
                      opacity = 0.9,
                      title = "Amenity exposure level",
                      position = "topright",
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>%
            addLayersControl(
                baseGroups = c("OSM (default)", "CartoDB"),
                overlayGroups = c("Amenity exposure value", 
                                  "Land Surface Temperature",
                                  "Administrative boundaries",
                                  "Amenity heat deviation",
                                  "Amenity exposure class"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        ### Population ----
        
        # filter data
        selected_city = input$City_pop
        selected_sector = input$Sector_pop
        
        # filter heat threshold
        city_lst_mask_threshold = city_lst_mask
        
        # get heat threshold
        heat_threshold_value = input$heat_threshold_pop
        
        values(city_lst_mask_threshold)[values(city_lst_mask_threshold) <= heat_threshold_value] = 0
        values(city_lst_mask_threshold)[values(city_lst_mask_threshold) > heat_threshold_value] = 1
        
        
        # filter population category
        selected_population = input$Category_pop
        
        # get filtered population category
        city_pop_mask = read.pop.category(pop_category = selected_population)
        
        # create exposed population raster
        city_pop_heat_exposure = city_pop_mask * city_lst_mask_threshold
        
        # filter boundary data
        city_boundary = boundary %>% 
            filter(city_id == selected_city_id)
        
        pal_Grid_pop <- colorNumeric("RdYlBu", 
                                     values(city_pop_mask),
                                     na.color = "transparent",
                                     reverse = TRUE)
        
        pal_Grid_pop_exposure <- colorNumeric("RdYlBu", 
                                              values(city_pop_heat_exposure),
                                              na.color = "transparent",
                                              reverse = TRUE)
        
        # plot map
        leafletProxy(mapId = "Map_plot_pop", data = filtereData())  %>%
            # Base groups
            addTiles(group = "OSM (default)") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
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
                           group = "Population",
                           maxBytes = 8 * 1024 * 1024) %>%
            # Legend for population count
            addLegend(pal = pal_Grid_pop, 
                      values = ~values(city_pop_mask), 
                      opacity = 0.9,
                      title = "Population",
                      position = "bottomright") %>% 
            # plot Population distribution raster
            addRasterImage(city_pop_heat_exposure, 
                           colors = pal_Grid_pop_exposure, 
                           opacity = 0.7,
                           group = "Population exposure",
                           maxBytes = 8 * 1024 * 1024) %>%
            # Legend for population count
            addLegend(pal = pal_Grid_pop_exposure, 
                      values = ~values(city_pop_heat_exposure), 
                      opacity = 0.9,
                      title = "Population exposure",
                      position = "topright") %>% 
            addLayersControl(
                baseGroups = c("OSM (default)", "CartoDB"),
                overlayGroups = c("Administrative boundaries",
                                  "Population",
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
                      nb_exposed_amenities_amenity_threshold = length(exposure_lst_mean[exposure_lst_mean>heat_threshold_value]),
                      deviation_amenity_threshold = mean(deviation_from_threshold)) %>% 
            mutate(exposure_class =
                       case_when(deviation_amenity_threshold <= 0 ~ "Low", 
                                 deviation_amenity_threshold <= 5 ~ "Moderate",
                                 deviation_amenity_threshold > 5 ~ "High")
            ) %>% 
            mutate(exposure_color =
                       case_when(exposure_class == "Low" ~ "green", 
                                 exposure_class == "Moderate" ~ "orange",
                                 exposure_class == "High" ~ "red")
            ) %>% 
            arrange(desc(deviation_amenity_threshold))
        
        city_amenity_sector_exposure$gcom_sector_name <- factor(city_amenity_sector_exposure$gcom_sector_name, 
                                                                levels = unique(city_amenity_sector_exposure$gcom_sector_name)[order(city_amenity_sector_exposure$deviation_amenity_threshold, decreasing = TRUE)])
        
        exposure_color = city_amenity_sector_exposure$exposure_color
        
        output$Sector_plot <- renderPlotly({
            
            fig = city_amenity_sector_exposure %>% 
                arrange(desc(deviation_amenity_threshold)) %>% 
                plot_ly() %>% 
                add_trace(x = ~gcom_sector_name, 
                          y = ~deviation_amenity_threshold, 
                          type = "bar",
                          orientation = "v",
                          marker = list(color = exposure_color),
                          text = ~paste("Deviation ratio: ", deviation_amenity_threshold, '<br>Sector name:', gcom_sector_name)) %>% 
                layout(yaxis = list(title = 'Heat deviation ratio from all amenities (%)'), 
                       xaxis = list(title = ''),
                       barmode = 'stack',
                       legend = list(orientation = 'h', x = 0.2, y = -0.5))
            
            fig
        })
        
        output$Sector_plot_pop <- renderPlotly({
            
            fig = city_amenity_sector_exposure %>% 
                arrange(desc(deviation_amenity_threshold)) %>% 
                plot_ly() %>% 
                add_trace(x = ~gcom_sector_name, 
                          y = ~deviation_amenity_threshold, 
                          type = "bar",
                          orientation = "v",
                          marker = list(color = exposure_color),
                          text = ~paste("Deviation ratio: ", deviation_amenity_threshold, '<br>Sector name:', gcom_sector_name)) %>% 
                layout(yaxis = list(title = 'Heat deviation ratio from all amenities (%)'), 
                       xaxis = list(title = ''),
                       barmode = 'stack',
                       legend = list(orientation = 'h', x = 0.2, y = -0.5))
            
            fig
        })
        
        ### Main indicators ----
        
        selected_city_lst_value = round(unique(city_amenity$city_lst_avg),2)
        
        # # City average heat value
        # output$selected_city_heat_value <- renderText({ 
        #     paste("<font size=3px>", "City average heat value: ","<font size=5px; weight=500; color=\"#0000FF\"><b>", selected_city_lst_value, "°C")
        # })
        
        # Amenity average heat value
        selected_amenities_avg_heat = round(mean(city_amenity$exposure_lst_mean),2)
        output$selected_amenities_avg_heat <- renderText({ 
            paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", selected_amenities_avg_heat, "°C")
        })
        
        # Selected amenities deviation heat
        selected_amenities_deviation_heat_value = round(mean(city_amenity$heat_dev_from_amenities),2) 
        output$selected_amenities_deviation_heat_value <- renderText({ 
            paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", selected_amenities_deviation_heat_value, "°C")
        })
        
        # Selected amenities deviation ratio
        selected_amenities_deviation_heat_ratio = round((selected_amenities_avg_heat/city_amenity_avg_heat * 100)-100,2)
        output$selected_amenities_deviation_heat_ratio <- renderText({ 
            paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", selected_amenities_deviation_heat_ratio, "%")
        })
        
        # output$selected_heat_threshold <- renderText({ 
        #     paste("<font size=3px>", "Selected heat threshold: ","<font size=5px; weight=500; color=\"#0000FF\"><b>", heat_threshold_value, "%")
        # })
        
        
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
                          "average deviation ratio" = deviation_amenity_threshold,
                          "Exposure category" = exposure_class)
        
        
        output$table_amenity <- DT::renderDataTable(
            DT::datatable(city_amenity_sector_exposure_plot, 
                          options = list(pageLength = 25)) %>% formatStyle(
                              "average deviation ratio", target = "row", 
                              backgroundColor = styleInterval(c(0, 5,20), c("lightgreen", "yellow", "orange", "red")),
                              fontWeight = 'bold')
            
        )
        
        output$table_population <- DT::renderDataTable(
            DT::datatable(city_amenity_sector_exposure_plot, 
                          options = list(pageLength = 25)) %>% formatStyle(
                              "average deviation ratio", target = "row", 
                              backgroundColor = styleInterval(c(0, 5,20), c("lightgreen", "yellow", "orange", "red")),
                              fontWeight = 'bold')
            
        )
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)

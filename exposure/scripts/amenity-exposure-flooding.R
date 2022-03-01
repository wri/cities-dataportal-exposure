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


boundary <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/cities_boundaries.geojson",
                    quiet = TRUE)

# filter data
selected_city = "CHL-Vitacura"


# filter boundary data
city_boundary = boundary %>% 
  filter(city_id == selected_city)


### read flooding data  ----

read.flood = function(selected_city, city_boundary){
  
  city_flooding_path =  paste("./data/flooding/data_flooding_riverine_rp00100_1980_",
                              selected_city,
                              ".tif",
                              sep = "")
  
  # collect raster data
  city_flooding = raster(city_flooding_path)
  # mask raster based on administrative boundaries
  # city_flooding_mask = raster::mask(city_flooding,
  #                                   city_boundary)
  
  return(city_flooding)
  
}

city_flooding = read.flood(selected_city = selected_city,
                           city_boundary = city_boundary)


########### Load amenity data ----

amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/amenity_exposure_lst.csv",
                                encoding = "UTF-8")

# filter amenity
city_amenity = amenity_exposure_lst %>%
  filter(city_id == selected_city) 

# amenity exposure to flooding ------


city_amenity_flooding = extract(city_flooding, 
                                amenity_exposure_lst[,c("longitude","latitude")])


amenity_exposure_lst_flooding = amenity_exposure_lst

amenity_exposure_lst_flooding$flood_level = city_amenity_flooding

amenity_exposure_lst_flooding$flood_level[is.na(amenity_exposure_lst_flooding$flood_level)] = 0

write.csv(amenity_exposure_lst_flooding,
          "./github/cities-dataportal-exposure/dashboard/test/app_test/data/amenity_exposure_lst_flooding.csv")

########### plot----

filtereData = amenity_exposure_lst[amenity_exposure_lst$city_id == selected_city, ]
filtereData = amenity_exposure_lst_flooding[amenity_exposure_lst_flooding$city_id == selected_city, ]

# define numerical color palette
pal_amenity <- colorNumeric("RdYlBu", 
                            domain = city_amenity$exposure_lst_mean,
                            reverse = TRUE)

pal_amenity_flooding <- colorNumeric("Blues", 
                            domain = amenity_exposure_lst_flooding$flood_level,
                            reverse = FALSE,
                            na.color = "transparent")

# define ratser color palette for LST data
pal_Grid <- colorNumeric("RdYlBu", 
                         values(city_lst_mask),
                         na.color = "transparent",
                         reverse = TRUE)

# define raster color palette for flooding data
pal_flooding <- colorNumeric("Blues", 
                         values(city_flooding),
                         na.color = "transparent",
                         reverse = FALSE)

# define labels information
labels_amenity <- sprintf("<strong>%s</strong> %s <br/><strong>%s:</strong> %s %s<br/><strong>%s:</strong> %s<br/><strong>%s:</strong> %s",
                          "Name",city_amenity$city_name, 
                          "Land surface temp", round(city_amenity$exposure_lst_mean, 2), "celsus",
                          "Amenity type", city_amenity$feature_value,
                          "Amenity sector", city_amenity$gcom_sector_name) %>% 
  lapply(htmltools::HTML)


leaflet(filtereData) %>%
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
                   group = "Amenity exposure Heat") %>%
  # Plot amenities with flood value
  addCircleMarkers(~longitude, ~latitude,
                   radius = 4,
                   color = "black",
                   fillColor = ~pal_amenity_flooding(flood_level),
                   stroke = TRUE,
                   fillOpacity = 0.7,
                   weight = 1,
                   popup = ~as.character(city_name),
                   label = labels_amenity,
                   group = "Amenity exposure Flood") %>%
  # Legend for amenitu heat values
  addLegend(pal = pal_amenity, 
            values = ~exposure_lst_mean, 
            opacity = 0.9,
            title = "Amenity heat value",
            position = "bottomright",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>%
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
  addRasterImage(city_flooding, 
                 colors = "Blues", 
                 opacity = 0.7,
                 group = "Riverine flooding") %>%
  # Legend for flooding raster
  addLegend(pal = pal_flooding, 
            values = ~values(city_flooding), 
            opacity = 0.9,
            title = "Riverine flooding",
            position = "topleft",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "CartoDB"),
    overlayGroups = c("Amenity exposure Heat", 
                      "Amenity exposure Flood",
                      "Riverine flooding"),
    options = layersControlOptions(collapsed = FALSE))


### Sector plot flood ----

# filter amenity
city_amenity = amenity_exposure_lst_flooding %>%
  filter(city_id == selected_city) 

# get heat threshold
flood_threshold_value = 0.23

# compute deviation from threshold
city_amenity$deviation_from_threshold_flood = round((city_amenity$flood_level/flood_threshold_value*100)-100,2) 
vec_deviation_from_threshold_flood = city_amenity$deviation_from_threshold_flood

# print(paste("vec_deviation_from_threshold_flood:", vec_deviation_from_threshold_flood, sep = " "))

city_amenity[city_amenity$deviation_from_threshold_flood <= 0,"exposure_class_flood" ] = "0-Low"
city_amenity[city_amenity$deviation_from_threshold_flood > 0 & city_amenity$deviation_from_threshold_flood <= 10,"exposure_class_flood" ] = "1-Moderate"
city_amenity[city_amenity$deviation_from_threshold_flood > 10,"exposure_class_flood" ] = "2-High"
vec_exposure_class_flood = city_amenity$exposure_class_flood

city_amenity_sector_exposure_flood = city_amenity %>% 
  group_by(gcom_sector_name) %>% 
  summarise(nb_amenities = n(),
            flood_min = min(flood_level),
            flood_mean = mean(flood_level),
            flood_max =  max(flood_level),
            nb_exposed_amenities_amenity_threshold = length(deviation_from_threshold_flood[deviation_from_threshold_flood>0]),
            deviation_amenity_threshold = mean(deviation_from_threshold_flood))  


city_amenity_sector_exposure_flood = city_amenity_sector_exposure_flood %>% 
  mutate(percent_exposed_amenities =round((nb_exposed_amenities_amenity_threshold/nb_amenities)*100,2)) %>% 
  mutate(exposure_class =
           case_when(percent_exposed_amenities <= 50 ~ "Low", 
                     percent_exposed_amenities <= 75 ~ "Moderate",
                     percent_exposed_amenities > 75 ~ "High")
  ) %>% 
  mutate(exposure_color =
           case_when(exposure_class == "Low" ~ "green", 
                     exposure_class == "Moderate" ~ "orange",
                     exposure_class == "High" ~ "red")
  ) %>% 
  arrange(desc(percent_exposed_amenities))

city_amenity_sector_exposure_flood$gcom_sector_name <- factor(city_amenity_sector_exposure_flood$gcom_sector_name, 
                                                              levels = unique(city_amenity_sector_exposure_flood$gcom_sector_name)[order(city_amenity_sector_exposure_flood$deviation_amenity_threshold, decreasing = TRUE)])

exposure_color = city_amenity_sector_exposure_flood$exposure_color

fig = city_amenity_sector_exposure_flood %>%
  arrange(desc(city_amenity_sector_exposure_flood)) %>%
  plot_ly() %>%
  add_trace(x = ~gcom_sector_name,
            y = ~percent_exposed_amenities,
            type = "bar",
            orientation = "v",
            # marker = list(color = exposure_color),
            text = ~paste("Percent of exposed amenities: ", percent_exposed_amenities, '<br>Sector name:', gcom_sector_name)) %>%
  layout(yaxis = list(title = 'Percent of amenities exposed to flood hazard (%)'),
         xaxis = list(title = ''),
         barmode = 'stack',
         legend = list(orientation = 'h', x = 0.2, y = -0.5))


fig

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
library(leaflet.opacity)
library(leaflet.multiopacity)

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

########### Load population data ----

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
  
  
  # print(city_pop_path)
  # collect raster data
  city_pop = raster(city_pop_path)
  city_pop[is.na(city_pop)] <- 0
  # mask raster based on administrative boundaries
  city_pop_mask = raster::mask(city_pop,city_boundary)
  
  
  
  return(city_pop_mask)
}

selected_population = "All"

city_pop_mask = read.pop.category(pop_category = selected_population,
                                  city_boundary = city_boundary,
                                  selected_city = selected_city,
                                  data_source = "local")

# flood threshold layer -----
city_flooding_threshold = city_flooding

flooding_threshold_value = 0.2

values(city_flooding_threshold)[values(city_flooding_threshold) <= flooding_threshold_value] = 0
values(city_flooding_threshold)[values(city_flooding_threshold) > flooding_threshold_value] = 1

# create exposed population raster

city_flooding_threshold = resample(city_flooding_threshold, city_pop_mask, method = 'bilinear')
city_pop_flooding_exposure = city_pop_mask * city_flooding_threshold


# plot map -----

pal_pop <- colorNumeric("Greens",
                        values(city_pop_mask),
                        na.color = "transparent",
                        reverse = FALSE)

# define raster color palette for flooding data
pal_flooding <- colorNumeric("Blues", 
                             values(city_flooding),
                             na.color = "transparent",
                             reverse = FALSE)

pal_pop_exposure_flooding <- colorNumeric("Reds",
                                 values(city_pop_flooding_exposure),
                                 na.color = "transparent",
                                 reverse = FALSE)

pal_flooding_class <- colorFactor(c("blue","red"),
                              levels = c(0,1),
                              na.color = "transparent")


map = leaflet(city_boundary)  %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
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
  # layer 1
  # plot flooding distribution raster
  addRasterImage(city_flooding, 
                 colors = "Reds", 
                 opacity = 0.7,
                 group = "Riverine flooding",
                 layerId = "Riverine flooding") %>%
  # Legend for flooding raster
  addLegend(pal = pal_flooding, 
            values = ~values(city_flooding), 
            opacity = 0.9,
            title = "Riverine flooding",
            position = "topleft",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>%
  # layer 2
  # plot Population distribution raster
  addRasterImage(city_pop_mask,
                 colors = pal_pop,
                 opacity = 0.7,
                 group = "Population count",
                 maxBytes = 8 * 1024 * 1024,
                 layerId = "Population count") %>%
  # Legend for population count
  addLegend(pal = pal_pop,
            values = ~values(city_pop_mask),
            opacity = 0.9,
            title = "Population count",
            position = "bottomright") %>%
  # layer 3
  # plot Population exposure to flooding
  addRasterImage(city_pop_flooding_exposure,
                 colors = pal_pop_exposure_flooding,
                 opacity = 0.7,
                 group = "Population exposure to flooding",
                 maxBytes = 8 * 1024 * 1024,
                 layerId = "Population exposure to flooding") %>%
  # Legend for population exposure class
  addLegend(pal = pal_pop_exposure_flooding,
            values = ~values(city_pop_flooding_exposure),
            opacity = 0.7,
            title = "Population exposure to flooding",
            position = "bottomleft") %>%   
  addLayersControl(
    baseGroups = c("OSM (default)", "CartoDB"),
    overlayGroups = c("Riverine flooding",
                      "Population count",
                      "Population exposure to flooding"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addOpacityControls(group = c("Riverine flooding",
                               "Population count",
                               "Population exposure to flooding"))

map

# percent of exposed population to flooding ----
pop_exposure_ratio_flooding = round(cellStats(city_pop_flooding_exposure, sum)/cellStats(city_pop_mask, sum),2)*100

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
  
  print(city_lst_path)
  
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
  
  
  print(city_pop_path)
  # collect raster data
  city_pop = raster(city_pop_path)
  city_pop[is.na(city_pop)] <- 0
  # mask raster based on administrative boundaries
  city_pop_mask = raster::mask(city_pop,city_boundary)
  
  
  
  return(city_pop_mask)
}


# city

selected_city = "CHL-Vitacura"

# filter boundary data
city_boundary = boundary %>% 
  filter(city_id == selected_city)

# get heat data for selected city
city_lst_mask = read.lst(city_boundary = city_boundary,
                         selected_city = "CHL-Vitacura",
                         data_source = "local")


# heat threshold layer -----
city_lst_mask_threshold = city_lst_mask

heat_threshold_value = 33

city_lst_mask_threshold = city_lst_mask

values(city_lst_mask_threshold)[values(city_lst_mask_threshold) <= heat_threshold_value] = 0
values(city_lst_mask_threshold)[values(city_lst_mask_threshold) > heat_threshold_value] = 1

# population layer ----

selected_population = "All" # All
city_pop_mask = read.pop.category(pop_category = selected_population,
                                  city_boundary = city_boundary,
                                  selected_city = selected_city,
                                  data_source = "local")

# population exposure class ----

# create exposed population raster
city_pop_heat_exposure = city_pop_mask * city_lst_mask_threshold




test = city_pop_mask * city_lst_mask
test = weighted.mean(city_lst_mask, values(city_pop_mask), na.rm=FALSE)
test = weighted.mean(values(city_pop_mask)[!is.na(values(city_pop_mask))],
                     values(city_lst_mask)[!is.na(values(city_lst_mask))])

# pop exposure classification
city_pop_heat_exposure_dist = values(city_pop_heat_exposure)[!is.na(values(city_pop_heat_exposure))]
city_pop_heat_exposure_dist = city_pop_heat_exposure_dist[city_pop_heat_exposure_dist>0]

city_pop_heat_exposure_median = median(city_pop_heat_exposure_dist)

# create classification matrix
m <- cbind(from = c(-Inf, 0, city_pop_heat_exposure_median),
           to = c(0, city_pop_heat_exposure_median, Inf),
           becomes = c(0, 1, 2))

city_pop_heat_exposure_class = reclassify(city_pop_heat_exposure, m)

city_pop_heat_exposure_class = raster::mask(city_pop_heat_exposure_class,city_boundary)

# create masked layer
city_lst_mask_threshold_na = city_lst_mask_threshold
city_lst_mask_threshold_na[city_lst_mask_threshold_na==0] <- NA

city_pop_mask_heat = mask(x = city_pop_mask,
                          mask = city_lst_mask_threshold_na)

city_pop_mask_heat = crop(city_pop_mask,
                          city_lst_mask_threshold)

city_pop_mask_heat <- crop(x = city_pop_mask_heat, y = extent(city_lst_mask_threshold))

# map prep ----

pal_heat <- colorNumeric("RdYlBu",
                         values(city_lst_mask),
                         na.color = "transparent",
                         reverse = TRUE)

pal_heat_class <- colorFactor(c("blue","red"),
                              levels = c(0,1),
                              na.color = "transparent")

pal_pop <- colorNumeric("RdYlBu",
                        values(city_pop_mask),
                        na.color = "transparent",
                        reverse = TRUE)

pal_pop_exposure <- colorNumeric("Reds",
                        values(city_pop_heat_exposure),
                        na.color = "transparent",
                        reverse = FALSE)

pal_Grid_pop_exposure_class <- colorFactor(c("green","yellow","red"),
                                           levels = c(0,1,2),
                                           na.color = "transparent")

pal_pop_exposure_masked <- colorNumeric("Reds",
                                 values(city_pop_mask_heat),
                                 na.color = "transparent",
                                 reverse = FALSE)

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
  # plot Heat distribution raster
  addRasterImage(city_lst_mask,
                 colors = pal_heat,
                 opacity = 0.7,
                 group = "Land Surface Temperature",
                 maxBytes = 8 * 1024 * 1024) %>%
  # Legend for Land surface temperature raster
  addLegend(pal = pal_heat,
            values = ~values(city_lst_mask),
            opacity = 0.9,
            title = "Land Surface Temperature",
            position = "bottomleft",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>% 
  # plot Heat distribution class raster
  addRasterImage(city_lst_mask_threshold,
                 colors = pal_heat_class,
                 opacity = 1,
                 group = "Heat exposure",
                 maxBytes = 8 * 1024 * 1024) %>%
  # Legend for Land surface temperature raster
  addLegend(colors = c("blue", "red"),
            labels = c("Not exposed", "Exposed"),
            opacity = 0.7,
            title = "Heat exposure",
            position = "topright") %>% 
  # plot Population distribution raster
  addRasterImage(city_pop_mask,
                 colors = pal_pop,
                 opacity = 0.9,
                 group = "Population count",
                 maxBytes = 8 * 1024 * 1024) %>%
  # Legend for population count
  addLegend(pal = pal_pop,
            values = ~values(city_pop_mask),
            opacity = 0.9,
            title = "Population count",
            position = "bottomright") %>%
  # plot Population distribution raster
  addRasterImage(city_pop_heat_exposure,
                 colors = pal_pop_exposure ,
                 opacity = 0.9,
                 group = "Population exposure count",
                 maxBytes = 8 * 1024 * 1024) %>%
  # Legend for population count
  addLegend(pal = pal_pop_exposure ,
            values = ~values(city_pop_heat_exposure),
            opacity = 0.9,
            title = "Population exposure count",
            position = "bottomright") %>%
  # # plot Population exposure class raster
  # addRasterImage(city_pop_heat_exposure_class,
  #                colors = pal_Grid_pop_exposure_class,
  #                opacity = 1,
  #                group = "Population exposure class",
  #                maxBytes = 8 * 1024 * 1024) %>%
  # # Legend for population exposure class
  # addLegend(colors = c("green", "yellow", "red"),
  #           labels = c("Low", "Moderate", "High"),
  #           # pal = pal_Grid_pop_exposure_class,
  #           # values = c(0,1,2),
  #           opacity = 0.7,
  #           title = "Population exposure class",
  #           position = "topleft") %>%   
  # plot Population distribution raster
  addRasterImage(city_pop_mask_heat,
                 colors = pal_pop_exposure_masked ,
                 opacity = 0.9,
                 group = "Population exposure mask",
                 maxBytes = 8 * 1024 * 1024) %>%
  # Legend for population mask
  addLegend(pal = pal_pop_exposure_masked ,
            values = ~values(city_pop_mask_heat),
            opacity = 0.9,
            title = "Population exposure mask",
            position = "bottomleft") %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "CartoDB"),
    overlayGroups = c("Land Surface Temperature",
                      "Heat exposure",
                      "Population count",
                      "Population exposure count",
                      "Population exposure class",
                      "Population exposure mask"),
    options = layersControlOptions(collapsed = FALSE)
  )

map

#############################
# polygonize

city_pop_heat_exposure_to_rp = aggregate(city_pop_heat_exposure, 5)

rp = rasterToPolygons(city_pop_heat_exposure_to_rp, fun=NULL, n=16, na.rm=TRUE, digits=12, dissolve=FALSE)

pal_grid <- colorNumeric(palette = "Reds",
                         domain = rp@data$layer,
                         na.color = "transparent",
                         reverse = FALSE)

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
  # plot boundary
  addPolygons(data = rp,
              group = "Grid",
              fillColor = ~pal_grid(rp@data$layer),
              color = "grey",
              weight = 1,
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.8,
              label = rp@data$layer,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
              ) %>% 
  # Legend for Land surface temperature raster
  addLegend(pal = pal_grid,
            values = rp@data$layer,
            opacity = 0.9,
            title = "grid",
            position = "bottomleft",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "CartoDB"),
    overlayGroups = c("Grid",
                      "Administrative boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )

map
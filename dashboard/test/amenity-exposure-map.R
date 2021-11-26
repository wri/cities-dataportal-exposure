
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
city_boundary_path = paste("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/PHL_makati.geojson")

# read the data
boundary <- st_read(city_boundary_path,
                    quiet = TRUE)

city_boundary = boundary


########### Load amenity data ----

# read amenity exposure Makati
amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/PHL-Makati/amenity_exposure_heat.csv",
                                encoding = "UTF-8")
# read amenity exposure Hobart
amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/AUS-Hobart/amenity_exposure_heat.csv",
                                encoding = "UTF-8")
# read amenity exposure Vitacura
amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/amenity_exposure_heat.csv",
                                encoding = "UTF-8")
# read amenity exposure for all cities
amenity_exposure_lst = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/amenity_exposure_lst.csv",
                                encoding = "UTF-8")

# remove outliers
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

# input variables ----

# define available cities
available_cities = unique(amenity_exposure_lst$city_name)
# define available sectors
available_amenity_sectors = unique(amenity_exposure_lst$gcom_sector_name)
# define available population categories
available_pop_categories = c("All", "Young (<20)", "Elderly (>60)", "Men", "Women")
available_pop_categories = available_amenity_sectors


# define slider threshold values
slider_min_heat = round(min(amenity_exposure_lst$exposure_lst_mean))
slider_max_heat = round(max(amenity_exposure_lst$exposure_lst_mean))
slider_value_heat = round(mean(amenity_exposure_lst$exposure_lst_mean))
slider_step = 1

# define city for prototype
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



# fitler data based on inputs ----

selected_city = "PHL-Makati" #"Makati" #input$City
selected_sector= available_amenity_sectors #input$Sector
heat_threshold_value = 38

# filter boundary data
city_boundary = boundary %>% 
  filter(city_id == selected_city_id)

# filter amenity

city_amenity = amenity_exposure_lst %>%
  filter(city_id == selected_city,
         gcom_sector_name %in% selected_sector) 

# filter and process based on heat threshold
# compute deviation from threshold
city_amenity$deviation_from_threshold = round((city_amenity$exposure_lst_mean/heat_threshold_value*100)-100,2) 
vec_deviation_from_threshold = city_amenity$deviation_from_threshold

city_amenity[city_amenity$deviation_from_threshold <= 0,"exposure_class" ] = "0-Low"
city_amenity[city_amenity$deviation_from_threshold > 0 & city_amenity$deviation_from_threshold <= 10,"exposure_class" ] = "1-Moderate"
city_amenity[city_amenity$deviation_from_threshold > 10,"exposure_class" ] = "2-High"
vec_exposure_class = city_amenity$exposure_class



# plot amenity exposure map ----

# filter data
amenity_exposure_lst = filter.amenity.city.boundary(amenity_exposure_lst = amenity_exposure_lst_Makati,
                                                    city_boundary = boundary_Makati)

filtereData = amenity_exposure_lst[amenity_exposure_lst$city_id == selected_city & amenity_exposure_lst$gcom_sector_name %in% selected_sector, ]

# prepare map plot ----

# define class color palette
pal_amenity_class <- colorFactor(c("green","yellow","red"),
                                 levels =c("0-Low","1-Moderate","2-High"))

# define numerical color palette
pal_amenity <- colorNumeric("RdYlBu", 
                            domain = city_amenity$exposure_lst_mean,
                            reverse = TRUE)

# define deviation color palette
pal_amenity_heat_deviation <- colorNumeric("RdYlBu", 
                                           domain = city_amenity$heat_dev_from_amenities,
                                           reverse = TRUE)
# define deviation color palette
pal_amenity_heat_deviation_threshold <- colorNumeric("RdYlBu", 
                                                     domain = vec_deviation_from_threshold,
                                                     reverse = TRUE)
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

# plot map
leaflet(filtereData)  %>%
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
  # Legend for amenitu heat values
  addLegend(pal = pal_amenity, 
            values = ~exposure_lst_mean, 
            opacity = 0.9,
            title = "Amenity heat value",
            position = "bottomright",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>%
  # Plot amenities with heat value
  # addCircleMarkers(~longitude, ~latitude,
  #                  radius = 4,
  #                  color = "black",
  #                  fillColor = ~pal_amenity_heat_deviation(heat_dev_from_amenities),
  #                  stroke = TRUE,
  #                  fillOpacity = 0.7,
  #                  weight = 1,
  #                  popup = ~as.character(city_name),
  #                  label = labels_amenity,
  #                  group = "Amenity heat deviation") %>%
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
  addLegend(pal = pal_amenity, 
            values = ~exposure_lst_mean, 
            opacity = 0.9,
            title = "Land Surface Temperature",
            position = "bottomleft",
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
                      "Amenity exposure class"),
    options = layersControlOptions(collapsed = FALSE)
  )
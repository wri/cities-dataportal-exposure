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
city_boundary_path = paste("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/boundary_CHL_vitacura_mapped_geom.geojson")
# city_boundary_path = paste("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/PHL_makati.geojson")
# city_boundary_path = paste("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/boundary_AUS_hobart_mapped.geojson")

# read the data
boundary <- st_read(city_boundary_path,
                    quiet = TRUE)

city_boundary = boundary

### read flooding coastal ----


flooding_path = "https://storage.googleapis.com/data_portal_exposure/data/flooding/inuncoast_historical_nosub_hist_rp0002_0_PHL-Makati.tif"
flooding_path = "https://storage.googleapis.com/data_portal_exposure/data/flooding/inuncoast_historical_nosub_hist_rp0005_0_PHL-Makati.tif"
flooding_path = "https://storage.googleapis.com/data_portal_exposure/data/flooding/inuncoast_historical_nosub_hist_rp0010_0_PHL-Makati.tif"

# collect raster data
flooding = raster(flooding_path)
# mask raster based on administrative boundaries
city_flooding = raster::mask(flooding,
                             city_boundary)
plot(city_flooding)
summary(values(city_flooding))

### read flooding riverine ----

flooding_path = "https://storage.googleapis.com/data_portal_exposure/data/flooding/riverine_rp00100_1980_CHL-Vitacura.tif"
flooding_path = "https://storage.googleapis.com/data_portal_exposure/data/flooding/riverine_rp00100_2030_CHL-Vitacura.tif"
flooding_path = "https://storage.googleapis.com/data_portal_exposure/data/flooding/riverine_rp00100_2050_CHL-Vitacura.tif"
flooding_path = "https://storage.googleapis.com/data_portal_exposure/data/flooding/riverine_rp00100_2080_CHL-Vitacura.tif"


flooding_path = "https://storage.googleapis.com/data_portal_exposure/data/flooding/riverine_rp00100_1980_CHL-Vitacura.tif"
# collect raster data
flooding = raster(flooding_path)
city_flooding = flooding
city_flooding[is.na(city_flooding)] = 0
# mask raster based on administrative boundaries
city_flooding = raster::mask(flooding,
                             city_boundary)

city_flooding[is.na(city_flooding)] = 0
plot(city_flooding)
summary(values(city_flooding))


# leaflet plot ---------------

# define ratser color palette for LST data
pal_flooding <- colorNumeric("Blues", 
                             values(city_flooding),
                             na.color = "transparent",
                             reverse = FALSE)

leaflet(city_boundary) %>%
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
  addRasterImage(city_flooding, 
                 colors = "Blues",
                 opacity = 0.9,
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
    overlayGroups = c("Amenity exposure value", 
                      # "Land Surface Temperature",
                      "Riverine flooding"),
    options = layersControlOptions(collapsed = FALSE))
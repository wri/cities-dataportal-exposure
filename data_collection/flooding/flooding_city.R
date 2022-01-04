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

### read land surface temperature ----

# define path
city_lst_path = "/vsicurl/https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/data/hazards/heat/land_surface_temperature/lst_mean_lst_PHL-Makati.tif"
flooding_path = "./data/flooding/inuncoast_historical_nosub_hist_rp0001_5.tif"

# collect raster data
flooding = raster(flooding_path)
# mask raster based on administrative boundaries
city_flooding = raster::mask(flooding,
                             city_boundary)




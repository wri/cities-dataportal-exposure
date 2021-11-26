# load input data ----

# load boundaries

boundary <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/cities_boundaries.geojson",
                    quiet = TRUE)

# define filters ----

selected_city = "AUS-Hobart" # "AUS-Hobart" # "PHL-Makati" #"CHL-Vitacura" #input$City
selected_sector= available_amenity_sectors #input$Sector
heat_threshold_value = 38

# filter boundary data
city_boundary = boundary %>% 
  filter(city_id == selected_city)



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
  
  
  if(data_source == "d3"){
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

test = read.pop.category(pop_category = "All",
                         city_boundary = city_boundary,
                         selected_city = selected_city,
                         data_source = "local")

# read lst

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

test = read.lst(city_boundary = city_boundary,
                selected_city = selected_city,
                data_source = "local")

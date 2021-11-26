# merge amenity exposure files for all cities

# read boundaries data ----


# Makati
boundary_Makati <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/PHL_makati.geojson",
                           quiet = TRUE)
# Vitacura
boundary_Vitacura <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/boundary_CHL_vitacura_mapped_geom.geojson",
                           quiet = TRUE)

# Hobart
boundary_Hobart <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/boundary_AUS_hobart_mapped.geojson",
                             quiet = TRUE)

# read amenities data ----
# read amenity exposure Makati
amenity_exposure_lst_Makati = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/PHL-Makati/amenity_exposure_heat.csv",
                                encoding = "UTF-8")
# process data
amenity_exposure_lst_Makati = amenity_exposure_lst_Makati %>% 
  drop_na(exposure_lst_mean)


# read amenity exposure Hobart
amenity_exposure_lst_Hobart = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/AUS-Hobart/amenity_exposure_heat.csv",
                                encoding = "UTF-8")
# process data
amenity_exposure_lst_Hobart = amenity_exposure_lst_Hobart %>% 
  drop_na(exposure_lst_mean)


# read amenity exposure Vitacura
amenity_exposure_lst_Vitacura = read.csv("https://storage.googleapis.com/data_portal_exposure/data/indicators/CHL-Vitacura/amenity_exposure_heat.csv",
                                encoding = "UTF-8")
# process data
amenity_exposure_lst_Vitacura = amenity_exposure_lst_Vitacura %>% 
  drop_na(exposure_lst_mean)


# filter amenity location based on boundaries ----

filter.amenity.city.boundary = function(amenity_exposure_lst, city_boundary){
  amenity_points <- st_as_sf(x = amenity_exposure_lst, 
                             coords = c("longitude", "latitude"),
                             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  amenity_within_boundary <- st_intersection(city_boundary, amenity_points)
  
  amenity_exposure_lst = amenity_exposure_lst %>% 
    filter(id %in% amenity_within_boundary$id)
  
  # compute average heat of all amenities
  city_amenity_avg_heat = mean(amenity_exposure_lst$exposure_lst_mean)
  amenity_exposure_lst$heat_dev_from_amenities = amenity_exposure_lst$exposure_lst_mean - city_amenity_avg_heat
  
  return(amenity_exposure_lst)
}

amenity_exposure_lst_Makati = filter.amenity.city.boundary(amenity_exposure_lst = amenity_exposure_lst_Makati,
                                                           city_boundary = boundary_Makati)

amenity_exposure_lst_Vitacura = filter.amenity.city.boundary(amenity_exposure_lst = amenity_exposure_lst_Vitacura,
                                                           city_boundary = boundary_Vitacura)

amenity_exposure_lst_Hobart = filter.amenity.city.boundary(amenity_exposure_lst = amenity_exposure_lst_Hobart,
                                                           city_boundary = boundary_Hobart)

# merge all ----

amenity_exposure_lst = amenity_exposure_lst_Makati %>% 
  bind_rows(amenity_exposure_lst_Hobart,amenity_exposure_lst_Vitacura)

# recode gcom sectors
amenity_exposure_lst =amenity_exposure_lst  %>% 
  mutate(gcom_sector_name=recode(gcom_sector_name, 
                                 ` Law & Order`="Law & Order",
                                 `Law & Order`="Law & Order")) 

amenity_exposure_lst =amenity_exposure_lst  %>% 
  mutate(gcom_sector_name=recode(gcom_sector_name, 
                                 ` Society/community & culture`="Society/community & culture",
                                 `Society/community & culture`="Society/community & culture"))

# Store output ----

write.csv(amenity_exposure_lst,
          "./github/cities-dataportal-exposure/exposure/outputs/amenity_exposure_lst.csv")



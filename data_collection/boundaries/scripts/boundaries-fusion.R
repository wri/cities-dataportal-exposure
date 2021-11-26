# merge boundaries of all cities

# read boundaries ----

# Makati
boundary_Makati <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/PHL_makati.geojson",
                           quiet = TRUE)
# Vitacura
boundary_Vitacura <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/boundary_CHL_vitacura_mapped_geom.geojson",
                             quiet = TRUE)

# Hobart
boundary_Hobart <- st_read("https://storage.googleapis.com/data_portal_exposure/data/administrative_boundaries/mapped/boundary_AUS_hobart_mapped.geojson",
                           quiet = TRUE)


# merge cities ----

boundary = boundary_Makati %>% 
  bind_rows(boundary_Vitacura, boundary_Hobart)


# store results ----

st_write(boundary,
         "./github/cities-dataportal-exposure/data_collection/boundaries/outputs/cities_boundaries.geojson",
         delete_dsn=TRUE)





library(wopr)


# Retrieve the WOPR data catalogue
catalogue <- getCatalogue()

# Select files from the catalogue by subsetting the data frame
selection <- subset(catalogue,
                    country == 'NGA' &
                      category == 'Population' & 
                      version == 'v1.2')

# Download selected files
downloadData(selection)

# Spatial Query
data(wopr_points)
plot(wopr_points, pch=16)

data(wopr_polys)
plot(wopr_polys)

getCatalogue(spatial_query=T)

N <- getPop(feature=wopr_polys[1,], 
            country='NGA', 
            version='v1.2')

summaryPop(N, confidence=0.95, tails=2, abovethresh=1e2, belowthresh=50)

N <- getPop(feature=wopr_polys[1,], 
            country='PHL', 
            version='v1.2')



# boundary

selected_city = "PHL-Makati"

# filter boundary data
city_boundary = boundary %>% 
  filter(city_id == selected_city)

# read PHL rasters

raster_path = paste(getwd(),"/data/population/PHL/phl_f_0_2020_constrained.tif",
                    sep = "")
phl_f_0_2020_constrained = raster(raster_path)


city_pop_mask = raster::mask(phl_f_0_2020_constrained,city_boundary)

city_pop_mask[is.na(city_pop_mask)] <- 0

plot(city_pop_mask)


# all pop

raster_path = "https://storage.googleapis.com/data_portal_exposure/data/population/worldpop_PHL-Makati_population.tif"
Makati_M_0 = raster(raster_path)
city_pop_mask_all = raster::mask(Makati_M_0,city_boundary)
city_pop_mask_all[is.na(city_pop_mask_all)] <- 0

# threshold
heat_threshold_value = 37

city_lst_mask_threshold = city_lst_mask

values(city_lst_mask_threshold)[values(city_lst_mask_threshold) <= heat_threshold_value] = 0
values(city_lst_mask_threshold)[values(city_lst_mask_threshold) > heat_threshold_value] = 1

# by category
raster_path = "https://storage.googleapis.com/data_portal_exposure/data/population/worldpop_PHL-Makati_M_5.tif"
Makati_M_0 = raster(raster_path)
city_pop_mask = raster::mask(Makati_M_0,city_boundary)
city_pop_ratio = city_pop_mask/city_pop_mask_all
summary(values(city_pop_ratio))

# women

selected_population = "Women"
city_pop_Women = read.pop.category(pop_category = selected_population,
                                  city_boundary = city_boundary,
                                  selected_city = selected_city,
                                  data_source = "local")
city_pop_Women[is.na(city_pop_Women)] <- 0
 
pop_sum = sum(values(city_pop_mask_all))
pop_sum

pop_sum_women = sum(values(city_pop_Women))
pop_sum_women

percent_category = pop_sum_women/pop_sum * 100
percent_category


# create exposed population raster
city_pop_Women_exposure = city_pop_Women * city_lst_mask_threshold
city_pop_Women_exposure[is.na(city_pop_Women_exposure)] <- 0

pop_sum_women_exposed = sum(values(city_pop_Women_exposure))
pop_sum_women_exposed

pop_sum_women_exposed_percent = pop_sum_women_exposed/pop_sum_women
pop_sum_women_exposed_percent

# elderly

selected_population = "Elderly"
city_pop_Elderly = read.pop.category(pop_category = selected_population,
                                   city_boundary = city_boundary,
                                   selected_city = selected_city,
                                   data_source = "local")
city_pop_Elderly[is.na(city_pop_Elderly)] <- 0

pop_sum_Elderly = sum(values(city_pop_Elderly))
pop_sum_Elderly

percent_category = pop_sum_Elderly/pop_sum * 100
percent_category


# create exposed population raster
city_pop_Elderly_exposure = city_pop_Elderly * city_lst_mask_threshold
city_pop_Elderly_exposure[is.na(city_pop_Elderly_exposure)] <- 0

pop_sum_Elderly_exposed = sum(values(city_pop_Elderly_exposure))
pop_sum_Elderly_exposed

pop_sum_Elderly_exposed_percent = pop_sum_Elderly_exposed/pop_sum_Elderly
pop_sum_Elderly_exposed_percent

# Children

selected_population = "Children"
city_pop_Children = read.pop.category(pop_category = selected_population,
                                     city_boundary = city_boundary,
                                     selected_city = selected_city,
                                     data_source = "local")
city_pop_Children[is.na(city_pop_Children)] <- 0

pop_sum_Children = sum(values(city_pop_Children))
pop_sum_Children

percent_category = pop_sum_Children/pop_sum * 100
percent_category


# create exposed population raster
city_pop_Children_exposure = city_pop_Children * city_lst_mask_threshold
city_pop_Children_exposure[is.na(city_pop_Children_exposure)] <- 0

pop_sum_Children_exposed = sum(values(city_pop_Children_exposure))
pop_sum_Children_exposed

pop_sum_Children_exposed_percent = pop_sum_Children_exposed/pop_sum_Children
pop_sum_Children_exposed_percent

################################################ Add to app

# get vulnerable population data

city_pop_all = read.pop.category(pop_category = "All",
                                   city_boundary = city_boundary,
                                   selected_city = selected_city,
                                   data_source = "local")
city_pop_all[is.na(city_pop_all)] <- 0

city_pop_women = read.pop.category(pop_category = "Women",
                                  city_boundary = city_boundary,
                                  selected_city = selected_city,
                                  data_source = "local")
city_pop_women[is.na(city_pop_women)] <- 0

city_pop_children = read.pop.category(pop_category = "Children",
                                   city_boundary = city_boundary,
                                   selected_city = selected_city,
                                   data_source = "local")
city_pop_children[is.na(city_pop_children)] <- 0

city_pop_elderly= read.pop.category(pop_category = "Elderly",
                                      city_boundary = city_boundary,
                                      selected_city = selected_city,
                                      data_source = "local")
city_pop_elderly[is.na(city_pop_elderly)] <- 0

# 




data_portal_cities = c("PHL-Makati")
pop_categories = c("All","Children","Elderly","Women")

i = 1
pop_category = pop_categories[i]

selected_city = "PHL-Makati"

pop_exposure_stat = data.frame(
  city_id = rep(selected_city,4),
  pop_category = c("All","Children","Elderly","Women"))

city_population_sum = sum(values(city_pop_all))
pop_exposure_stat[pop_exposure_stat$pop_category == "All", "Population count"] = sum(values(city_pop_all))
pop_exposure_stat[pop_exposure_stat$pop_category == "All", "Population percent"] = round(sum(values(city_pop_all))/city_population_sum * 100,2)

# create exposed population raster
city_pop_category_exposure = city_pop_all * city_lst_mask_threshold
city_pop_category_exposure[is.na(city_pop_category_exposure)] <- 0
ciy_pop_exposed_count = sum(values(city_pop_category_exposure))
pop_exposure_stat[pop_exposure_stat$pop_category == "All", "Number of exposed population"] = round(ciy_pop_exposed_count,0)
pop_exposure_stat[pop_exposure_stat$pop_category == "All", "Percent of exposed population"] = round(ciy_pop_exposed_count/city_population_sum * 100,2)
pop_exposure_stat


pop_exposure_stat


pop_categories = c("Children","Elderly","Women")

pop_exposure_stat = data.frame(
  Selected.city = rep(selected_city,3),
  Population.category = pop_categories)

# All population exposure stat

city_pop_all = read.pop.category(pop_category = "All",
                                 city_boundary = city_boundary,
                                 selected_city = selected_city,
                                 data_source = "local")
city_pop_all[is.na(city_pop_all)] <- 0

city_population_sum = sum(values(city_pop_all))

# population exposure stat by populualtion category

for(i in 1:length(pop_categories)){
  pop_category = pop_categories[i]
  
  city_pop_category = read.pop.category(pop_category = pop_category,
                               city_boundary = city_boundary,
                               selected_city = selected_city,
                               data_source = "local")
  city_pop_category[is.na(city_pop_category)] <- 0
  
  pop_category_count = sum(values(city_pop_category))
  pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Population count"] = round(pop_category_count,0)
  pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Population percent"] = round(sum(values(city_pop_category))/city_population_sum * 100,2)
  
    
  # create exposed population raster
  city_pop_category_exposure = city_pop_category * city_lst_mask_threshold
  city_pop_category_exposure[is.na(city_pop_category_exposure)] <- 0
  city_pop_category_exposed_count = sum(values(city_pop_category_exposure))
  pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Number of exposed population"] = round(city_pop_category_exposed_count,0)
  pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Percent of exposed population from population category"] = round(city_pop_category_exposed_count/pop_category_count * 100,2)
  pop_exposure_stat[pop_exposure_stat$Population.category == pop_category, "Percent of exposed population from all population"] = round(city_pop_category_exposed_count/city_population_sum * 100,2)
}


# plot table
DT::datatable(pop_exposure_stat)

# plot barchart

fig = pop_exposure_stat %>% 
  arrange(desc(`Percent of exposed population from all population`)) %>% 
  plot_ly() %>% 
  add_trace(x = ~Population.category,
            y = ~`Percent of exposed population from all population`, 
            type = "bar",
            orientation = "v",
            # marker = list(color = 'blue'),
            text = ~paste("Population percent: ",`Population percent`, 
                          '<br>Percent of exposed population from all population:', `Percent of exposed population from all population`)) %>% 
  layout(yaxis = list(title = 'Percent of exposed persons by category (%)'),
         xaxis = list(title = 'Population categories'))

fig
            



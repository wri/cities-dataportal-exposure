# Cities Exposure Data Portal pilot

Welcome to the City Data Portal project repository dedicated to the assement of cities' exposure to natural hazards.

**This is a work in progress repository! It is not yet an official WRI repository. 
For more information about the project status, please contact Eric.Mackres@wri.org.**

This repository provides:

- **Data Exploration**: Scripts for exploring cities' exposure indicators to naturla hazards: extreme heat events and flooding
- **Data Pipeline**: Scripts for collecting, processing and storing data needed for exposure assessment: administrative boundaries, amenity locations, population distribution, land surface temperature...
- **Dashboard**: Scripts for building an interactive dashboard to visualize the computed exposure indicators

# Data sources

| Dataset | Source |  Description |
| ------ |------ |------ |
| Administrative boundaries | GeoBoundaris; OpenStreetMap |   |
| Amenities | OpenStreetMap |   |
| Amenities' sectors | lobal covenant of mayor [GCRF](https://www.globalcovenantofmayors.org/our-initiatives/data4cities/common-global-reporting-framework/) |   |
| Land Surface Temperature | Landsat | |  [integrate_lst_data.js](https://code.earthengine.google.com/5b00ea20c399c16c41c7c2ea98a08586?accept_repo=users%2Femackres%2FSCIP) |
| Population | WorldPop |   |

# Dashboard

You can explore the city exposure dashboard here: https://wri-cities.shinyapps.io/cities-exposure-dashboard/.

# Technical documentation

A complete technical documentation of the exposure assessment framework is provided here: https://cities-data-portal-adaptation.s3.eu-west-3.amazonaws.com/docs/climate-adaptattion-dashboard.html




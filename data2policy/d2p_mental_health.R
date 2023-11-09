library(spdep)
library(sf)
library(smerc)
library(RColorBrewer)


bldgs <- read.csv("data/All_Active_Air_Pollution_Emitting_Facilities.csv")
coords = bldgs[,c("X", "Y")]
CO <- st_read("data/Colorado_State_Boundary/Colorado_State_Boundary.shp")
# plot(CO$geometry)
# points(coords)

health_data <- read.csv("data/Mental_Health_CDPHE_Census_Tracts.csv")
CO_tracts <- st_read("data/Colorado_Census_Tract_Boundaries/Colorado_Census_Tract_Boundaries.shp")
CO_tracts$FIPS = as.numeric(CO_tracts$FIPS)
Z <- merge(health_data, CO_tracts,by.x='Census_Tract_FIPS',by.y='FIPS')
st_z <- st_as_sf(Z)

plot(st_z["MentalHlth_Census_Tract_Estimate"])
points(coords)

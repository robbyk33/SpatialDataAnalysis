# project scratch work

library(tmap)
library(leaflet)
library(RColorBrewer)

CO <- st_read("data/Colorado_State_Boundary/Colorado_State_Boundary.shp")
counties <- st_read("projectdata/CO_counties/Colorado_County_Boundaries.shp")


tm_shape(health_data) +
  tm_fill(
    "Per_Adults",
    # style ='quantile',
    # n=4,
    palette='Blues')


# read in CO census tracts
# CO_tracts <- st_read("data/Colorado_Census_Tract_Boundaries/Colorado_Census_Tract_Boundaries.shp")
# 
# CO_tracts$FIPS = as.numeric(CO_tracts$FIPS)
# # CO_tracts
# Z <- merge(health_data, CO_tracts,by.x='Census_Tract_FIPS',by.y='FIPS')
# st_z <- st_as_sf(Z)


# read in air polluting building data
bldgs <- read.csv("data/All_Active_Air_Pollution_Emitting_Facilities.csv")

bldg_sf <- st_as_sf(bldgs, coords=c("Longitude","Latitude"), crs=4269)


# geom_sf(data=bldg_sf,color=alpha('blue',.15))+


# set up color map
mycol = brewer.pal(5, "Accent")

# create vector of colors to show results
# default is white (no clustering)
bncol = rep("white", nrow(counties2))


# the most likely cluster locations are lightgreen for cstar = 1000
bncol[bn500$clusters[[1]]$locids] = mycol[1]

# the most likely cluster locations are ______ for cstar = 2000
bncol[bn2000$clusters[[1]]$locids] = mycol[2]

# # the most likely cluster locations are magenta for cstar = 3000
# bncol[bn3000$clusters[[1]]$locids] = mycol[3]

# the most likely cluster locations are lightpurple for cstar = 5000
bncol[bn5k$clusters[[1]]$locids] = mycol[3]

# the most likely cluster locations are _________ for cstar = 10000
bncol[bn10k$clusters[[1]]$locids] = mycol[4]

# the most likely cluster locations are _________ for cstar = 10000
bncol[bn20k$clusters[[1]]$locids] = mycol[5]

# plot most likely clusters
plot(counties2$geometry, border="grey60", axes = FALSE, col = bncol)
legend("topright",
       legend = c("Cluster k = 500", "Cluster k = 2000",
                  "Cluster k = 5000","Cluster k = 10000",
                  "Cluster k = 20000"),
       lwd = 10, col = mycol)

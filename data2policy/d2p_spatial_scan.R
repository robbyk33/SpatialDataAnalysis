################################################################
# Spatial-Scan Method
library(ggplot2)
library(smerc)
library(RColorBrewer)
library(sf)

counties2 <- readRDS("projectdata/county.rds")
ap_bldgs <- readRDS("projectdata/ap_bldgs.rds")


centrs = cbind(counties$CENT_LONG,counties$CENT_LAT)

# observed cases, rounded down
cases = counties2$cases

# population (same as nydf$population)
population = counties2$Total.Population

# expected number of cases
e = sum(cases)/sum(population) * population

# apply circular scan method
scan = scan.test(coords = centrs,
                 cases = cases,
                 pop = population,
                 ex = e,
                 nsim = 999,
                 alpha  = 0.1)

# results from the test are available in
summary(scan)
# cluster information
clusters(scan)

# need to color 8 clusters
mycol = grDevices::hcl.colors(8)
# color.clusters(scan, col = mycol) colors the 8 clusters using the desired clusters
plot(counties2$geometry, border="grey60", axes=FALSE,
     col = color.clusters(scan, col = mycol))
title(
  main =
    "Spatial Scan Method\nMost Likely Cluster of Adult Mental Distress Cases")





#############################################
# Spatial scan for air pollution emitting bldgs

b_cases = counties2$bldg_count
b_permits = counties2$Permits



# expected number of cases
e = sum(b_cases)/sum(b_permits) * b_permits

# apply circular scan method
scan2 = scan.test(coords = centrs,
                 cases = b_cases,
                 pop = b_permits,
                 ex = e,
                 nsim = 999,
                 alpha  = 0.1)

# results from the test are available in
summary(scan2)
# cluster information
clusters(scan2)

# need to color 7 clusters
mycol = grDevices::hcl.colors(7)
# color.clusters(scan, col = mycol) colors the 7 clusters using the desired clusters
plot(counties2$geometry, border="grey60", axes=FALSE,
     col = color.clusters(scan2, col = mycol))
title(
  main = 
    "Spatial Scan Method\nMost Likely Cluster of Pollution Emitting Buildings")
plot(ap_bldgs,add=TRUE,col=alpha("red",0.15))

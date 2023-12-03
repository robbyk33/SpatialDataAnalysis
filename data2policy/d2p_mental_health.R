library(spdep)
library(ggplot2)
library(smerc)
library(RColorBrewer)
library(sf)



# read in Colorado state boundary
CO <- st_read("data/Colorado_State_Boundary/Colorado_State_Boundary.shp")
counties <- st_read("projectdata/CO_counties/Colorado_County_Boundaries.shp")

# health_data <- read.csv("data/Mental_Health_CDPHE_Census_Tracts.csv")
health_data <- st_read("projectdata/CO_adultmentalhealth_county/Mental_Health_in_Adults_-_Colorado_BRFSS_2014-2017_(County).shp")

# read in air polluting building data
ap_bldgs <- st_read("projectdata/air_polluting_bldgs/All_Active_Air_Pollution_Emitting_Facilities.shp")
ap_adj <- st_transform(ap_bldgs,crs=st_crs(counties))

# read in county population data
c_pops <- read.csv("projectdata/population-county.csv")

# add population column to counties df
counties <- merge(counties, c_pops, by.x='FULL',by.y='COUNTY')

# plot population visual
ggplot()+
  geom_sf(data=counties,aes(fill=Total.Population))+
  scale_fill_viridis_c(option = "E")+
  labs(title = "Population by County")

# plot mental health data
ggplot() +
  # geom_sf(data = counties,color='black')+
  geom_sf(data=health_data,aes(fill=Per_Adults))+
  scale_fill_viridis_c(option = "D")+
  labs(title="% Adults Experiencing Frequent Mental Distress in CO by County")

# overlay air pollution emitting bldgs
ggplot() +
  geom_sf(data=health_data,aes(fill=Per_Adults))+
  scale_fill_viridis_c(option = "D")+
  geom_sf(data=ap_bldgs,aes(color='red'))+
  scale_colour_manual(name = 'Polluting Bldgs',
                      values =c(red=alpha('red',.10)), labels = c(''))+
  labs(title="% Adults Experiencing Frequent Mental Distress in CO by County")


# get bldg counts in each county

# generate matrix returning True/False if point geometry intersects the polygon
count_mat <- st_intersects(x=ap_adj,y=counties,sparse = FALSE)

# sums total number of TRUE values in each column
counts <- apply(count_mat, FUN=sum,MARGIN = 2)

# add bldg counts to the counties dataframe
counties$bldg_count = counts

# get dataframe of county label and % adult cases
temp_h <- st_drop_geometry(health_data[3:4])

# add mental health % to county dataframe for simple analysis
counties2 <- merge(counties,temp_h,x.by="LABEL",y.by="LABEL")


counties2$cases <- as.integer(
  (counties2$Per_Adults/100)*(counties2$Total.Population))


# import historic count of bldg permits for all counties since 1985
bldg_permits <- read.csv("projectdata/county_bldg_permits.csv")

df_permits <- data.frame(
  County=bldg_permits$COUNTY,
  Permits=bldg_permits$Census.Building.Permits)


counties2 <- merge(counties2,df_permits,by.x="FULL",by.y="County")

str(counties2)

#store cleaned data
saveRDS(counties2,file = "projectdata/county.rds")

saveRDS(ap_adj,file="projectdata/ap_bldgs.rds")


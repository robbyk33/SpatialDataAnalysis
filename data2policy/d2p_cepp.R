################################################################
# CEPP Method
library(ggplot2)
library(smerc)
library(RColorBrewer)
library(sf)

counties2 <- readRDS("projectdata/county.rds")
ap_bldgs <- readRDS("projectdata/ap_bldgs.rds")


centrs = cbind(counties$CENT_LONG,counties$CENT_LAT)
quantile(counties2$Total.Population)

cepp10k = cepp.test(coords = centrs,
                     cases = counties2$cases,
                     pop = counties2$Total.Population,
                     nstar = 10000,
                     alpha = 0.10)


cepp25k = cepp.test(coords = centrs,
                     cases = counties2$cases,
                     pop = counties2$Total.Population,
                     nstar = 25000,
                     alpha = 0.10)

cepp50k = cepp.test(coords = centrs,
                    cases = counties2$cases,
                    pop = counties2$Total.Population,
                    nstar = 50000,
                    alpha = 0.10)

cepp100k = cepp.test(coords = centrs,
                    cases = counties2$cases,
                    pop = counties2$Total.Population,
                    nstar = 100000,
                    alpha = 0.10)

#################################################################
# plotting

library(ggpubr)

# gen plots for each c* value analyzed
g1 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=color.clusters(cepp10k)))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "n* = 10000")+
  theme_minimal() 

g2 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=color.clusters(cepp25k)))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "n* = 25000")+
  theme_minimal() 

g3 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=color.clusters(cepp50k)))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "n* = 50000")+
  theme_minimal() 

g4 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=color.clusters(cepp100k)))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "n* = 100000")+
  theme_minimal() 

bn_plots <- ggarrange(g1,g2,g3,g4, ncol=2,nrow=2)
annotate_figure(bn_plots,
                top = text_grob("Turnbull's CEPP\nPotential Clusters of Adult Mental Distress Cases",
                                color = "black",
                                face = "bold",
                                size = 14))

################################################################
# CEPP Method - Pollution emitting Buildings
library(ggplot2)
library(smerc)
library(RColorBrewer)
library(sf)

counties2 <- readRDS("projectdata/county.rds")
ap_bldgs <- readRDS("projectdata/ap_bldgs.rds")


centrs = cbind(counties$CENT_LONG,counties$CENT_LAT)
quantile(counties2$Permits)


cepp500 = cepp.test(coords = centrs,
                   cases = counties2$bldg_count,
                   pop = counties2$Permits,
                   nstar = 500,
                   alpha = 0.10)

cepp1500 = cepp.test(coords = centrs,
                    cases = counties2$bldg_count,
                    pop = counties2$Permits,
                    nstar = 1500,
                    alpha = 0.10)

cepp10k = cepp.test(coords = centrs,
                    cases = counties2$bldg_count,
                    pop = counties2$Permits,
                    nstar = 10000,
                    alpha = 0.10)

cepp50k = cepp.test(coords = centrs,
                    cases = counties2$bldg_count,
                    pop = counties2$Permits,
                    nstar = 50000,
                    alpha = 0.10)

#################################################################
# plotting

library(ggpubr)

# gen plots for each c* value analyzed
g1 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=color.clusters(cepp500)))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "n* = 500")+
  theme_minimal() 

g2 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=color.clusters(cepp1500)))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "n* = 1500")+
  theme_minimal() 

g3 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=color.clusters(cepp10k)))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "n* = 10000")+
  theme_minimal() 

g4 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=color.clusters(cepp50k)))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "n* = 50000")+
  theme_minimal() 

bn_plots <- ggarrange(g1,g2,g3,g4, ncol=2,nrow=2)
annotate_figure(bn_plots,
                top = text_grob("Turnbull's CEPP\nPotential Clusters of Pollution Emitting Buildings",
                                color = "black",
                                face = "bold",
                                size = 14))

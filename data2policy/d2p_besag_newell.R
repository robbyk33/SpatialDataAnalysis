################################################################
# Besag-Newell Analysis
library(ggplot2)
library(smerc)
library(RColorBrewer)
library(sf)

counties2 <- readRDS("projectdata/county.rds")
ap_bldgs <- readRDS("projectdata/ap_bldgs.rds")

centrs = cbind(counties$CENT_LONG,counties$CENT_LAT)

hist(counties2$cases)


##
bn500 = bn.test(coords = centrs,
                cases = counties2$cases,
                pop = counties2$Total.Population,
                cstar = 500,
                alpha = 0.1)
clusters(bn500)

##
bn1000 =  bn.test(coords = centrs,
                  cases = counties2$cases,
                  pop = counties2$Total.Population,
                  cstar = 1000,
                  alpha = 0.1)
clusters(bn1000)

##
# bn2000 =  bn.test(coords = centrs,
#                   cases = counties2$cases,
#                   pop = counties2$Total.Population,
#                   cstar = 2000,
#                   alpha = 0.1)
# clusters(bn2000)

##
bn3600 = bn.test(coords = centrs,
                 cases = counties2$cases,
                 pop = counties2$Total.Population,
                 cstar = 3600,
                 alpha = 0.1)
clusters(bn3600)

# ##
# bn5k = bn.test(coords = centrs,
#                 cases = counties2$cases,
#                 pop = counties2$Total.Population,
#                 cstar = 8000,
#                 alpha = 0.1)
# clusters(bn5k)

##
bn25k =  bn.test(coords = centrs,
                  cases = counties2$cases,
                  pop = counties2$Total.Population,
                  cstar = 25000,
                  alpha = 0.1)
clusters(bn25k)

# ##
# bn20k =  bn.test(coords = centrs,
#                  cases = counties2$cases,
#                  pop = counties2$Total.Population,
#                  cstar = 25000,
#                  alpha = 0.1)
# clusters(bn20k)



#############################################################
# plotting

# gen color mappings
bncol500 = color.clusters(bn500)
bncol1000 = color.clusters(bn1000)
bncol3600 = color.clusters(bn3600)
bncol25k = color.clusters(bn25k)

library(ggpubr)

# gen plots for each c* value analyzed
g1 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol500))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 500")+
  theme_minimal() 

g2 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol1000))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 1000")+
  theme_minimal() 

g3 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol3600))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 3600")+
  theme_minimal() 

g4 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol25k))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 25000")+
  theme_minimal() 

bn_plots <- ggarrange(g1,g2,g3,g4, ncol=2,nrow=2)
annotate_figure(bn_plots,
                top = text_grob("Besag-Newell Method\nMost Likely Clusters of Adult Mental Distress Cases",
                                color = "black",
                                face = "bold",
                                size = 14))


# look more closely at most likely cluster information
bn200$clusters[[1]][c("cases", "population", "pvalue")]
bn500$clusters[[1]][c("cases", "population", "pvalue")]
bn1000$clusters[[1]][c("cases", "population", "pvalue")]
bn3000$clusters[[1]][c("cases", "population", "pvalue")]
bn5000$clusters[[1]][c("cases", "population", "pvalue")]
bn10k$clusters[[1]][c("cases", "population", "pvalue")]


################################################################
# Besag-Newell Analysis for Air Polluting Buildings
library(ggplot2)
library(smerc)
library(RColorBrewer)
library(sf)

counties2 <- readRDS("projectdata/county.rds")
ap_bldgs <- readRDS("projectdata/ap_bldgs.rds")

centrs = cbind(counties$CENT_LONG,counties$CENT_LAT)
quantile(counties2$bldg_count)

##
bn100 = bn.test(coords = centrs,
                cases = counties2$bldg_count,
                pop = counties2$Permits,
                cstar = 100,
                alpha = 0.1)
clusters(bn100)

##
bn1000 =  bn.test(coords = centrs,
                  cases = counties2$bldg_count,
                  pop = counties2$Permits,
                  cstar = 1000,
                  alpha = 0.1)
clusters(bn1000)

##
bn200 =  bn.test(coords = centrs,
                  cases = counties2$bldg_count,
                  pop = counties2$Permits,
                  cstar = 200,
                  alpha = 0.1)
clusters(bn200)
# 
# 
# ##
# bn50 = bn.test(coords = centrs,
#                cases = counties2$bldg_count,
#                pop = counties2$Permits,
#                cstar = 50,
#                alpha = 0.1)
# clusters(bn50)
# 
# ##
# bn160 =  bn.test(coords = centrs,
#                  cases = counties2$bldg_count,
#                  pop = counties2$Permits,
#                  cstar = 160,
#                  alpha = 0.1)
# clusters(bn160)

##
bn500 = bn.test(coords = centrs,
                cases = counties2$bldg_count,
                pop = counties2$Permits,
                cstar = 500,
                alpha = 0.1)
clusters(bn500)

##
bn1500 =  bn.test(coords = centrs,
                 cases = counties2$bldg_count,
                 pop = counties2$Permits,
                 cstar = 1500,
                 alpha = 0.1)
clusters(bn1500)

#############################################################
# plotting
# bncol20 = color.clusters(bn20)
bncol100 = color.clusters(bn100)
bncol200 = color.clusters(bn200)
bncol500 = color.clusters(bn500)
bncol1000 = color.clusters(bn1000)
bncol1500 = color.clusters(bn1500)
# bncol2500 = color.clusters(bn2500)

library(ggpubr)

# g1 <- ggplot() +
#         geom_sf(counties2$geometry,mapping=aes(fill=bncol20))+
#         scale_fill_discrete(
#           na.value = "white",
#           guide="none"
#         ) +
#         labs(title = "c* = 20")+
#   theme_minimal() 


g1 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol100))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 100")+
  theme_minimal() 

g2 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol200))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 200")+
  theme_minimal() 


g3 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol500))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 500")+
  theme_minimal() 

g1000 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol1000))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 1000")+
  theme_minimal() 

g4 <- ggplot() +
  geom_sf(counties2$geometry,mapping=aes(fill=bncol1500))+
  scale_fill_discrete(
    na.value = "white",
    guide="none"
  ) +
  labs(title = "c* = 1500")+
  theme_minimal() 



bn_plots <- ggarrange(g2,g3,g1000,g4, ncol=2,nrow=2)
annotate_figure(bn_plots,
                top = text_grob("Besag-Newell Method\nMost Likely Clusters of Pollution Emitting Buildings",
                                color = "black",
                                face = "bold",
                                size = 14))


# look more closely at most likely cluster information
bn100$clusters[[1]][c("cases", "population", "pvalue")]
bn200$clusters[[1]][c("cases", "population", "pvalue")]
bn500$clusters[[1]][c("cases", "population", "pvalue")]
bn1000$clusters[[1]][c("cases", "population", "pvalue")]
bn1500$clusters[[1]][c("cases", "population", "pvalue")]
# bn3000$clusters[[1]][c("cases", "population", "pvalue")]
# bn5000$clusters[[1]][c("cases", "population", "pvalue")]
# bn10k$clusters[[1]][c("cases", "population", "pvalue")]


legend = c("Cluster k = 500", "Cluster k = 1500",
           "Cluster k = 3000",
           "Cluster k = 10000")

legend = c("Cluster k = 500", "Cluster k = 2000",
           "Cluster k = 5000","Cluster k = 10000",
           "Cluster k = 20000")

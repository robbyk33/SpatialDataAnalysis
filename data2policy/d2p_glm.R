library(ggplot2)
library(smerc)
library(RColorBrewer)
library(sf)

counties2 <- readRDS("projectdata/county.rds")

#  Fit GLM log(MH_r) = alpha + beta*bldgs + gamma*ln_pop
# model poisson regression using glm()

# gen log of population and polluting bldgs
ln_pop = log(counties2$Total.Population)
ln_bldgs = log(counties2$bldg_count)

# simple poisson glm
glm1 <- glm(cases~bldg_count,
            data = counties2,family = poisson(link = "log"))

# specification using log(bldgcounts)
glm2 <- glm(cases~as.integer(ln_bldgs),
            data = counties2,family = poisson(link = "log"))

# add ln_pop control term
glm3 <- glm(cases~as.integer(ln_bldgs)+as.integer(ln_pop),
            data = counties2,family = poisson(link = "log"))


glm4 <- glm(cases~offset(ln_pop) + bldg_count,
            data = counties2,family = poisson(link = "log"))

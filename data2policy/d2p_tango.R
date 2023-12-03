################################################################
# Tango Method
library(ggplot2)
library(smerc)
library(RColorBrewer)
library(sf)

counties2 <- readRDS("projectdata/county.rds")


centrs = cbind(counties$CENT_LONG,counties$CENT_LAT)


###################################################
cases = counties2$cases
pop = counties2$Total.Population

# Find distance matrix
d = as.matrix(dist(centrs))

quantile(d)
##################################################
# Exponential decay weight matrix
# use different kappas in defining weights
w1  <- dweights(centrs, kappa = 1)
w7  <- dweights(centrs, kappa = 7)
w10 <- dweights(centrs, kappa = 10)
w15 <- dweights(centrs, kappa = 15)
w20 <- dweights(centrs, kappa = 20)


# Calculate Tango's statistic

# the tango function takes the number of cases,
# the population, and the matrix of weights

(tango_1  <- tango.test(cases, pop, w1))
(tango_7  <- tango.test(cases, pop, w7))
(tango_10 <- tango.test(cases, pop, w10))
(tango_15 <- tango.test(cases, pop, w15))
(tango_20 <- tango.test(cases, pop, w20))



# extracting goodness-of-fit and spatial autocorrelation
# components of tango's index
gof <- c(tango_1$gof,tango_7$gof,
         tango_10$gof,tango_15$gof,
         tango_20$gof)
sa <- c(tango_1$sa,tango_7$sa,
        tango_10$sa,tango_15$sa,
        tango_20$sa)
plot(gof, sa)


# Monte Carlo p-values

# compare monte carlo p-value to chi-square approximation p-value
(tango_mc1 <-  tango.test(cases, pop, w1, nsim = 9999))

(tango_mc7 <-  tango.test(cases, pop, w7, nsim = 9999))

(tango_mc10 <-  tango.test(cases, pop, w10, nsim = 9999))

(tango_mc15 <-  tango.test(cases, pop, w15, nsim = 9999))

(tango_mc20 <-  tango.test(cases, pop, w20, nsim = 9999))


# comparing gof and sa components of tango's index for the observed
# data to the simulated data
# x is observed

plot(tango_mc1)

hist(tango_mc1$gof.sim, xlim = range(c(tango_mc1$gof.sim, tango_mc1$gof)))
abline(v = tango_mc1$gof)

hist(tango_mc1$sa.sim, xlim = range(c(tango_mc1$sa.sim, tango_mc1$sa)))
abline(v = tango_mc1$sa)

plot(tango_mc7)

plot(tango_mc10)

plot(tango_mc15)

plot(tango_mc20)

plot(tango_mc7,
     obs.list = list(pch = 19, col = "purple"),
     sim.list = list(pch = "x", col = "grey"))

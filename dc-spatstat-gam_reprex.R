library(tidyverse)

# cargar paquete
library(readr)
library(spatstat)

# importar datos
preston_crime <- read_rds("pcrime-spatstat.rds.gz.rds")

# dividir ppp
crime_splits <- split(preston_crime)
#crime_splits
plot(crime_splits)

# estimar densidad
crime_densities <- density(crime_splits)
#crime_densities
plot(crime_densities)

# generar fracción de densidad ~ riesgo relativo
frac_violent_crime_density <- crime_densities[[2]] / 
  (crime_densities[[1]] + crime_densities[[2]])

#png("figure/fractional_density.png"),width = 600,height = 600)
plot(frac_violent_crime_density)
#dev.off()


# follow the flow ---------------------------------------------------------

humberside <- preston_crime

library(sp)        # classes and methods for spatial data
# library(sf)        # simple features
library(spatstat)  # spatial point pattern analysis
library(maptools)  # reading and handling spatial objects
library(viridis)   # nice palette
library(fields)    # image.plot

# INTENSITY ESTIMATION ---------------------------------------------------------

levels(humberside$marks) <- c("control","case")

# Split data humberside
cases <- split(humberside)$case
controls <- split(humberside)$control
dens_cases <- density(cases, sigma = bw.ppl)
dens_control <- density(controls, sigma = bw.ppl)
opar <- par(mfrow = c(1,2))
tc <- colourmap(terrain.colors(128), breaks=seq(0, 0.01, length = 129))
plot(dens_cases, col=tc, ribside="bottom")
plot(dens_control, col=tc, ribside="bottom")
par(opar)

# Number of cases and controls
cases$n
controls$n

# DENSITY RATIO ----------------------------------------------------------------

# Average bandwidth
bwcases.ppl <- attr(density(cases, sigma = bw.ppl), "sigma")
bwcontr.ppl <- attr(density(controls, sigma = bw.ppl),'sigma')
bwppl <- (bwcases.ppl+bwcontr.ppl)/2

# Density estimation
smcases.ppl <- density(cases, sigma = bwppl)
smcontrols.ppl <- density(controls, sigma = bwppl)

# Proportion of cases equals to proportion of controls
alphahat <- cases$n/controls$n
smcontrols.ppl$v <- alphahat * smcontrols.ppl$v

# Density ratio
xvals <- smcases.ppl$xcol
yvals <- smcases.ppl$yrow
zvals <- t(smcases.ppl$v)/t(smcontrols.ppl$v)
# image.plot(xvals, yvals, zvals, asp = 1, zlim = c(0, 5))
image.plot(xvals, yvals, zvals, asp = 1)
points(cases, pch = "+", col = 2)
points(controls, pch = "o", col = 3)

# Density ratio with relrisk function
rr <- relrisk(humberside, sigma = bwppl, case = "case", relative = T)
#x11()
plot(rr / alphahat)
points(cases, pch = "+", col = 2)
points(controls, pch = "o", col = 3)

# Density ratio with relrisk function with zoom
rr <- relrisk(humberside, sigma = bwppl, case = "case", relative = T)
#x11()
tc <- colourmap(terrain.colors(128), breaks=seq(0,3,length=129))
plot(rr / alphahat, col=tc)
points(cases, pch = "+", col = 2)
points(controls, pch = "o", col = 4)

# Probability of being a case
zvals <- t(smcases.ppl$v)/(t(smcases.ppl$v) + t(smcontrols.ppl$v))
image.plot(xvals, yvals, zvals, asp = 1)
points(cases, pch = "+", col = 2)
points(controls, pch = "o", col = 4)

# Probability of being a case with relrisk function without correction
rr <- relrisk(humberside, sigma = bwppl, case = "case", relative = F)
tc <- colourmap(terrain.colors(128), breaks=seq(0,1,length=129))
plot(rr, col=tc)
points(cases, pch = "+", col = 2)
points(controls, pch = "o", col = 4)

# GAM MODEL --------------------------------------------------------------------
library(tidyverse)

preston_crime <- read_rds("pcrime-spatstat.rds.gz.rds")
humberside <- preston_crime
levels(humberside$marks) <- c("control","case")

library(mgcv)
humber_df <- as.data.frame(humberside)
humber_df$cases <- 1 * (humber_df$marks == "case")

gam_humber <- gam(cases ~ s(x, y, k = 100), 
                  data = humber_df, 
                  family = poisson(link = log))
                  # family = binomial(link = logit))
plot(humberside$window)
vis.gam(gam_humber, 
        view=c("x","y"),
        plot.type="contour",
        color="heat", 
        type = "link",
        add = TRUE)
plot(humberside$window, add = TRUE)
points(controls, pch = "o", col = 4)
points(cases, pch = "+", col = 2)
# gam.check(gam_humber)

gam_humber %>% summary() #rsquared + deviance explained
gam_humber %>% broom::tidy()
gam_humber %>% broom::tidy(parametric=TRUE)
gam_humber %>% broom::glance()
gam_humber %>% broom::augment()
gam_humber %>% broom::augment(type.predict="link")
gam_humber %>% broom::augment(type.predict="response")
gam_humber %>% broom::augment(type.predict="term")
gam_humber %>% plot()
gam_humber %>% vis.gam()
gam_humber %>% getViz() %>% sm(.,1) %>% plot()  



library(mgcViz)

gam_humber_viz <- getViz(gam_humber)

gam_humber_viz_raster <- 
  plot(sm(gam_humber_viz, 1)) + 
  l_fitRaster() +
  l_fitContour()

gam_humber_viz_raster + coord_fixed()

gam_humber_viz_raster$data$fit %>% 
  as_tibble() %>% 
  ggplot() +
  geom_contour_filled(aes(x = x, y = y, z = z)) + 
  coord_fixed()

gam_humber_viz_raster$data$fit %>% 
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = z)) +
  scale_fill_viridis_c() +
  coord_fixed()

# according to page 359 of GAM in R book

gam(cases ~ s(x, y,bs = "gp",k = 100), 
    data = humber_df, 
    family = binomial(link = logit)) %>% 
  getViz() %>% 
  plot(sm(., 1)) + 
  l_fitRaster() +
  l_fitContour() + 
  coord_fixed()

# gam(cases ~ s(x, y,bs = "sos",k = 100), 
#     data = humber_df, 
#     family = binomial(link = logit)) %>% 
#   getViz() %>% 
#   plot(sm(., 1)) + 
#   l_fitRaster() +
#   l_fitContour()


# test assumptions --------------------------------------------------------

from_augment <- augment(gam_humber,
                        type.residuals="deviance",
                        type.predict="response")
gb<-list(data=from_augment %>% pull(.resid),
         coords=from_augment %>% select(x,y) %>% as.matrix())
# FIRST:
# variaogram of deviance residuals
# spatial autocorrelation
# uncorrelated residuals should give a more or less flat variogram
library(geoR)
plot(variog(gb,max.dist=100)) 
# SECOND:
# deviance residuals against fitted probabilities
# normal distribution or residuals
# plot(fitted(m10),residuals(m10)) #ggplot replica
from_augment %>% 
  ggplot(aes(x = .fitted,y = .resid)) +
  geom_point() +
  geom_smooth()



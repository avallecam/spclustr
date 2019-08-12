set.seed(33)
# _AREAL DATA ----------------------------------------------------------

#london EU remain or leave referendum data

# Load packages ----------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
library(raster)
library(cartogram)
library(rgeos) #gArea to calculate area of geometry
library(spdep) #spatial dependency or correlation
theme_set(theme_bw())
list.files("data-dc/")

# import data -------------------------------------------------------------

london_ref <- read_rds("data-dc/london_eu.rds.gz.rds")
class(london_ref) #spatial polygons data frame
str(london_ref)

#convert to sf
london_ref_sf <- st_as_sf(london_ref) 
class(london_ref_sf)
str(london_ref_sf)

# dc01 - plot --------------------------------------------------------------------
# explore data

# See what information we have for each borough
london_ref_sf %>% as_tibble()
summary(london_ref)

# Which boroughs voted to "Leave"?
london_ref$NAME[london_ref$Leave > london_ref$Remain]

london_ref_sf %>% 
  as_tibble() %>% 
  filter(Leave>Remain) %>% pull(NAME)

# Plot a map of the percentage that voted "Remain"
spplot(london_ref, zcol = "Pct_Remain")

# my tmap way
tm_shape(london_ref_sf) +
  tm_polygons(col="Pct_Remain",style="cont") +
  tm_text(text = "NAME",size = 0.5)

# dc02 - cartogram --------------------------------------------------------------------
# cartogram to account for population-biased areas

# large areas 
# carry more visual "weight" than small areas, 
# many people live in the small areas.
# 
# correct by cartogram.  
# controlled distortion of region area, 
# proportional to a desired quantity, e.g. population. 

# Make a scatterplot of electorate vs borough area
names(london_ref)
# Deviation from a straight line shows the degree of misrepresentation.
plot(london_ref$Electorate, gArea(london_ref, byid = TRUE))

# Make a cartogram, scaling the area to the electorate
# generates a spatialpolygondataframe with modified geometry (multipolygon)
carto_ref <- cartogram(london_ref, "Electorate")
carto_ref_sf <- st_as_sf(carto_ref)
plot(carto_ref)

# Check the linearity of the electorate-area plot
plot(carto_ref$Electorate, gArea(carto_ref, byid = TRUE))

# Make a fairer map of the Remain percentage
spplot(carto_ref, "Pct_Remain")

# check the cartogram with tmaps
london_ref_sf %>% as_tibble() %>% glimpse()
carto_ref_sf %>% as_tibble() %>% glimpse()

tm_shape(carto_ref_sf) +
  #tm_polygons(col="TOTAL_POP",style="cont") +
  tm_polygons(col="Pct_Remain",style="cont") +
  tm_text(text = "NAME",size = 0.5)


# dc03 - moran --------------------------------------------------------------------
# spatial autocorrelation test
#?moran.test
#?getisord

# Make neighbor list
borough_nb <- poly2nb(london_ref)
class(borough_nb)
str(borough_nb)

# Get center points of each borough
borough_centers <- coordinates(london_ref)

# Show the connections
plot(london_ref); plot(borough_nb, borough_centers, add = TRUE)

# Map the total pop'n
spplot(london_ref, zcol = "TOTAL_POP")

# Run a Moran I test on total pop'n
# in this test you can not reject the
# the Ho of no spatial correlation
moran.test(
  london_ref$TOTAL_POP, 
  nb2listw(borough_nb)
)
moran_pop <- moran.mc(
  x= london_ref$TOTAL_POP, 
  listw = nb2listw(borough_nb),
  nsim = 999
)
moran_pop
#str(moran_pop)
hist(moran_pop$res)
abline(v=moran_pop$statistic,col="red")

# Map % Remain
spplot(london_ref, zcol = "Pct_Remain")

# Run a Moran I MC test on % Remain
# here you can reject
# the Ho of no spatial correlation
moran_remain <- moran.mc(
  london_ref$Pct_Remain, 
  nb2listw(borough_nb), 
  nsim = 999
)
moran_remain
#str(moran_remain)
hist(moran_remain$res)
abline(v=moran_remain$statistic,col="red")

# dc04: SIR, CI, ExPr --------------------------------------------------------------------

london <- read_rds("data-dc/london_2017_2.rds.gz.rds")
class(london) #spatial polygons data frame
#str(london)
london %>% as_tibble()

# dc05: GLM --------------------------------------------------------------------

# Fit a poisson GLM.
model_flu <- glm(
  Flu_OBS ~ HealthDeprivation, 
  offset = log(TOTAL_POP), 
  data = london, 
  family = poisson)

# Is HealthDeprivation significant?
summary(model_flu)

# Put residuals into the spatial data.
london$Flu_Resid <- residuals(model_flu)

# Map the residuals using spplot
spplot(london, "Flu_Resid")

# Compute the neighborhood structure.
library(spdep)
borough_nb <- poly2nb(london)

# Test spatial correlation of the residuals.
moran.mc(london$Flu_Resid, listw = nb2listw(borough_nb), nsim = 999)

#then, try to add other covariates that could explain outcome (age structure?)
#if still presenting correlated residuals
#add spatial term explicity
#Y=xb+S(x,y)
#one alternative
#BYM model
#S_i ~ N(mean(S_j~i),var_i^2)
#a type of conditonal autoregression or CAR model
#use bayesian inference to estimate the parameters
#carbayes r package
#bayesx r package
#spatial variation explained!


# dc06: spatial variation explained! --------------------------------------

#Bayesian statistical models return samples 
#of the parameters of interest (the "posterior" distribution) 
#based on some "prior" distribution which is then updated by the data. 
#The Bayesian modeling process returns a number of samples 
#from which you can compute the mean, or an exceedence probability, 
#or any other quantity you might compute from a distribution.


# BYM-CAR model 
# (Besag, York and Mollié)
# conditional autocorrelation
# https://ij-healthgeographics.biomedcentral.com/articles/10.1186/1476-072X-6-39

model_flu %>% broom::tidy()
model_flu %>% broom::confint_tidy()

library(R2BayesX)

# Fit a Bayesian GLM
bayes_flu <- bayesx(Flu_OBS ~ HealthDeprivation, 
                    offset = log(london$TOTAL_POP), 
                    family = "poisson", data = as.data.frame(london), 
                    control = bayesx.control(seed = 17610407))

# Summarize it                    
summary(bayes_flu)

# Look at the samples from the Bayesian model
plot(samples(bayes_flu))

# Compute adjacency objects
borough_nb <- poly2nb(london)
borough_gra <- nb2gra(borough_nb)

# Fit spatial model
flu_spatial <- bayesx(
  Flu_OBS ~ HealthDeprivation + sx(i, bs = "spatial", map = borough_gra),
  offset = log(london$TOTAL_POP),
  family = "poisson", data = data.frame(london), 
  control = bayesx.control(seed = 17610407)
)

# Summarize the model
summary(flu_spatial)

# Map the fitted spatial term only (#Markov Random Field)
london$spatial <- fitted(flu_spatial, term = "sx(i):mrf")[, "Mean"]
spplot(london, zcol = "spatial")

# Map the residuals
london$spatial_resid <- residuals(flu_spatial)[, "mu"]
spplot(london, zcol = "spatial_resid")

# Test residuals for spatial correlation
moran.mc(london$spatial_resid, nb2listw(borough_nb), 999)

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

# dc04 --------------------------------------------------------------------



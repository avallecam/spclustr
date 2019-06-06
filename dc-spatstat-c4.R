set.seed(33)
# GEOSTATISTICAL DATA ----------------------------------------------------------

# Load the spatstat package
library(tidyverse)
library(spatstat)

#ph_grid <- read_rds("data-dc/ph_grid.rds.gz.rds")
geo_bounds <- read_rds("data-dc/ca_geo_bounds.rds.gz.rds")
ca_geo <- read_rds("data-dc/ca_geo.rds.gz.rds")
class(ca_geo) #ppp
str(ca_geo)
str(ca_geo, 1)
ca_geo
# Get some summary information on the dataset
summary(ca_geo)
names(ca_geo)
summary(ca_geo$pH)
# Get a summary of the acidity (pH) values
summary(ca_geo$pH)
# Look at the distribution
hist(ca_geo$pH)
# Make a vector that is TRUE for the missing data
miss <- is.na(ca_geo$pH)
table(miss)
# Plot a map of acidity
spplot(ca_geo[!miss, ],zcol="pH")


# fitting trend surface ---------------------------------------------------

# Are they called lat-long, up-down, or what?
coordnames(ca_geo)

# Complete the formula
m_trend <- lm(pH ~ x + y, as.data.frame(ca_geo))
m_trend %>% broom::tidy()
# Check the coefficients
summary(m_trend)
plot(m_trend)


# predicting from a trend surface -----------------------------------------
#Your next task is to compute the pH at the locations that have missing data in the source. 
#You can use the predict() function on the fitted model from the previous exercise for this.

# Make a vector that is TRUE for the missing data
miss <- is.na(ca_geo$pH)

# Create a data frame of missing data
ca_geo_miss <- as.data.frame(ca_geo)[is.na(ca_geo$pH),]

# Predict pH for the missing data
predictions <- predict(m_trend, newdata =ca_geo_miss, se.fit = TRUE)

#Alkaline soils are those with a pH over 7. 
#Our linear model gives us estimates and standard deviation based on a normal (Gaussian) assumption. 
#Compute the probability of the soil being over 7 using pnorm() 
#with the mean and standard deviation values from the prediction data.

# Compute the exceedence probability
pAlkaline <- 1 - pnorm(7, mean = predictions$fit, sd = predictions$se.fit)
hist(pAlkaline)


# Variogram estimation ----------------------------------------------------
#"Everything is related to
#everything else, but near
#things are more related than
#distant things."

# a fitted variogram describe the spatial correlation of samples
# enough to make geostatistical predictions on any location
# the process of interpolating geostatistical data from a variogram 
# is called kriging

# variogram cloud different that a variogram plot (mean per bin of distance)

# ca_geo, miss have been pre-defined
ls.str()
# Make a cloud from the non-missing data up to 10km
plot(variogram(pH ~ 1, ca_geo[!miss, ], cloud = TRUE, cutoff = 10000))
# Make a variogram of the non-missing data
plot(variogram(pH ~ 1, ca_geo[!miss, ]))

# Variogram with spatial trend --------------------------------------------

# ca_geo, miss have been pre-defined
ls.str()
# See what coordinates are called
coordnames(ca_geo)
# The pH depends on the coordinates
ph_vgm <- variogram(pH ~ x + y, ca_geo[!miss, ])
plot(ph_vgm)


# Variogram model fitting -------------------------------------------------

library(gstat)

# ca_geo, miss, ph_vgm have been pre-defined
ls.str()

# Eyeball the variogram and estimate the initial parameters
nugget <- 0.16
psill <- 0.14
range <- 10000

# Fit the variogram
v_model <- fit.variogram(
  ph_vgm, 
  model = vgm(
    model = "Ste",
    nugget = nugget,
    psill = psill,
    range = range,
    kappa = 0.5
  )
)

# Show the fitted variogram on top of the binned variogram
plot(ph_vgm, model = v_model)
print(v_model)


# Kriging -----------------------------------------------------------------

# Set the trend formula and the new data
km <- krige(pH ~ x + y, ca_geo[!miss, ], newdata = ca_geo[miss, ], model = v_model)
names(km)

# Plot the predicted values
spplot(km, "var1.pred")

# Compute the probability of alkaline samples, and map
km$pAlkaline <- 1 - pnorm(7, mean = km$var1.pred, sd = sqrt(km$var1.var))
spplot(km, "pAlkaline")


# making a prediction grid ------------------------------------------------

#You have been asked to produce an alkaline probability map over the study area. 
#To do this, you are going to do some kriging via the krige() function. 
#This requires a SpatialPixels object which will take a bit of data manipulation to create. 
#You start by defining a grid, creating points on that grid, cropping to the study region, 
#and then finally converting to SpatialPixels. On the way, you'll meet some new functions.
#
#GridTopology() defines a rectangular grid. It takes three vectors of length two as inputs. 
#The first specifies the position of the bottom left corner of the grid. 
#The second specifies the width and height of each rectangle in the grid, 
#and the third specifies the number of rectangles in each direction.
#
#To ensure that the grid and the study area have the same coordinates, some housekeeping is involved. 
#SpatialPoints() converts the points to a coordinate reference system (CRS), 
#or projection (different packages use different terminology for the same concept). 
#The CRS is created by wrapping the study area in projection(), then in CRS(). 
#For the purpose of this exercise, you don't need to worry about exactly what these functions do, 
#only that this data manipulation is necessary to align the grid and the study area.
#
#Now that you have that alignment, crop(), as the name suggests, crops the grid to the study area.
#
#Finally, SpatialPixels() converts the raster cropped gridpoints to the equivalent sp object.

# Plot the polygon and points
plot(geo_bounds); points(ca_geo)

# Find the corners of the boundary
bbox(geo_bounds)

# Define a 2.5km square grid over the polygon extent. The first parameter is
# the bottom left corner.
grid <- GridTopology(c(537853,5536290), c(2500, 2500), c(72, 48))

# Create points with the same coordinate system as the boundary
library(raster)
gridpoints <- SpatialPoints(grid, proj4string = CRS(projection(geo_bounds)))
plot(gridpoints)

# Crop out the points outside the boundary
cropped_gridpoints <- crop(gridpoints, geo_bounds)
plot(cropped_gridpoints)

# Convert to SpatialPixels
spgrid <- SpatialPixels(cropped_gridpoints)
coordnames(spgrid) <- c("x", "y")
plot(spgrid)


# gridded predictions -----------------------------------------------------

#You can now compute kriged estimates over the grid using the variogram model 
#from before (v_model) and the grid of SpatialPixels

# Do kriging predictions over the grid
ph_grid <- krige(pH ~ x + y, ca_geo[!miss, ], newdata = spgrid, model = v_model)

# Calc the probability of pH exceeding 7
ph_grid$pAlkaline <- 1 - pnorm(7, mean = ph_grid$var1.pred, sd = sqrt(ph_grid$var1.var))

# Map the probability of alkaline samples
spplot(ph_grid, zcol = "pAlkaline")


# autokriging at point locations ------------------------------------------

library(automap)

# Kriging with linear trend, predicting over the missing points
ph_auto <- autoKrige(
  pH ~ x + y, 
  input_data = ca_geo[!miss, ], 
  new_data = ca_geo[miss, ], 
  model = "Mat"
)

# Plot the variogram, predictions, and standard error
plot(ph_auto)


# auto-kriging over a grid ------------------------------------------------

# Auto-run the kriging
ph_auto_grid <- autoKrige(pH ~ x + y, input_data = ca_geo[!miss,], new_data = spgrid)

# Remember predictions from manual kriging
plot(ph_grid)

# Plot predictions and variogram fit
plot(ph_auto_grid)

# Compare the variogram model to the earlier one
v_model
ph_auto_grid$var_model


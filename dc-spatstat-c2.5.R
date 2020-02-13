
set.seed(33)

# _point pattern analysis_ ----------------------------------------------------------

# Load the spatstat package
library(tidyverse)
library(spatstat)
library(sf)
library(tmap)
theme_set(theme_bw())

# _SPATIO-TEMPORAL ------------------------------------------------------


# descriptive -------------------------------------------------------------

# 01 ----------------------------------------------------------------------

sasq <- read_rds("data-dc/sasquatch.rds.gz.rds")
class(sasq) #ppp
sasq

# Get a quick summary of the dataset
summary(sasq)

# Plot unmarked points
plot(unmark(sasq))

# Plot the points using a circle sized by date
plot(sasq, which.marks = "date")
# conclusion: data is clustered in the north
# not homogeneously 

# 02 ----------------------------------------------------------------------

# Show the available marks
names(marks(sasq))

# Histogram the dates of the sightings, grouped by year
hist(marks(sasq)$date, "years", freq = TRUE)

# Plot and tabulate the calendar month of all the sightings
plot(table(marks(sasq)$month))

# Split on the month mark
sasq_by_month <- split(sasq, "month", un = TRUE)

# Plot monthly maps
plot(sasq_by_month)

# Plot smoothed versions of the above split maps
plot(density(sasq_by_month))


# quiz --------------------------------------------------------------------

#Are the sightings uniform over the study time period? Is there an annual pattern? 
#Annual sightings peaked around 2004, peak month is September.
plot(table(marks(sasq)$month))
plot(table(marks(sasq)$year))
#Annual sightings increased to 2004, then leveled off, clear monthly pattern.


# ppp2sf ------------------------------------------------------------------


ppp2sf <- function (ppp) {
  
  data <- tibble::tibble(x = ppp$x, y = ppp$y)
  if (!is.null(ppp$marks)) data$marks = ppp$marks
  # data_sf <- st_as_sf(data, coords = c("x", "y"))
  data_sf <- data
  
  if (!is.null(ppp$window$bdry)) {
    bnd <- as.matrix(as.data.frame(ppp$window$bdry[[1]]))
    bnd <- rbind(bnd, bnd[1, ])
    bnd <- st_sf(id = 1, geometry = st_sfc(st_polygon(list(bnd))))
  } else {
    bnd <- cbind(c(ppp$window$xrange, rev(ppp$window$xrange)),
                 rep(ppp$window$yrange, each = 2))
    bnd <- rbind(bnd, bnd[1, ])
    bnd <- st_sf(id = 1, geometry = st_sfc(st_polygon(list(bnd))))
  }
  
  return(list(data = data_sf, bnd = bnd))
}


house_poly <- ppp2sf(sasq)$bnd %>% 
  st_as_sf(remove = F,
           crs = 7801, agr = "constant")
house <- ppp2sf(sasq)$data %>% 
  st_as_sf(coords = c("x", "y"), remove = F,
           crs = 7801, agr = "constant")

house_poly %>% 
  tm_shape() +
  tm_polygons(alpha=0) +
  tm_shape(house) +
  tm_dots() +
  tm_scale_bar(width = 0.75)


# inferential -------------------------------------------------------------

library(splancs)

# 03 ----------------------------------------------------------------------

# Get a matrix of event coordinates
sasq_xy <- as.matrix(coords(sasq))

# Check the matrix has two columns
dim(sasq_xy)

# Get a vector of event times
sasq_t <- marks(sasq)$date

# Extract a two-column matrix from the ppp object
sasq_poly <- as.matrix(as.data.frame(Window(sasq)))
dim(sasq_poly)

# Set the time limit to 1 day before and 1 after the range of times
tlimits <- range(sasq_t) + c(-1, 1)

# Scan over 400m intervals from 100m to 20km
s <- seq(100, 20000, by = 400)

# Scan over 14 day intervals from one week to 31 weeks 
tm <- seq(7, 7 * 31, by = 14)

# 04 ----------------------------------------------------------------------

# Run 999 simulations 
sasq_mc <- stmctest(sasq_xy, sasq_t, sasq_poly, tlimits, s, tm, nsim = 999, quiet = TRUE)
names(sasq_mc)

# Histogram the simulated statistics and add a line at the data value
ggplot(data.frame(sasq_mc), aes(x = t)) +
  geom_histogram(binwidth = 1e13) +
  geom_vline(aes(xintercept = t0))

# Compute the p-value as the proportion of tests greater than the data
sum(sasq_mc$t > sasq_mc$t0) / 999

#explain clustering
#maybe one sight tent to drive the most curious people


# _APPLY ------------------------------------------------------------------



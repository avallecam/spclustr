#' ---
#' title: "bivariate point pattern"
#' subtitle: "mape_r-01"
#' author: "Andree Valle-Campos"
#' date: "13/2/2020"
#' output: 
#'   html_document:
#'     toc: TRUE
#'     number_sections: true
#'     toc_float: TRUE
#'     code_folding: hide
#'     #df_print: kable
#'     highlight: tango
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      warning=FALSE,
                      message = FALSE,
                      fig.align = "center")
options(knitr.kable.NA = '.',digits = 2)

#' 
#' # load packages
#' 
## ------------------------------------------------------------------------
# once per session
library(tidyverse)
#library(spatstat)
#library(sf)

# additional configuration
theme_set(theme_bw())
set.seed(33)

#' 
#' # import data
#' 
## ------------------------------------------------------------------------
preston_crime <- read_rds("data-dc/pcrime-spatstat.rds.gz.rds")

#' 
#' ## explore the object
#' 
#' __En tu computadora local,__ explora cuál es la salida de cada una de estas acciones
#' 
## ----eval=FALSE----------------------------------------------------------
## # always!
## class(preston_crime)
## # ppp is a class from the spatstat package
## 
## # Load the spatstat package
## library(spatstat)
## preston_crime %>% print.ppp()
## 
## # Get some summary information on the dataset
## summary(preston_crime)
## 
## # list of attributes
## attributes(preston_crime)
## 
## # explore attributes with $
## preston_crime$markformat
## 
## # window is an exclusive element from ppp objects
## preston_crime$window
## 
## # class it is?
## class(preston_crime$window)
## 
## # object structure
## str(preston_crime)

#' 
## ----eval=FALSE,echo=FALSE-----------------------------------------------
## # library(raster)
## # preston_osm <- read_rds("data-dc/osm_preston_gray.rds.gz.rds")
## # class(preston_osm) #raster
## # preston_osm
## # str(preston_osm)
## # summary(preston_osm)

#' 
#' # transform data
#' 
#' el 
#' 
## ------------------------------------------------------------------------

# from ppp to tibble -----------------------

gpsrino <- preston_crime %>% as_tibble()
gpsrino %>% class()
gpsrino

# fromm tibble to sf -----------------------

library(sf)
house <- st_as_sf(gpsrino, coords = c("x", "y"), remove = F,
                  crs = 27561, agr = "constant")
# notes
# this always works for peru!
# peru: crs = 4326
# CRS("+proj=longlat +datum=WGS84")

house %>% class()
house

#' 
#' # exploratory plots
#' 
#' ## using geom_point
#' 
## ------------------------------------------------------------------------
house %>% 
  
  #dplyr
  mutate(marks=fct_relevel(marks,"Violent crime")) %>% 
  
  #ggplot2
  ggplot(aes(x = x, y = y, color = marks, size = marks)) +
  
  #geometry
  geom_point() +
  coord_fixed(ratio = 1) +
  
  #aestetics
  scale_color_manual(values = c("red","black")) +
  scale_size_manual(values = c(1.5,0.5))

#' 
#' ## using geom_sf
#' 
## ------------------------------------------------------------------------
house %>%  
  
  #dplyr
  mutate(marks=fct_relevel(marks,"Violent crime")) %>% 
  
  #ggplot2
  ggplot(aes(color=marks,size=marks)) +
  
  #geometry
  geom_sf() +
  coord_sf() +
  
  #aestetics
  scale_color_manual(values = c("red","black")) +
  scale_size_manual(values = c(1.5,0.5))

#' 
#' # kernel smoothing
#' 
## ------------------------------------------------------------------------
house %>%  
  
  #dplyr
  mutate(marks=fct_relevel(marks,"Violent crime")) %>% 
  
  #ggplot2
  ggplot() +
  
  #geometry
  geom_sf() +
  coord_sf() +
  
  facet_grid(~marks)

#' 
#' ## binwidth selection
#' 
#' ### histogram
#' 
#' __binwidth = __
#' 
#' > The width of the bins. Can be specified as a numeric value or as a function that calculates width from unscaled x. Here, "unscaled x" refers to the original x values in the data, before application of any scale transformation. When specifying a function along with a grouping structure, the function will be called once per group. The default is to use the number of bins in bins, covering the range of the data. You should always override this value, exploring multiple widths to find the best to illustrate the stories in your data.
#' 
## ------------------------------------------------------------------------
house %>% 
  ggplot(aes(x = x)) +
  geom_histogram()

house %>% 
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 10)

house %>% 
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 1000)

#' 
#' ### density
#' 
#' __stat_density__
#' 
#' > Computes and draws kernel density estimate, which is a smoothed version of the histogram. This is a useful alternative to the histogram for continuous data that comes from an underlying smooth distribution.
#' 
#' __bw = __
#' 
#' > The smoothing bandwidth to be used. If numeric, the standard deviation of the smoothing kernel. If character, a rule to choose the bandwidth, as listed in stats::bw.nrd().
#' 
## ------------------------------------------------------------------------
house %>% 
  ggplot(aes(x = x)) +
  stat_density() 
# default: bw = "nrd0" method
# rule-of-thumb for choosing the bandwidth 
# of a Gaussian kernel density estimator

house %>% 
  ggplot(aes(x = x)) +
  stat_density(bw = 1000)

house %>% 
  ggplot(aes(x = x)) +
  stat_density(bw = 100)

#' 
#' ## density 2d
#' 
#' __stat_density_2d__
#' 
#' > Perform a 2D kernel density estimation using MASS::kde2d() and display the results with contours. This can be useful for dealing with overplotting. This is a 2d version of geom_density().
#' 
#' __h =__
#' 
#' > Bandwidth (vector of length two). If NULL, estimated using MASS::bandwidth.nrd()
#' 
## ------------------------------------------------------------------------
house %>% 
  ggplot() +
  stat_density_2d(aes(x = x, y = y,fill = ..level..),
                  alpha=0.3,
                  geom = "polygon"
                  ) +
  coord_fixed(ratio = 1)

house %>% 
  ggplot() +
  stat_density_2d(aes(x = x, y = y,fill = ..level..),
                  alpha=0.3,
                  geom = "polygon",
                  h = c(50,50)
                  ) +
  coord_fixed(ratio = 1)

house %>% 
  ggplot() +
  stat_density_2d(aes(x = x, y = y,fill = ..level..),
                  alpha=0.3,
                  geom = "polygon",
                  h = c(5000,5000)
                  ) +
  coord_fixed(ratio = 1)

#' 
#' ### how to choose the better bandwidth?
#' 
## ----echo=FALSE----------------------------------------------------------
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

#' 
#' __bw.scott__
#' 
#' > Use Scott's rule of thumb to determine the smoothing bandwidth for the kernel estimation of point process intensity.
#' 
#' > This function selects a bandwidth sigma for the kernel estimator of point process intensity computed by density.ppp.
#' 
## ------------------------------------------------------------------------
# extract window to sf -----------
window_boundary <- ppp2sf(preston_crime) %>% 
  pluck(2) %>% 
  st_as_sf(remove = F,
           crs = 27561, agr = "constant")

library(maptools)

house_g <- house %>% select(geometry)
house_poly <- window_boundary %>% st_buffer(dist = 0) %>% st_union() #needs to be cleaner!
p.sp  <- as(house_g, "Spatial")  # Create Spatial* object
p.ppp <- as(p.sp, "ppp")      # Create ppp object
Window(p.ppp) <- as.owin(as(house_poly, "Spatial"))
h_ppp <- bw.scott(p.ppp) #bw.ppl(p.ppp)
h_ppp

#' 
## ------------------------------------------------------------------------
house %>% 
  ggplot(aes(x = x, y = y)) +
  
  stat_density_2d(aes(fill = ..level..),
                  alpha=0.3,
                  geom = "polygon",
                  h = h_ppp
                  ) +
  
  coord_fixed(ratio = 1)

#' 
#' 
#' ### use multiple geometries
#' 
## ------------------------------------------------------------------------
house %>% 
  ggplot(aes(x = x, y = y)) +
  
  stat_density_2d(aes(fill = ..level..),
                  alpha=0.3,
                  geom = "polygon",
                  h = h_ppp
                  ) +
  
  geom_point() +
  coord_fixed(ratio = 1)

house %>% 
  ggplot() +
  
  stat_density_2d(aes(x = x, y = y,fill = ..level..),
                  alpha=0.3,
                  geom = "polygon",
                  h = h_ppp
                  ) +
  
  geom_sf(alpha=0.05,size=0.5) +
  coord_sf()

house %>% 
  ggplot() +
  
  stat_density_2d(aes(x = x, y = y,fill = ..level..),
                  alpha=0.3,
                  geom = "polygon",
                  h = h_ppp
                  ) +
  
  geom_sf(alpha=0.05,size=0.5) +
  coord_sf() +
  
  facet_grid(~marks)

#' 
#' ### add aestetics
#' 
## ------------------------------------------------------------------------
house %>% 
  ggplot() +
  stat_density_2d(aes(x = x, y = y,fill = ..level..),
                  alpha=0.3,
                  geom = "polygon",
                  h = h_ppp
                  ) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient2("Case\ndensity",
                       low = "yellow",
                       mid = "gold",
                       high = "red",
                       guide = FALSE
                       ) +
  facet_grid(~marks)

#' 
#' # bivariate point pattern
#' 
#' __sigma__
#' 
#' > Standard deviation of isotropic smoothing kernel. Either a numerical value, or a function that computes an appropriate value of sigma.
#' 
## ----eval=FALSE----------------------------------------------------------
## crime_splits <- split(preston_crime)
## plot(crime_splits)
## crime_densities <- density(crime_splits,sigma=h_ppp)
## crime_densities
## plot(crime_densities)
## frac_violent_crime_density <- crime_densities[[2]] /
##   (crime_densities[[1]] + crime_densities[[2]])
## 
## #png(paste0("figure/zg-frac_den-pcr_v-ML-",name_community,".png"),width = 600,height = 600)
## plot(frac_violent_crime_density)
## #dev.off()

#' 
#' # ending
#' 
## ----eval=FALSE,echo=FALSE,message=FALSE---------------------------------
## #generar material para estudiantes
## knitr::purl("mape_r-01.Rmd", output = "mape_r-01.R", documentation = 2)
## #purl("r02.Rmd", output = "r02.R", documentation = 2)


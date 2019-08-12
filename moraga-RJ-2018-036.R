####################################################################
# Small Area Disease Risk Estimation and Visualization using R
# by Paula Moraga 
# (adapted to a more tidyverse-style by Andree Valle)
####################################################################

# The R package is not on CRAN because it uses some external C libraries that make difficult to build the binaries.
# Therefore, we need to install it adding the URL of the INLA repository:
  
#install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)

library(tidyverse)
library(sf)

# Data ---------------------------------
library(SpatialEpi)
data(pennLC)
attributes(pennLC)

# Y: Observed cases ---------------------------------
# X: Smokers proportions ---------------------------------

#outcome at county level + covariates (race,sex,age_strata)
d <- pennLC$data %>% 
  as_tibble() %>% 
  group_by(county) %>% 
  summarise(Y=sum(cases),
            pop=sum(population)) %>% 
  ungroup() %>% 
  
  #add exposure at county level
  left_join(pennLC$smoking) %>% 
  rownames_to_column() %>% 
  
  #add polygon to tibble
  left_join(pennLC$spatial.polygon %>% 
              st_as_sf() %>% 
              as_tibble() %>% 
              rownames_to_column()) 

# Expected cases (using stratas!) ---------------------------------
e <- pennLC$data %>% 
  as_tibble() %>% 
  select(county,cases,population,race,gender,age)

e %>% count(race,gender,age)

population <- e$population
cases <- e$cases
n.strata <- 16 # = 2 races * 2 genders * 4 age bands
E <- expected(population, cases, n.strata)

# SIR ---------------------------------

map <- d %>% 
  left_join(
    e %>% 
      select(county) %>% 
      distinct() %>% 
      mutate(E=E)
  ) %>% 
  mutate(SIR=Y/E) %>% 
  # add data to map 
  st_as_sf() %>% 
  # transform to SpatialPolygonDataFrame
  as('Spatial')

# Mapping SIR ---------------------------------
map %>% 
  st_as_sf() %>% 
  ggplot(aes(fill=SIR)) +
  geom_sf() +
  scale_fill_distiller(palette = "YlOrRd",direction = 1) +
  #scale_fill_continuous(trans = 'reverse') +
  guides(fill = guide_colorbar(reverse=F)) +
  theme_bw() +
  labs(title = "Standardize Incidence Ratio",
       subtitle = "Political Map: Polygon boundaries for each county")

# alternative - cartogram
plot(map)
map_carto <- cartogram(map, "pop", itermax=5)
plot(map_carto)

map_carto %>% 
  st_as_sf() %>% 
  ggplot(aes(fill=SIR)) +
  geom_sf() +
  scale_fill_distiller(palette = "YlOrRd",direction = 1) +
  #scale_fill_continuous(trans = 'reverse') +
  guides(fill = guide_colorbar(reverse=F)) +
  theme_bw() +
  labs(title = "Standardize Incidence Ratio",
       subtitle = "Cartogram: polygon area proportional to the population size")

# Neighbourhood matrix ---------------------------------
library(spdep)
library(INLA)
nb <- poly2nb(map)
head(nb)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

# Inference using INLA ---------------------------------

#index both random effects
map$re_u <- 1:nrow(map@data) #spatial residual variation
map$re_v <- 1:nrow(map@data) #modeling unstructure noise

#create formula
#iid: independent and identically distributed
formula <- 
  Y ~ # outcome
  smoking + # exposure
  f(re_u, model = "besag", graph = g) + #random effect - spatial
  f(re_v, model = "iid") #random effect - noise

res <- inla(formula, 
            family = "poisson", 
            data = map@data, 
            E = E,
            control.predictor = list(compute = TRUE))

# Results ---------------------------------

summary(res)

marginal <- inla.smarginal(res$marginals.fixed$smoking)
marginal <- data.frame(marginal)
ggplot(marginal, aes(x = x, y = y)) + 
  geom_line() +
  geom_vline(xintercept = 0, col = "blue") + 
  labs(x = expression(beta[1]), 
       y = "Density") +
  theme_bw()
  
head(res$summary.fitted.values)

map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]

# Mapping disease risk ---------------------------------
map %>% 
  st_as_sf() %>% 
  ggplot(aes(fill=RR)) +
  geom_sf() +
  scale_fill_distiller(palette = "YlOrRd",direction = 1) +
  #scale_fill_continuous(trans = 'reverse') +
  guides(fill = guide_colorbar(reverse=F)) +
  theme_bw()

# alternative - cartogram
plot(map)
map_carto <- cartogram(map, "pop", itermax=5)
plot(map_carto)

map_carto %>% 
  st_as_sf() %>% 
  ggplot(aes(fill=RR)) +
  geom_sf() +
  scale_fill_distiller(palette = "YlOrRd",direction = 1) +
  #scale_fill_continuous(trans = 'reverse') +
  guides(fill = guide_colorbar(reverse=F)) +
  theme_bw()

# Range of values of SIRs and RRs ---------------------------------

range(map@data$SIR)
range(map@data$RR)

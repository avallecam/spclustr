set.seed(33)
# _BIVARIATE DATA ----------------------------------------------------------

# Load the spatstat package
library(tidyverse)
library(spatstat)
theme_set(theme_bw())

preston_crime <- read_rds("data-dc/pcrime-spatstat.rds.gz.rds")
class(preston_crime) #ppp
preston_crime
# Get some summary information on the dataset
summary(preston_crime)
str(preston_crime)
attributes(preston_crime)
class(preston_crime$window)

library(raster)
preston_osm <- read_rds("data-dc/osm_preston_gray.rds.gz.rds")
class(preston_osm) #raster
preston_osm
str(preston_osm)
summary(preston_osm)

# Get a table of marks
table(marks(preston_crime))

# Define a function to create a map
preston_map <- function(cols = c("green","red"), cex = c(1, 1), pch = c(1, 1)) {
  plotRGB(preston_osm) # from the raster package
  plot(preston_crime, cols = cols, pch = pch, cex = cex, add = TRUE, show.window = TRUE)
}

# Draw the map with colors, sizes and plot character
png("figure/dc-01-case_ctrl.png")
preston_map(
  cols = c("black", "red"), 
  cex = c(0.5, 1), 
  pch = c(19, 19)
)
dev.off()

# explore data ------------------
# preston_crime has been pre-defined
preston_crime

# split data ------------------
# Use the split function to show the two point patterns
crime_splits <- split(preston_crime)

# Plot the split crime
png("figure/dc-02-case_ctrl-split.png")
plot(crime_splits)
dev.off()

# calculate density ------------------
# Compute the densities of both sets of points
crime_densities <- density(crime_splits)

crime_densities
png("figure/dc-03-case_ctrl-split_density.png")
plot(crime_densities)
dev.off()
str(crime_densities)

# fractional densities ------------------

#HOTSPOT: CASE RATIO DENSITY
# Calc the violent density divided by the sum of both
frac_violent_crime_density <- crime_densities[[2]] / 
  (crime_densities[[1]] + crime_densities[[2]])
# Plot the density of the fraction of violent crime
png("figure/dc-04-case_ctrl-frac_density.png")
plot(frac_violent_crime_density)
dev.off()

# SPATIAL SEGREGATION ------------------------

#get the violent crime ratio using spatial segregation model

library(spatialkernel)

# bandwidth selection ---------
# seek for the bandwidth that maximizes the test statistic
# use a cross-validation method

# Scan from 500m to 1000m in steps of 50m
bw_choice <- spseg(
    preston_crime, 
    h = seq(500, 1000, by = 50),
    opt = 1)

# Plot the results and highlight the best bandwidth
plotcv(bw_choice)
abline(v = bw_choice$hcv, lty = 2, col = "red")

# Print the best bandwidth
print(bw_choice$hcv)

# segregation probabilities --------------------------

#compute the probabilities for violent and non-violent crimes as a smooth surface
#as well as the p-values for a point-wise test of segregation

# Set the correct bandwidth and run for 10 simulations only
seg10 <- spseg(
  pts = preston_crime, 
  h = bw_choice$hcv,
  opt = 3,
  ntest = 10, 
  proc = TRUE)
# Plot the segregation map for violent crime
plotmc(seg10, "Violent crime")

seg <- seg10

seg <- spseg(pts = preston_crime, h = bw_choice$hcv, opt = 3,
             ntest = 1000, proc = TRUE)
# Plot seg, the result of running 1000 simulations
png("figure/dc-05-case-segregation_map-mc_prob_pval.png")
plotmc(seg, "Violent crime")
dev.off()

#seg <- spseg(pts = preston_crime, h = bw_choice$hcv, opt = 3,
#             ntest = 1000, proc = TRUE)
## Plot seg, the result of running 1000 simulations
#plotmc(seg, "Violent crime")

#graficos equivalentes a cotas en un mapa de altitud


# mapping segregation -----------------------------------------------------

# With a base map and some image and contour functions 
# we can display both the probabilities and the 
# significance tests over the area with more control 
# than the plotmc() function.
# 
# The seg object is a list with several components. The 
# X and Y coordinates of the grid are stored in the 
# $gridx and $gridy elements. The probabilities of each 
# class of data (violent or non-violent crime) are in a 
# matrix element $p with a column for each class. The 
# p-value of the significance test is in a similar 
# matrix element called $stpvalue. Rearranging columns 
# of these matrices into a grid of values can be done 
# with R's matrix() function. From there you can 
# construct list objects with a vector $x of 
# X-coordinates, $y of Y-coordinates, and $z as the 
# matrix. You can then feed this to image() or 
# contour() for visualization.


# Inspect the structure of the spatial segregation object
str(seg)

# Get the number of columns in the data so we can rearrange to a grid
ncol <- length(seg$gridx)

# Rearrange the probability column into a grid
# OJO: PROBABILITY DENSITY
prob_violent <- list(x = seg$gridx,
                     y = seg$gridy,
                     z = matrix(seg$p[, "Violent crime"],
                                ncol = ncol))
image(prob_violent)

# Rearrange the p-values, but choose a p-value threshold
# OJO: P-VALUE FROM THE MONTECARLO TEST
p_value <- list(x = seg$gridx,
                y = seg$gridy,
                z = matrix(seg$stpvalue[, "Violent crime"] < 0.05,
                           ncol = ncol))
image(p_value)

#https://gis.stackexchange.com/questions/23841/create-a-raster-with-georeferenced-information-in-r
rast <- raster::raster(p_value)
raster::projection(rast) <- CRS("+proj=longlat +datum=WGS84")
plot(rast)


#Call the segmap() function shown in the script to find areas 
#where the probability of a crime being violent is above 0.15. 
#Use 0.05 as the lower probability.
# Create a mapping function
segmap <- function(prob_list, pv_list, low, high){
  
  # background map
  plotRGB(preston_osm)
  
  # p-value areas
  image(pv_list, 
        col = c("#00000000", "#FF808080"), add = TRUE) 
  
  # probability contours
  contour(prob_list,
          levels = c(low, high),
          col = c("#206020", "red"),
          labels = c("Low", "High"),
          add = TRUE)
  
  # boundary window
  plot(Window(preston_crime), add = TRUE)
}

# Map the probability and p-value
png("figure/dc-06-case-probability_map-signif.png")
segmap(prob_violent, p_value, 0.05, 0.15)
dev.off()

# extra step: to dataset -----
#https://gis.stackexchange.com/questions/23841/create-a-raster-with-georeferenced-information-in-r
seg$stpvalue %>% as_tibble() %>% rownames_to_column() %>% 
  dplyr::select(rowname,case.prob.pval="Violent crime") %>% 
  left_join(seg$p %>% as_tibble() %>% rownames_to_column() %>% 
              dplyr::select(rowname,case.prob="Violent crime")) %>% 
  filter(case.prob.pval<=0.001) -> prob.threshold
  #ggplot() + 
  #geom_histogram(aes(case.prob.pval), bins = 100)
  #geom_histogram(aes(case.prob), bins = 100)
  #geom_point(aes(case.prob,case.prob.pval))
prob_violent_pvalue <- list(x = seg$gridx,
                     y = seg$gridy,
                     z = matrix(seg$p[, "Violent crime"] >= min(prob.threshold$case.prob), #corregir user-defined
                                ncol = ncol))
image(prob_violent_pvalue)
library(sf)
gpsrino <- preston_crime %>% as.data.frame() %>% as_tibble()
house <- st_as_sf(gpsrino, coords = c("x", "y"), remove = F,
                  crs = 27561, agr = "constant") #27561
#list of matrix coordinate and dicotomous z value
#convert to raster
rast <- raster::raster(prob_violent_pvalue)
raster::projection(rast) <- CRS("+init=epsg:27561") #"+init=epsg:27561"
plot(rast)
class(rast)
summary(rast)
#convert raster to polygon
#http://taromieno.netlify.com/post/raster_to_polygons_2018/
#spdf <- sp::as(rast,'SpatialPolygonsDataFrame')
spdf <- inlmisc::Grid2Polygons(rast)
#convert polygon to sf
sf_polygon <- st_as_sf(spdf)
#plot geometric points and polygon
ggplot() +
  geom_point(aes(x,y),data = house %>% filter(marks=="Violent crime"), color="red", size=1.5) +
  geom_point(aes(x,y),data = house %>% filter(marks!="Violent crime"), color="black",size=0.5, alpha=0.5) +
  geom_sf(data = sf_polygon,alpha=0) +
  coord_sf()
ggsave("figure/dc-06-case_prob_pval_map-signif.png",height = 10,width = 10)
  #geom_sf(data = house)
#tm_shape(sf_polygon) + tm_borders()
#intersection of point in polygon
house_inter <- st_intersects(house,sf_polygon,sparse = F) %>% 
  as_tibble() %>% #count(V1)
  rownames_to_column("id_st") %>% 
  rename("in_poly"="V2") %>% 
  dplyr::select(-V1)
#create final data set with new covariate
house_final <- house %>% 
  rownames_to_column("id_st") %>% 
  left_join(house_inter)
house_final %>% count(in_poly)
#merge with original ids - no arrage of observations discarted
final_db <- gpsrino %>% 
  #filter(community_1== base::levels(.$community_1)[num_community] ) %>% 
  #select(id,prev_viv,latitud,longitud) %>% 
  rownames_to_column("id_st") %>% 
  left_join(house_final)
#final_db %>% write_rds(paste0("data/zg-pcr_v-",name_community,".rds"))

# summary table ----------------------------------------------------------

final_db %>% 
  count(in_poly,marks)

prob.threshold_summary <- prob.threshold %>% 
  summarise_at(.vars = vars(case.prob,case.prob.pval),
               .funs = list(~min,~median,~max)) %>% 
  gather(key,value) %>% 
  separate(col = key,into = c("key","stat"),sep = "_") %>% 
  spread(stat,value) %>% 
  dplyr::select(key,min,median,max)

final_db_all <- final_db %>% 
  count(marks) %>% 
  #group_by(in_poly) %>% 
  mutate(den_inpoly = sum(n),
         prc_inpoly = 100*n/sum(n)) %>% 
  filter(marks=="Violent crime") %>% 
  dplyr::select(-marks) %>% 
  dplyr::select(den_inpoly,n,prc_inpoly) %>% 
  #rename_all(list(~str_replace_all(.,"(.+)","A\\_\\1"))) %>% 
  #rownames_to_column()
  #ungroup()
  mutate(in_poly="ALL")

final_db_sum <- final_db %>% 
  count(in_poly,marks) %>% 
  group_by(in_poly) %>% 
  mutate(den_inpoly = sum(n),
         prc_inpoly = 100*n/sum(n)) %>% 
  ungroup() %>% 
  mutate(in_poly=as.character(in_poly)) %>% 
  filter(marks=="Violent crime") %>% 
  dplyr::select(-marks) %>% 
  #gather(key,value,-in_poly) %>% 
  #mutate(in_poly = str_replace(in_poly,"(.).+","\\1"),
  #       in_poly= str_c(in_poly,"_",key)) %>% dplyr::select(-key) %>% 
  #spread(in_poly,value) %>% 
  #rownames_to_column() %>% 
  bind_rows(final_db_all) %>% 
  arrange(in_poly) %>% 
  rename(observed="n",number_loc="den_inpoly",prc_cases="prc_inpoly")

bind_rows(final_db_sum,prob.threshold_summary) %>% 
  as.data.frame() %>% 
  xlsx::write.xlsx("table/dc-06-case_prob_pval_map-signif.xlsx",showNA = F)






# _HUMBERSIDE DATA ----------------------------------------------------------

# Load the spatstat package
library(tidyverse)
library(spatstat)
theme_set(theme_bw())

preston_crime <- humberside #read_rds("data-dc/pcrime-spatstat.rds.gz.rds")
class(preston_crime) #ppp
preston_crime
# Get some summary information on the dataset
summary(preston_crime)

#library(raster)
#preston_osm <- read_rds("data-dc/osm_preston_gray.rds.gz.rds")
#class(preston_osm) #raster
#preston_osm
#str(preston_osm)
#summary(preston_osm)

# Get a table of marks
table(marks(preston_crime))

# Define a function to create a map
#preston_map <- function(cols = c("green","red"), cex = c(1, 1), pch = c(1, 1)) {
#  plotRGB(preston_osm) # from the raster package
#  plot(preston_crime, cols = cols, pch = pch, cex = cex, add = TRUE, show.window = TRUE)
#}

# Draw the map with colors, sizes and plot character
#png("figure/dc-01-case_ctrl.png")
#preston_map(
#  cols = c("black", "red"), 
#  cex = c(0.5, 1), 
#  pch = c(19, 19)
#)
#dev.off()

# explore data ------------------
# preston_crime has been pre-defined
preston_crime

# split data ------------------
# Use the split function to show the two point patterns
crime_splits <- split(preston_crime)

# Plot the split crime
png("figure/nh-02-case_ctrl-split.png")
plot(crime_splits)
dev.off()

# calculate density ------------------
# Compute the densities of both sets of points
crime_densities <- density(crime_splits)

crime_densities
png("figure/nh-03-case_ctrl-split_density.png")
plot(crime_densities)
dev.off()
str(crime_densities)

# fractional densities ------------------

#HOTSPOT: CASE RATIO DENSITY
# Calc the violent density divided by the sum of both
frac_violent_crime_density <- crime_densities[[1]] / 
  (crime_densities[[1]] + crime_densities[[2]])
# Plot the density of the fraction of violent crime
png("figure/nh-04-case_ctrl-frac_density.png")
plot(frac_violent_crime_density)
dev.off()

# SPATIAL SEGREGATION ------------------------

#get the violent crime ratio using spatial segregation model

library(spatialkernel)

# bandwidth selection ---------
# seek for the bandwidth that maximizes the test statistic
# use a cross-validation method

# Scan from 500m to 1000m in steps of 50m
bw_choice <- spseg(
  preston_crime, 
  h = seq(500, 5000, by = 50),
  opt = 1)

# Plot the results and highlight the best bandwidth
plotcv(bw_choice)
abline(v = bw_choice$hcv, lty = 2, col = "red")

# Print the best bandwidth
print(bw_choice$hcv)

# segregation probabilities --------------------------

#compute the probabilities for violent and non-violent crimes as a smooth surface
#as well as the p-values for a point-wise test of segregation

# Set the correct bandwidth and run for 10 simulations only
seg10 <- spseg(
  pts = preston_crime, 
  h = bw_choice$hcv,
  opt = 3,
  ntest = 10, 
  proc = TRUE)
# Plot the segregation map for violent crime
plotmc(seg10, "case")

seg <- seg10

seg <- spseg(pts = preston_crime, h = bw_choice$hcv, opt = 3,
             ntest = 1000, proc = TRUE)
# Plot seg, the result of running 1000 simulations
png("figure/nh-05-case-segregation_map-mc_prob_pval.png")
plotmc(seg, "case")
dev.off()

#seg <- spseg(pts = preston_crime, h = bw_choice$hcv, opt = 3,
#             ntest = 1000, proc = TRUE)
## Plot seg, the result of running 1000 simulations
#plotmc(seg, "Violent crime")

#graficos equivalentes a cotas en un mapa de altitud


# mapping segregation -----------------------------------------------------

# With a base map and some image and contour functions 
# we can display both the probabilities and the 
# significance tests over the area with more control 
# than the plotmc() function.
# 
# The seg object is a list with several components. The 
# X and Y coordinates of the grid are stored in the 
# $gridx and $gridy elements. The probabilities of each 
# class of data (violent or non-violent crime) are in a 
# matrix element $p with a column for each class. The 
# p-value of the significance test is in a similar 
# matrix element called $stpvalue. Rearranging columns 
# of these matrices into a grid of values can be done 
# with R's matrix() function. From there you can 
# construct list objects with a vector $x of 
# X-coordinates, $y of Y-coordinates, and $z as the 
# matrix. You can then feed this to image() or 
# contour() for visualization.


# Inspect the structure of the spatial segregation object
str(seg)

# Get the number of columns in the data so we can rearrange to a grid
ncol <- length(seg$gridx)

# Rearrange the probability column into a grid
# OJO: PROBABILITY DENSITY
prob_violent <- list(x = seg$gridx,
                     y = seg$gridy,
                     z = matrix(seg$p[, "case"],
                                ncol = ncol))
image(prob_violent)

# Rearrange the p-values, but choose a p-value threshold
# OJO: P-VALUE FROM THE MONTECARLO TEST
p_value <- list(x = seg$gridx,
                y = seg$gridy,
                z = matrix(seg$stpvalue[, "case"] < 0.05,
                           ncol = ncol))
image(p_value)

#https://gis.stackexchange.com/questions/23841/create-a-raster-with-georeferenced-information-in-r
rast <- raster::raster(p_value)
raster::projection(rast) <- CRS("+proj=longlat +datum=WGS84")
plot(rast)


#Call the segmap() function shown in the script to find areas 
#where the probability of a crime being violent is above 0.15. 
#Use 0.05 as the lower probability.
# Create a mapping function
segmap <- function(prob_list, pv_list, low, high){
  
  # background map
  plotRGB(preston_osm)
  
  # p-value areas
  image(pv_list, 
        col = c("#00000000", "#FF808080"), add = TRUE) 
  
  # probability contours
  contour(prob_list,
          levels = c(low, high),
          col = c("#206020", "red"),
          labels = c("Low", "High"),
          add = TRUE)
  
  # boundary window
  plot(Window(preston_crime), add = TRUE)
}

# Map the probability and p-value
png("figure/dc-06-case-probability_map-signif.png")
segmap(prob_violent, p_value, 0.05, 0.15)
dev.off()

# extra step: to dataset -----
#https://gis.stackexchange.com/questions/23841/create-a-raster-with-georeferenced-information-in-r
seg$stpvalue %>% as_tibble() %>% rownames_to_column() %>% 
  dplyr::select(rowname,case.prob.pval= "case") %>% 
  left_join(seg$p %>% as_tibble() %>% rownames_to_column() %>% 
              dplyr::select(rowname,case.prob="case")) %>% 
  filter(case.prob.pval<=0.001) -> prob.threshold
#ggplot() + 
#geom_histogram(aes(case.prob.pval), bins = 100)
#geom_histogram(aes(case.prob), bins = 100)
#geom_point(aes(case.prob,case.prob.pval))
prob_violent_pvalue <- list(x = seg$gridx,
                            y = seg$gridy,
                            z = matrix(seg$p[, "case"] >= min(prob.threshold$case.prob), #corregir user-defined
                                       ncol = ncol))
image(prob_violent_pvalue)
library(sf)
gpsrino <- preston_crime %>% as.data.frame() %>% as_tibble()
house <- st_as_sf(gpsrino, coords = c("x", "y"), remove = F,
                  crs = 27561, agr = "constant") #27561
#list of matrix coordinate and dicotomous z value
#convert to raster
rast <- raster::raster(prob_violent_pvalue)
raster::projection(rast) <- CRS("+init=epsg:27561") #"+init=epsg:27561"
plot(rast)
class(rast)
summary(rast)
#convert raster to polygon
#http://taromieno.netlify.com/post/raster_to_polygons_2018/
#spdf <- sp::as(rast,'SpatialPolygonsDataFrame')
spdf <- inlmisc::Grid2Polygons(rast)
#convert polygon to sf
sf_polygon <- st_as_sf(spdf)
#plot geometric points and polygon
ggplot() +
  geom_point(aes(x,y),data = house %>% filter(marks=="case"), color="red", size=1.5) +
  geom_point(aes(x,y),data = house %>% filter(marks!="case"), color="black",size=0.5, alpha=0.5) +
  #geom_sf(data = sf_polygon,alpha=0) +
  coord_sf()
ggsave("figure/nh-06-case_prob_pval_map-signif.png",height = 10,width = 10)
#geom_sf(data = house)
#tm_shape(sf_polygon) + tm_borders()
#intersection of point in polygon
house_inter <- st_intersects(house,sf_polygon,sparse = F) %>% 
  as_tibble() %>% #count(V1)
  rownames_to_column("id_st") %>% 
  rename("in_poly"="V2") %>% 
  dplyr::select(-V1)
#create final data set with new covariate
house_final <- house %>% 
  rownames_to_column("id_st") %>% 
  left_join(house_inter)
house_final %>% count(in_poly)
#merge with original ids - no arrage of observations discarted
final_db <- gpsrino %>% 
  #filter(community_1== base::levels(.$community_1)[num_community] ) %>% 
  #select(id,prev_viv,latitud,longitud) %>% 
  rownames_to_column("id_st") %>% 
  left_join(house_final)
#final_db %>% write_rds(paste0("data/zg-pcr_v-",name_community,".rds"))

# summary table ----------------------------------------------------------

gpsrino %>% count(marks) %>% 
  mutate(den = sum(n),
         prc = 100*n/sum(n))

final_db %>% 
  count(in_poly,marks)

prob.threshold_summary <- prob.threshold %>% 
  summarise_at(.vars = vars(case.prob,case.prob.pval),
               .funs = list(~min,~median,~max)) %>% 
  gather(key,value) %>% 
  separate(col = key,into = c("key","stat"),sep = "_") %>% 
  spread(stat,value) %>% 
  dplyr::select(key,min,median,max)

final_db_all <- final_db %>% 
  count(marks) %>% 
  #group_by(in_poly) %>% 
  mutate(den_inpoly = sum(n),
         prc_inpoly = 100*n/sum(n)) %>% 
  filter(marks=="Violent crime") %>% 
  dplyr::select(-marks) %>% 
  dplyr::select(den_inpoly,n,prc_inpoly) %>% 
  #rename_all(list(~str_replace_all(.,"(.+)","A\\_\\1"))) %>% 
  #rownames_to_column()
  #ungroup()
  mutate(in_poly="ALL")

final_db_sum <- final_db %>% 
  count(in_poly,marks) %>% 
  group_by(in_poly) %>% 
  mutate(den_inpoly = sum(n),
         prc_inpoly = 100*n/sum(n)) %>% 
  ungroup() %>% 
  mutate(in_poly=as.character(in_poly)) %>% 
  filter(marks=="Violent crime") %>% 
  dplyr::select(-marks) %>% 
  #gather(key,value,-in_poly) %>% 
  #mutate(in_poly = str_replace(in_poly,"(.).+","\\1"),
  #       in_poly= str_c(in_poly,"_",key)) %>% dplyr::select(-key) %>% 
  #spread(in_poly,value) %>% 
  #rownames_to_column() %>% 
  bind_rows(final_db_all) %>% 
  arrange(in_poly) %>% 
  rename(observed="n",number_loc="den_inpoly",prc_cases="prc_inpoly")

bind_rows(final_db_sum,prob.threshold_summary) %>% 
  as.data.frame() %>% 
  xlsx::write.xlsx("table/dc-06-case_prob_pval_map-signif.xlsx",showNA = F)







# _EMERGE DATA -------------------------------------------------------------

# Load the spatstat package
library(tidyverse)
library(spatstat)
library(haven)
theme_set(theme_bw())

gpsrino_p <- read_dta("../../R_/elixr/data/z0_ind_viv_t3.dta") %>% 
  as_factor() 

base::levels(gpsrino_p$community_1)

zg_poly <- st_read("data-raw/qgis/pol-ZG.shp") #%>% st_transform(4326)
pa_poly <- st_read("data-raw/qgis/pol-PA.shp")
nn_poly <- st_read("data-raw/qgis/pol-NN.shp")
ll_poly <- st_read("data-raw/qgis/pol-LL.shp")

num_community <- 4
name_community <- base::levels(gpsrino_p$community_1)[num_community]

gpsrino <- gpsrino_p %>%
  filter(community_1== base::levels(.$community_1)[num_community] ) %>% 
  select(#id,vivienda,
         latitud,longitud,
         #starts_with("micr_"),
         starts_with("prev_"),
         #starts_with("sero_"),
         -ends_with("_mix"),-ends_with("_fal")
         )

#gpsrino %>% glimpse()

library(sf)
house <- st_as_sf(gpsrino, coords = c("longitud", "latitud"), remove = T,
                  crs = 4326, agr = "constant")
#house %>% glimpse()

poly_list <- list(ll_poly,nn_poly,pa_poly,zg_poly)

house_poly <- poly_list[[num_community]]

house_bound <- house %>% 
  st_buffer(dist = 0.0005) %>% 
  st_union()

ggplot() +
  geom_sf(data=house_poly) +
  #geom_sf(data=house_bound) +
  geom_sf(data=house)
ggsave(paste0("figure/zg-ggmap-pcr_v-",name_community,".png"),height = 10, width = 10)

library(tmap)
mp <- tm_shape(house_bound) +
  tm_fill() +
  tm_shape(house) + 
  tm_dots() +
  tm_scale_bar(position = c("right", "bottom"))
tmap_save(mp,paste0("figure/zg-tmap-pcr_v-",name_community,".png"),height = 10, width = 10)

# transform sf to ppp -------------
#fuente: https://mgimond.github.io/Spatial/reading-and-writing-spatial-data-in-r.html
#maaptools package is required
library(maptools)
p.sp  <- as(house, "Spatial")  # Create Spatial* object
p.ppp <- as(p.sp, "ppp")      # Create ppp object
class(p.sp)
class(p.ppp)
house
p.sp
p.ppp
summary(p.ppp)
# add window -------
#str(p.ppp)
#str(house_poly)
#str(as(house_poly, "Spatial"))
#class(house_poly)
Window(p.ppp) <- as.owin(as(house_poly, "Spatial"))
summary(p.ppp)
# split, explore, density, fractional ---------------
preston_crime <- p.ppp
crime_splits <- split(preston_crime)
plot(crime_splits)
crime_densities <- density(crime_splits)
crime_densities
plot(crime_densities)
frac_violent_crime_density <- crime_densities[[2]] / 
  (crime_densities[[1]] + crime_densities[[2]])

png(paste0("figure/zg-frac_den-pcr_v-ML-",name_community,".png"),width = 600,height = 600)
plot(frac_violent_crime_density)
dev.off()

# bandwidth selection ---------
library(spatialkernel)
# Scan from 20m to 500m in steps of 5m
bw_choice <- spseg(
  preston_crime, 
  h = seq(2, 500, by = 5),#ll:25;nn:20;pa:2;zg:15
  opt = 1)
plotcv(bw_choice)
abline(v = bw_choice$hcv, lty = 2, col = "red")
print(bw_choice$hcv)

# segregation probabilities --------------------------
seg <- spseg(pts = preston_crime, h = bw_choice$hcv, opt = 3,
             ntest = 1000, proc = TRUE)
png(paste0("figure/zg-segprob-pcr_v-",name_community,".png"))
plotmc(seg,"positive")
dev.off()

str(seg)
ncol <- length(seg$gridx)
# OJO: PROBABILITY DENSITY
prob_violent <- list(x = seg$gridx,
                     y = seg$gridy,
                     z = matrix(seg$p[, "positive"],
                                ncol = ncol))
image(prob_violent)
# OJO: P-VALUE FROM THE MONTECARLO TEST
p_value <- list(x = seg$gridx,
                y = seg$gridy,
                z = matrix(seg$stpvalue[, "positive"] < 0.05,
                           ncol = ncol))
image(p_value)

# Create a mapping function
segmap <- function(prob_list, pv_list, low, high){
  
  # background map
  #plotRGB(preston_osm)
  plot(p.ppp,name_community)
  
  # p-value areas
  image(pv_list, 
        col = c("#00000000", "#FF808080"), add = TRUE) 
  
  # probability contours
  contour(prob_list,
          levels = c(low, high),
          col = c("#206020", "red"),
          labels = c("Low", "High"),
          add = TRUE)
  
  # boundary window
  #plot(Window(preston_crime), add = TRUE)
}
# save plot
png(paste0("figure/zg-pcr_v-",name_community,".png"))
segmap(prob_violent, p_value, 0.05, 0.15)
dev.off()

#plot(house)
#plot(p.sp)
#plot(p.ppp)

# extra step: to dataset -----
#https://gis.stackexchange.com/questions/23841/create-a-raster-with-georeferenced-information-in-r
#list of matrix coordinate and dicotomous z value
#convert to raster
rast <- raster::raster(p_value)
raster::projection(rast) <- CRS("+proj=longlat +datum=WGS84")
plot(rast)
class(rast)
summary(rast)
#convert raster to polygon
#http://taromieno.netlify.com/post/raster_to_polygons_2018/
#spdf <- sp::as(rast,'SpatialPolygonsDataFrame')
spdf <- inlmisc::Grid2Polygons(rast)
#convert polygon to sf
sf_polygon <- st_as_sf(spdf)
#plot geometric points and polygon
ggplot() +
  geom_sf(data = sf_polygon) +
  geom_sf(data = house)
#tm_shape(sf_polygon) + tm_borders()
#intersection of point in polygon
house_inter <- st_intersects(house,sf_polygon,sparse = F) %>% 
  as_tibble() %>% #count(V1)
  rownames_to_column("id_st") %>% 
  rename("in_poly"="V2") %>% 
  select(-V1)
#create final data set with new covariate
house_final <- house %>% 
  rownames_to_column("id_st") %>% 
  left_join(house_inter)
house_final %>% count(in_poly)
#merge with original ids - no arrage of observations discarted
final_db <- gpsrino_p %>% 
  filter(community_1== base::levels(.$community_1)[num_community] ) %>% 
  select(id,prev_viv,latitud,longitud) %>% 
  rownames_to_column("id_st") %>% 
  left_join(house_final)
final_db %>% write_rds(paste0("data/zg-pcr_v-",name_community,".rds"))

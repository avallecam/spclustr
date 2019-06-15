
# ggmap -------------------------------------------------------------------

#basado en: https://github.com/dkahle/ggmap
#complicaciones:
#Failed to connect to tile.stamen.com


# librerias ---------------------------------------------------------------

library(tidyverse)
library(ggmap)


# credentials -------------------------------------------------------------

#limitante: para usar google maps, se debe de crear una cuenta
#alternativa: usar openstreetmaps

register_google(key = "AIzaSyAvGB5P3z9Y7ATJa1GUzYaYNwX7SaM0Vk4")
ggmap_credentials()


# githubpage --------------------------------------------------------------

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap() 

# start -------------------------------------------------------------------

violent_crimes <- crime %>% 
  # only violent crimes
  filter(offense != "auto theft", 
         offense != "theft", 
         offense != "burglary") %>% 
  # rank violent crimes
  mutate(offense=as.factor(offense),
         offense=forcats::fct_drop(offense)) %>% #summary()
  # restrict to downtown
  filter(-95.39681 <= lon & lon <= -95.34188,
         29.73631 <= lat & lat <=  29.78400)


qmplot(lon, lat, 
       data = violent_crimes, 
       maptype = "toner-lite", 
       #check the effect of I() function
       color = I("red"),
       extent = "panel")

qmplot(lon, lat, 
       data = violent_crimes, 
       maptype = "toner-lite", 
       #check the effect of I() function
       color = I("red"),
       extent = "panel",
       geom = "density2d")

qmplot(lon, lat, 
       data = violent_crimes, 
       maptype = "toner-lite", 
       #check the effect of I() function
       color = offense,
       extent = "panel"#,
       #geom = "density2d"
) +
  facet_wrap(~offense)

qmplot(lon, lat, 
       data = violent_crimes, 
       maptype = "toner-lite", 
       #check the effect of I() function
       color = offense,
       extent = "panel",
       darken = .4
       #geom = "density2d"
) +
  facet_wrap(~month)


### Heatmap ------------------

robberies <- violent_crimes %>% 
  filter(offense == "robbery")

qmplot(lon, lat, 
       data = violent_crimes, 
       geom = "blank", zoom = 15, 
       maptype = "toner-background", darken = .7, 
       legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), 
                  geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Robbery\nPropensity", 
                       low = "white", 
                       mid = "yellow", 
                       high = "red", 
                       midpoint = 650)


### Clustering ------------------
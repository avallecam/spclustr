library(scanstatistics)
library(tidyverse)
library(magrittr)
library(sp)
library(tsibble)
#pedestrian %>% tsibble::has_gaps()

# parameters --------------------------------------------------------------

complete_time_range <- 1973:1991
cluster_time_range <- 1986:1989
baseline_time_range <- complete_time_range %>% 
  enframe(name = NULL) %>% 
  filter(!is_in(value,min(cluster_time_range):max(complete_time_range))) %>% 
  pull(value)
baseline_last <- max(baseline_time_range)
k_predefined <- 15 #controvertial decision
k_top_score_clusters <- 5

# map ---------------------------------------------------------------------

# Load map data
data(NM_map)
data(NM_geo)

NM_map %>% as_tibble()
NM_geo %>% as_tibble()

NM_map %>% as_tibble() %>% count(county)
NM_geo %>% as_tibble() %>% count(county) 

# Plot map with labels at centroids
ggplot() + 
  geom_polygon(data = NM_map,
               mapping = aes(x = long, y = lat, group = group),
               color = "grey", fill = "white") +
  geom_text(data = NM_geo, 
            mapping = aes(x = center_long, y = center_lat, label = county)) +
  ggtitle("Counties of New Mexico")

# cases -------------------------------------------------------------------

data(NM_popcas)
head(NM_popcas)

NM_popcas %>% as_tibble()
NM_popcas %>% as_tibble() %>% count(county) #32 counties
NM_popcas %>% count(year)

#we do have zero counts!
NM_popcas %>% as_tibble() %>% skimr::skim(count)

NM_popcas %>% 
  as_tibble() %>% 
  count(county,sort = T) %>% 
  count(n)

NM_popcas %>% 
  as_tibble() %>% 
  filter(count==0) %>% 
  count(county,sort = T)

NM_popcas %>% 
  as_tibble() %>% 
  as_tsibble(key = county,index = year) %>% #glimpse()
  naniar::replace_with_na(replace = list(count=0)) %>% 
  #I lost one complete county because it always had 0
  filter(!is.na(count)) %>% 
  #naniar::vis_miss()
  #has_gaps(.full = TRUE) %>% count(.gaps)
  #count_gaps(.full = TRUE) %>% arrange(desc(.n))
  group_by(county) %>% 
  fill_gaps(count=0L,population=mean(population),.full = TRUE) %>% 
  #naniar::vis_miss()
  as_tibble() %>% 
  count(county)

NM_popcas %>% 
  as_tibble() %>% 
  ggplot(aes(x = year,y = count)) +
  geom_line() +
  facet_wrap(~county)

NM_popcas %>% 
  as_tibble() %>% 
  mutate(rate=count/population*10^6) %>% 
  ggplot(aes(x = year,y = rate)) +
  geom_line() +
  facet_wrap(~county)

# problem -----------------------------------------------------------------

#' data from 1973-1991
#' detect clusters during the years 1986-1989
#' steps
#' (0)
#' define you department/county of interest
#' analysis will be don with aggregated district data
#' (1)
#' retrieve cases from period time of interest
#' trasform time-location-cases to matrix using df_to_matrix
#' (2)
#' zone, which is the name for the spatial component of a potential outbreak cluster. 
#' zone, consists of one or more locations grouped together according to their similarity across features
#' zone, uses the seat coordintates equivalent to the main ciy town in a county
#' zone, define a prestablished k (example = 15)
#' (3)
#' estimate a baseline of cases usign a regression model (simple interpolation)
#' then predict cases to obtain the expected cases per county of state throuout the years
#' ( )
#' use this 03 sources: cases, zone and expected -> run scan

# expectation based -------------------------------------------------------

# __ observed cases -------------------------------------------------------

counts <- NM_popcas %>% 
  as_tibble() %>% 
  filter(is_in(year,cluster_time_range)) %>% 
  #filter(year >= 1986 & year < 1990) %>%
  df_to_matrix(time_col = "year", 
               location_col = "county", 
               value_col = "count")
counts

# __ spatial zones --------------------------------------------------------

# Remove Cibola since cases have been counted towards Valencia. Ideally, this
# should be accounted for when creating the zones.
zones <- NM_geo %>%
  filter(county != "cibola") %>%
  select(seat_long, seat_lat) %>%
  as.matrix() %>%
  spDists(x = ., y = ., longlat = TRUE) %>%
  dist_to_knn(k = k_predefined) %>%
  knn_zones

# __ baselines ------------------------------------------------------------

# create a model to estimate expected population 
mod <- glm(count ~ offset(log(population)) + 1 + I(year - baseline_last),
           family = poisson(link = "log"),
           data = NM_popcas %>% filter(year < baseline_last+1))

mod %>% avallecam::epi_tidymodel_coef()
NM_popcas %>% filter(year < baseline_last+1) %>% as_tibble()
mod %>% broom::augment()

# make an augment but only for the years of interest
ebp_baselines <- NM_popcas %>% 
  filter(is_in(year,cluster_time_range)) %>% 
  #filter(year >= 1986 & year < 1990) %>%
  mutate(mu = predict(mod, newdata = ., type = "response")) %>%
  df_to_matrix(value_col = "mu")

#understant covariate expression used at regression
# y ~ a + b*x
# glm(y ~ I(x-x0)-1, offset=y0)
# the expression centers the coeficient
# thte additional +1 do not affect the coefficient estimate
# this is what we are modeling
NM_popcas %>% 
  filter(year < baseline_last+1) %>% #as_tibble() %>% 
  count(year) %>% 
  mutate(new=year-baseline_last+1)

# __ calculation ----------------------------------------------------------

set.seed(1)
poisson_result <- scan_eb_poisson(counts = counts, 
                                  zones = zones, 
                                  baselines = ebp_baselines,
                                  n_mcsim = 999)
print(poisson_result)
poisson_result %>% str()
poisson_result %>% attributes()
mlc_location <- poisson_result$MLC$locations
#poisson_result$MLC$relative_risk
# poisson_result$observed %>% as_tibble()
# poisson_result$replicates %>% as_tibble()

# __ results --------------------------------------------------------------

counties <- as.character(NM_geo$county)
counties[mlc_location]


# __ heuristic scores -----------------------------------------------------

NM_popcas %>% count(county)

# Calculate scores and add column with county names
county_scores <- score_locations(poisson_result, zones)
county_scores %<>% mutate(county = factor(counties[-length(counties)], 
                                          levels = levels(NM_geo$county)))

# Create a table for plotting
score_map_df <- merge(NM_map, county_scores, by = "county", all.x = TRUE) %>%
  arrange(group, order)

score_map_df %>% as_tibble()

# As noted before, Cibola county counts have been attributed to Valencia county
score_map_df[score_map_df$subregion == "cibola", ] %<>%
  mutate(relative_score = score_map_df %>% 
           filter(subregion == "valencia") %>% 
           select(relative_score) %>% 
           .[[1]] %>% .[1])

ggplot() + 
  geom_polygon(data = score_map_df,
               mapping = aes(x = long, y = lat, group = group, 
                             fill = relative_score),
               color = "grey") +
  scale_fill_viridis_c(option = "magma") +
  # scale_fill_gradient(low = "#e5f5f9", high = "darkgreen",
  #                     guide = guide_colorbar(title = "Relative\nScore")) +
  geom_label(data = NM_geo, 
            mapping = aes(x = center_long, y = center_lat, label = county),
            alpha = 0.5) +
  ggtitle("County scores")


# __ top scoring ----------------------------------------------------------

top5 <- top_clusters(poisson_result, 
                     zones, 
                     k = k_top_score_clusters, 
                     overlapping = FALSE)

top5

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

top5 %>% as_tibble() %>% 
  unnest(cols = counties) #%>% separate_rows()

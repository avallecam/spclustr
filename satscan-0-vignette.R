

# README -------------------------------------------------------------

# (1)
# EN LA FUNCIÓN satscan()
# EL ARGUMENTO sslocation = "" DEBE TENER LA RUTA 
# A LA UBICACIÓN DE LA CARPETA QUE CONTIENE 
# EL SOFTWARE EN VUESTROS COMPUTADORES mysatscanloc 
mysatscanloc <- "C:/Program Files (x86)/SaTScan"

# (2)
# Recomiendo ejecutar en conjunto,
# es decir, sobrear todo los comandos,
# las secciones de 
# write, invisible, ss.options, write.ss, satscan y summary
# al no hacerlo, incorrectamente sale error.

# (3)
# Accede a la lista de parámetros y los números de sus opciones
ss.options(reset=TRUE)

# paquetes ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(rsatscan)
theme_set(theme_bw()) #fondo blanco y negro del ggplot

#create temporal file ------------

td = tempdir()

# o -----------------------------------------------------------------------

# _VIGNETTE SatScan ----------------------------------------------------------------
# https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html
# video tutorial https://vimeo.com/123859199

# _DATA: NHumbersidegeo -----------------------------------

# _(point-pattern) (binomial/year) ---------------------------------------------------------

# _METHOD: bernoulli pure spatial -------------------------------------------

#explore class data frame ------------
class(NHumbersidegeo)
dim(NHumbersidegeo)
head(NHumbersidegeo)
head(NHumbersidecas)
head(NHumbersidectl)

#write temporal data files ------------
write.cas(NHumbersidecas, td, "NHumberside")
write.ctl(NHumbersidectl, td, "NHumberside")
write.geo(NHumbersidegeo, td, "NHumberside")

#reset SaTScan parameters ------------
invisible(ss.options(reset=TRUE))

#set SaTScan parameters ------------ 
ss.options(list(CaseFile="NHumberside.cas", ControlFile="NHumberside.ctl"))
ss.options(list(PrecisionCaseTimes=0, #none!
                StartDate="2001/11/1", EndDate="2001/11/24")) #range days!
ss.options(list(CoordinatesFile="NHumberside.geo", CoordinatesType=0, ModelType=1)) #bernoulli #lat/lon 1
#ss.options(list(CoordinatesFile="NHumberside.geo", CoordinatesType=0, ModelType=6)) #discrete poisson
ss.options(list(TimeAggregationUnits = 3, #day!
                NonCompactnessPenalty=0))
ss.options(list(ReportGiniClusters="n", LogRunToHistoryFile="n"))
ss.options(list(OutputShapefiles="y", OutputTemporalGraphHTML="y")) #outputfiles
ss.options(list(SpatialWindowShapeType=0)) #0 circullar, 1 elliptical

#write the SaTScan parameter file ------------
write.ss.prm(td, "NHumberside")

# run SaTScan --------
# ojo: sslocation tiene que ser seteado de acuerdo al computador local
NHumberside = satscan(td, "NHumberside", sslocation=mysatscanloc)

# analyze the results from SaTScan --------------
summary(NHumberside)
#str(NHumberside)
summary.default(NHumberside)
#NHumberside$main
NHumberside$col #summary of each cluster + radius + number of locations per cluster + LRT significance + RR
head(NHumberside$rr) #relative risk for each location
head(NHumberside$gis) #locations within clusters
head(NHumberside$llr) #likelihood ratio test ¿permutations?
NHumberside$sci #summary of each cluster + percantage of cases
sp::plot(NHumberside$shapeclust) #spatial polygon df #not available
#NHumberside$prm

#plot list of
#randomized likelihood ratios
#to calculate p-values
#by re-randomize data and find the 
#most likely posterior of all those
#only the most extreme are detected as clusters
hist(NHumberside$llr$LLR,main = "monte carlo",xlim = c(0,25))
abline(v=NHumberside$col[,c("LLR")],col="red")
#how far to the right, are more unusual

# visual interpretation ---------------------------------------------------

library(sf)

NHumberside_cluster <- NHumberside$col %>% 
  as_tibble() %>% 
  rename_all(list(~make.names(.))) %>% 
  rename_all(list(~str_to_lower(.))) %>% 
  mutate(prc_cases=100*(observed/population)) %>% 
  select(cluster,loc_id,x,y,
         radius,
         observed:rel_risk,number_loc,prc_cases,llr,p_value) %>% 
  st_as_sf(coords = c("x", "y"), remove = F,
           crs = 7801, agr = "constant") %>% 
  st_buffer(dist = .$radius)

NHumberside_df_cluster <- NHumberside$gis %>% 
  as_tibble() %>% 
  rename_all(list(~make.names(.))) %>% 
  rename_all(list(~str_to_lower(.))) %>% 
  select(loc_id,cluster,loc_x,loc_y)

NHumberside_df <- NHumbersidegeo %>% 
  as_tibble() %>% 
  left_join(NHumbersidecas) %>% 
  left_join(NHumbersidectl) %>% 
  #mutate(num_test=numcases!=numcontrols) %>% filter(num_test==F) #count(num_test)
  rename_all(list(~make.names(.))) %>% 
  rename(loc_id=locationid) %>% 
  mutate(loc_id=as.factor(loc_id),
         numcases=as.factor(numcases)) %>% 
  left_join(NHumberside_df_cluster) %>% 
  left_join(NHumberside_cluster) %>% 
  filter(loc_id!=69)

NHumberside_df %>% #glimpse()
  ggplot(aes(x.coordinate,y.coordinate#,
             #colour=as.factor(cluster)
  )) +
  geom_point(data = NHumberside_df %>% filter(numcases==1) ,colour="red",size=1.5) +
  geom_point(data = NHumberside_df %>% filter(numcases==0) ,colour="black",size=0.5, alpha=0.5) +
  geom_sf(data=NHumberside_cluster,aes(x,y,fill=as.factor(cluster)),alpha=0.2) +
  scale_fill_discrete("cluster") +
  #scale_color_grey() +
  #geom_point(aes(size=radius),colour="red",alpha=0.3) + #no real radius
  #geom_point(aes(size=as.integer(cluster)),colour="red",alpha=0.3) +
  coord_fixed(ratio = 1) +
  coord_sf() +
  labs(title = "no significant cluster")

ggsave("figure/uni-00-scan_cluster_map-signif.png",height = 10, width = 10)
xlsx::write.xlsx(NHumberside_cluster %>% as.data.frame() %>% select(-x,-y,-geometry),
                 "table/uni-00-scan_cluster_map-signif.xlsx")

# clean up temporal files -------------------------------------------------

file.remove(paste0(td,"/NHumberside.prm"))
file.remove(paste0(td,"/NHumberside.cas"))
file.remove(paste0(td,"/NHumberside.geo"))

# o -----------------------------------------------------------------------

# _SOURCE: DataCamp -----------------------------
# https://www.datacamp.com/courses/spatial-statistics-in-r
# https://assets.datacamp.com/production/repositories/748/datasets/6b32a67b58072c2c181daf2dae81d5944934fbea/pcrime-spatstat.rds

# _DATA violent crime ----------------------------------------------------

# _(point-pattern) (binomial/year) --------------------------------------------------------

# _METHOD: bernoulli pure spatial -------------------------------------------

# import out data ---------------------------------------------------------

library(spatstat)

preston_crime <- read_rds("data-dc/pcrime-spatstat.rds.gz.rds")
class(preston_crime) #ppp
preston_crime
# Get some summary information on the dataset
summary(preston_crime)

library(sf)

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

preston_crime_df <- ppp2sf(preston_crime)$data %>% 
  mutate(numcases=as.numeric(marks)-1,
         numcontrols=if_else(numcases==1,0,1)) %>%  #%>% count(numcontrols,numcases)
  rownames_to_column("locationid")

NHumbersidegeo <- preston_crime_df %>% 
  select(locationid,"x-coordinate"=x,"y-coordinate"=y) %>% 
  as.data.frame()

NHumbersidecas <- preston_crime_df %>% 
  select(locationid,numcases) %>% 
  as.data.frame()

NHumbersidectl <- preston_crime_df %>% 
  select(locationid,numcontrols) %>% 
  as.data.frame()

#explore class data frame ------------
class(NHumbersidegeo)
dim(NHumbersidegeo)
head(NHumbersidegeo)
head(NHumbersidecas)
head(NHumbersidectl)

#write temporal data files ------------
write.cas(NHumbersidecas, td, "NHumberside")
write.ctl(NHumbersidectl, td, "NHumberside")
write.geo(NHumbersidegeo, td, "NHumberside")

#reset SaTScan parameters ------------
invisible(ss.options(reset=TRUE))

#set SaTScan parameters ------------
ss.options(list(CaseFile="NHumberside.cas", ControlFile="NHumberside.ctl"))
ss.options(list(PrecisionCaseTimes=0, StartDate="2001/11/1", EndDate="2001/11/24"))
ss.options(list(CoordinatesFile="NHumberside.geo", CoordinatesType=0, ModelType=1)) #bernoulli
#ss.options(list(CoordinatesFile="NHumberside.geo", CoordinatesType=0, ModelType=6)) #discrete poisson
ss.options(list(TimeAggregationUnits = 3, NonCompactnessPenalty=0))
ss.options(list(ReportGiniClusters="n", LogRunToHistoryFile="n",OutputShapefiles="y"))
ss.options(list(SpatialWindowShapeType=0)) #0 circullar, 1 elliptical

#write the SaTScan parameter file ------------
write.ss.prm(td, "NHumberside")

# run SaTScan --------
# ojo: sslocation tiene que ser seteado de acuerdo al computador local
NHumberside = satscan(td, "NHumberside", sslocation=mysatscanloc)

# analyze the results from SaTScan --------------
summary(NHumberside)
#str(NHumberside)
summary.default(NHumberside)
#NHumberside$main
NHumberside$col #summary of each cluster + radius + number of locations per cluster + LRT significance + RR
head(NHumberside$rr) #relative risk for each location
head(NHumberside$gis) #locations within clusters
head(NHumberside$llr) #likelihood ratio test ¿permutations?
NHumberside$sci #summary of each cluster + percantage of cases
sp::plot(NHumberside$shapeclust) #spatial polygon df #not available
#NHumberside$prm

#plot list of
#randomized likelihood ratios
#to calculate p-values
#by re-randomize data and find the 
#most likely posterior of all those
#only the most extreme are detected as clusters
hist(NHumberside$llr$LLR,main = "monte carlo",xlim = c(0,25))
abline(v=NHumberside$col[,c("LLR")],col="red")
#how far to the right, are more unusual

# visual interpretation ---------------------------------------------------

NHumberside_cluster <- NHumberside$col %>% 
  as_tibble() %>% 
  rename_all(list(~make.names(.))) %>% 
  rename_all(list(~str_to_lower(.))) %>% 
  mutate(prc_cases=100*(observed/population)) %>% 
  select(cluster,loc_id,x,y,
         radius,
         observed:rel_risk,number_loc,prc_cases,llr,p_value) %>% 
  st_as_sf(coords = c("x", "y"), remove = F,
           crs = 7801, agr = "constant") %>% 
  st_buffer(dist = .$radius)

NHumberside_df_cluster <- NHumberside$gis %>% 
  as_tibble() %>% 
  rename_all(list(~make.names(.))) %>% 
  rename_all(list(~str_to_lower(.))) %>% 
  select(loc_id,cluster,loc_x,loc_y)

NHumberside_df <- NHumbersidegeo %>% 
  as_tibble() %>% 
  left_join(NHumbersidecas) %>% 
  left_join(NHumbersidectl) %>% 
  #mutate(num_test=numcases!=numcontrols) %>% filter(num_test==F) #count(num_test)
  rename_all(list(~make.names(.))) %>% 
  rename(loc_id=locationid) %>% 
  mutate(loc_id=as.factor(loc_id),
         numcases=as.factor(numcases)) %>% 
  left_join(NHumberside_df_cluster) %>% 
  left_join(NHumberside_cluster)

NHumberside_df %>% #glimpse()
  ggplot(aes(x.coordinate,y.coordinate#,
             #colour=as.factor(cluster)
  )) +
  geom_point(data = NHumberside_df %>% filter(numcases==1) ,colour="red",size=1.5) +
  geom_point(data = NHumberside_df %>% filter(numcases==0) ,colour="black",size=0.5, alpha=0.5) +
  geom_sf(data=NHumberside_cluster,aes(x,y,fill=as.factor(cluster)),alpha=0.2) +
  scale_fill_discrete("cluster") +
  #scale_color_grey() +
  #geom_point(aes(size=radius),colour="red",alpha=0.3) + #no real radius
  #geom_point(aes(size=as.integer(cluster)),colour="red",alpha=0.3) +
  coord_fixed(ratio = 1) +
  coord_sf() +
  labs(title = "cluster 1 p<0.001")

ggsave("figure/dc-06-scan_cluster_map-signif.png",height = 10, width = 10)
xlsx::write.xlsx(NHumberside_cluster %>% as.data.frame() %>% select(-x,-y,-geometry),
                 "table/dc-06-scan_cluster_map-signif.xlsx")

# clean up temporal files -------------------------------------------------

file.remove(paste0(td,"/NHumberside.prm"))
file.remove(paste0(td,"/NHumberside.cas"))
file.remove(paste0(td,"/NHumberside.geo"))

# o -----------------------------------------------------------------------

# _VIGNETTE SatScan ----------------------------------------------------------------
# https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html
# video tutorial https://vimeo.com/123859199

# _DATA: Brain Cancer cases NMex -----------------------------------

# _(areal data) (count/population/year/covariates) ------------------------------------------------------------

# _METHOD: poisson space-time --------------------------------------------------------------

# input files -------------------------------------------------------------

head(NMcas) #base nominal
head(NMgeo) #centroide de cada condado
head(NMpop) #base consolidada

#casos
#base por sujeto (año colecta, grupo de edad y sexo)
#eventos duplicados son registros distintos
NMcas %>% as_tibble() %>% clean_names() %>% arrange(year,county,agegroup)
#controles
#base agregada con denominador por estrato (quinquenio y sexo) y año
#tres años de datos
NMpop %>% as_tibble() %>% clean_names() %>% arrange(year,county,agegroup) %>% print(n=37)
NMpop %>% as_tibble() %>% clean_names() %>% count(county,year) # cada 9 años

write.cas(NMcas, td,"NM")
write.geo(NMgeo, td,"NM")
write.pop(NMpop, td,"NM")

# set parameters ----------------------------------------------------------

invisible(ss.options(reset=TRUE))
ss.options(list(CaseFile="NM.cas",StartDate="1973/1/1",EndDate="1991/12/31", 
                PopulationFile="NM.pop",
                CoordinatesFile="NM.geo", CoordinatesType=0, 
                #3=Retrospective Space-Time
                AnalysisType=3#,
                #0=Discrete Poisson
                #ModelType=0
))
ss.options(c("NonCompactnessPenalty=0", "ReportGiniClusters=n", "LogRunToHistoryFile=n"))

write.ss.prm(td,"testnm")
testnm = satscan(td,"testnm", sslocation=mysatscanloc)
summary(testnm)

# explore output ----------------------------------------------------------

summary.default(testnm)
#testnm$main
testnm$col #summary of each cluster + radius + number of locations per cluster + LRT significance + RR
head(testnm$rr) #relative risk for each location
head(testnm$gis) #locations within clusters
head(testnm$llr) #likelihood ratio test ¿permutations?
testnm$sci #summary of each cluster + percantage of cases
sp::plot(testnm$shapeclust) #spatial polygon df #not available
#testnm$prm

#plot list of
#randomized likelihood ratios
#to calculate p-values
#by re-randomize data and find the 
#most likely posterior of all those
#only the most extreme are detected as clusters
hist(testnm$llr$LLR,main = "monte carlo",xlim = c(0,25))
abline(v=testnm$col[,c("LLR")],col="red")
#how far to the right, are more unusual

file.remove(paste0(td,"/NM.pop"))
file.remove(paste0(td,"/NM.cas"))
file.remove(paste0(td,"/NM.geo"))

# o -----------------------------------------------------------------------

# _SOURCE SatScan manual ----------------------------------------------------------------
# https://www.satscan.org/tutorials.html
# https://www.satscan.org/tutorials/nyscancer/SaTScanTutorialNYSCancer.pdf
# https://www.satscan.org/datasets/nyscancer/NYS_Cancer.zip

# _DATA: Breast Cancer cases NYC -----------------------------------

# _(areal data) (count/expected/year) -----------------------------------------

# _METHOD: poisson discrete time ---------------------------------------------------

# import out data ---------------------------------------------------------

NYScas <- read.table("data-satscan/NYS_BreastCancer.cas",sep=" ",header=F)
NYSpop <- read.table("data-satscan/NYS_BreastCancer.pop",sep=" ",header=F)
NYSgeo <- read.table("data-satscan/Coordinates.geo",sep=" ",header=F)

head(NYScas)
head(NYSgeo)
head(NYSpop)

NYSpop %>% as_tibble() %>% count(V2)
NYScas %>% as_tibble() %>% count(V3)

write.cas(NYScas, td,"NYS")
write.geo(NYSgeo, td,"NYS")
write.pop(NYSpop, td,"NYS")

# set parameters ----------------------------------------------------------

invisible(ss.options(reset=TRUE))
ss.options(list(CaseFile="NYS.cas",StartDate="2009/1/1",EndDate="2009/12/31", 
                PopulationFile="NYS.pop",
                CoordinatesFile="NYS.geo", 
                CoordinatesType=1, 
                AnalysisType=1, ModelType=0
))
ss.options(c("NonCompactnessPenalty=0", "ReportGiniClusters=n", "LogRunToHistoryFile=n"))

write.ss.prm(td,"testnys")
testnys = satscan(td,"testnys", sslocation=mysatscanloc)
summary(testnys)

summary.default(testnys)


# explore output ----------------------------------------------------------


# o -----------------------------------------------------------------------

# _VIGNETTE SatScan ----------------------------------------------------------------
# https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html
# video tutorial https://vimeo.com/123859199

# _DATA: Fever cases NYC -----------------------------------

# _(areal data) (caso/fecha) ------------------------------------------------------------

# _METHOD: space-time permutation --------------------------------------------------------------

# input files -------------------------------------------------------------

head(NYCfevercas) #base nominal
head(NYCfevergeo) #centroide de cada condado
#head(NMpop) #base consolidada

#casos
# 1 por día, filas unicas
NYCfevercas %>% as_tibble() %>% skimr::skim()
NYCfevercas %>% as_tibble() %>% 
  mutate(date=lubridate::as_date(date)) %>% 
  summarise_at(.vars = vars(date),.funs = c("min","max"))

write.cas(NYCfevercas, td,"NM")
write.geo(NYCfevergeo, td,"NM")
#write.pop(NMpop, td,"NM")

# set parameters ----------------------------------------------------------

invisible(ss.options(reset=TRUE))
ss.options(list(CaseFile="NM.cas",
                StartDate="2001/11/1",EndDate="2001/11/24", 
                #PopulationFile="NM.pop",
                CoordinatesFile="NM.geo", 
                #latitud longitud
                CoordinatesType=1, 
                #4=Prospective Space-Time
                AnalysisType=4, 
                #2=Space-Time Permutation
                ModelType=2, 
                #3=Day
                TimeAggregationUnits=3,
                #time
                PrecisionCaseTimes=3, #3:day
                #MaxTemporalSizeInterpretation=1, #0:percentages, 1:time 
                #MaxTemporalSize=7, #maximum temporal cluster size (<=90%) ¿?
                #
                #UseDistanceFromCenterOption="y",
                #MaxSpatialSizeInDistanceFromCenter=3, 
                #NonCompactnessPenalty=0,
                #
                #ProspectiveStartDate="2001/11/24", 
                ReportGiniClusters="n", 
                LogRunToHistoryFile="n"
))
#ss.options(c("NonCompactnessPenalty=0", "ReportGiniClusters=n", "LogRunToHistoryFile=n"))

write.ss.prm(td,"testnm")
testnm = satscan(td,"testnm", sslocation=mysatscanloc)
summary(testnm)
summary.default(testnm)
#
testnm$col #summary of each cluster + radius + number of locations per cluster + LRT significance + RR
head(testnm$rr) #relative risk for each location
head(testnm$gis) #locations within clusters
head(testnm$llr) #likelihood ratio test ¿permutations?
testnm$sci #summary of each cluster + percantage of cases
sp::plot(testnm$shapeclust) #spatial polygon df #not available
#testnm$prm



# o -----------------------------------------------------------------------

# some key parameters -----------------------------------------------------

#[Analysis]  
#
#analysis type
#"AnalysisType=1"
# 	1=Purely Spatial
# 	2=Purely Temporal
# 	3=Retrospective Space-Time
# 	4=Prospective Space-Time
# 	5=Spatial Variation in Temporal Trends
# 	6=Prospective Purely Temporal   

#model type
#"ModelType=0"
# 	0=Discrete Poisson
# 	1=Bernoulli
# 	2=Space-Time Permutation
# 	3=Ordinal
# 	4=Exponential
# 	5=Normal
# 	6=Continuous Poisson
# 	7=Multinomial

#scan areas
#"ScanAreas=1"
# 	1=High Rates(Poison,Bernoulli,STP); High Values(Ordinal,Normal); Short Survival(Exponential)
# 	2=Low Rates(Poison,Bernoulli,STP); Low Values(Ordinal,Normal); Long Survival(Exponential)
# 	3=Both Areas

#time aggregation units
#"TimeAggregationUnits=1"
# 	0=None
# 	1=Year
# 	2=Month
# 	3=Day
# 	4=Generic
#
# time aggregation length
# TimeAggregationLength=1
#   Positive Integer

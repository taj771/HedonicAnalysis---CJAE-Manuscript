

rm(list=ls(all=TRUE))

# https://www.hydrosheds.org/page/hydrolakes

library(tidyverse)
library(rcanvec)
library(geosphere)
library(prettymapr)

library(raster)
library(sf)


df_water1 <- st_read(
  "F:/Data/canvec_50K_ON_Hydro_shp/canvec_50K_ON_Hydro/waterbody_2_1.shp")

df_water2 <- st_read(
  "F:/Data/canvec_50K_ON_Hydro_shp/canvec_50K_ON_Hydro/waterbody_2_2.shp")

df_water3 <- st_read(
  "F:/Data/canvec_50K_ON_Hydro_shp/canvec_50K_ON_Hydro/waterbody_2_3.shp")

df_water4 <- st_read(
  "F:/Data/canvec_50K_ON_Hydro_shp/canvec_50K_ON_Hydro/waterbody_2_4.shp")

df_water5 <- st_read(
  "F:/Data/canvec_50K_ON_Hydro_shp/canvec_50K_ON_Hydro/waterbody_2_5.shp")

df_water1 <- df_water1 %>%
  filter(definit_en == "Lake") %>%  
  st_transform(4326, crs(CRS(4326)))

df_water2 <- df_water2 %>%
  filter(definit_en == "Lake") %>%  
  st_transform(4326, crs(CRS(4326)))

df_water3 = df_water3 %>%
  bind_rows(df_water4, df_water5) %>%
  filter(definit_en == "Lake") %>%  
  st_transform(4326, crs(CRS(4326)))

rm(df_water4, df_water5)


df_water2 %>% st_bbox()

df_water= st_as_sf(df_water) %>%  
  st_transform(4326, crs(CRS(4326)))


xmin <- -83.540
xmax <- -73.608
ymin <- 41.591
ymax <- 46.498

filt_bbox <- sf::st_bbox(c(xmin = ifelse(is.na(xmin), -180, xmin), 
                           ymin = ifelse(is.na(ymin),  -90,  ymin), 
                           xmax = ifelse(is.na(xmax), +180, xmax), 
                           ymax = ifelse(is.na(ymax),  +90, ymax)), 
                         crs = st_crs(4326)) %>% 
  sf::st_as_sfc(.)


df_water11 <- df_water2 %>%
  sample_n(10)%>%  
  st_transform(4326, crs(CRS(4326)))

df_water11 %>% st_bbox()

find_data <- sf::st_intersects(df_water2, filt_bbox)
#> although coordinates are longitude/latitude, st_within assumes that they are planar
filt_data <- df_water2[which(lengths(find_data) != 0), ]

filt_data$area_sqkm <- as.numeric(st_area(filt_data))*1e-6

summary(filt_data$area_sqkm)
filt_data <- filt_data %>%
  filter(area_sqkm > 0.1)
summary(filt_data$area_sqkm)


lake_names = tibble(lake_name = filt_data$namelk1en,
                    area = filt_data$area_sqkm) %>%
  filter(!is.na(lake_name))




# all of muskoka
#altalake <- makebbox(46.498, -73.608, 41.591, -83.540)
#canvec.download(nts(bbox=altalake))
#canvec.export(nts(bbox=altalake), "canvec_data")
#altalake <- searchbbox("skeleton lake, ON")

#coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")

#canvec.qplot(bbox=altalake)

#canvec.qplot(bbox=altalake, layers=c("waterbody", "river"))
#canvec.qplot(bbox=altalake, layers=c("lake"))
#canvec.download(nts(bbox=altalake))
#tt = canvec.load(nts(bbox=altalake), "waterbody")

#tt2 = do.call(rbind, tt)
#tt2 <- subset(tt2, !is.na(tt2@data$LAKNAMEEN))
#tt2$area_sqkm <- area(tt2)*1e-6
#tt2 <- subset(tt2, tt2@data$area_sqkm > 0.1)


#closeAllConnections()
filt_data= st_as_sf(filt_data) %>%  
  st_transform(4326, crs(CRS(4326)))


df <-read.csv("data/df_house_wq.csv", header = T)
df2 = as_tibble(df) %>%
  mutate(lbasement_area = ifelse(bsmtarea == 0, 0, log(bsmtarea))) %>%
    filter((county %in% c("Muskoka","Kawartha Lakes"))) %>%
  mutate(id = seq(1:nrow(.))) %>%
 # sample_n(1000) %>%
  dplyr::select(id, longitude, latitude) 

#df2 = as.matrix(df2)
#df3 = dist2Line(df2, tt2, distfun=distGeo)

#points being the spatialpointsdataframe, and lines being your spatiallinesdataframe
#require(parallel)
#fun<-function(i) data.frame(dist2Line(df2[i,], tt2)) #some function like this

#cl <- makeCluster(detectCores())
#clusterEvalQ(cl, { library("geosphere") }) #don't know what this does, but it's how i learned this. 
#clusterExport(cl, c("dist2Line", "df2", "tt2")) #here you have to include all your objects and functions you want to use, and export them to a cluster, whatever that is.
#results <- parLapply(cl,df2,fun=fun) #use parLapply to 'loop' through the points and return a list of dataframes. should be a list. 


lake_name_index = tibble(lake_name = tt2$LAKNAMEEN,
                         lake_area = tt2$area_sqkm,
                         lake_index = seq(1:length(tt2$LAKNAMEEN)))

df_sf = df2 %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

df_sf$lake_index = st_nearest_feature(df_sf, tt2)
df_sf$dist = st_distance(df_sf, tt2[df_sf$lake_index,], by_element=TRUE)

df_sf  = left_join(df_sf, lake_name_index, by = "lake_index")

df_lakes <-read_csv("data/sd_merged_data.csv")

df_lakes = df_lakes %>% 
  filter(!is.na(lat_lon)) %>%
  mutate(lat = round(as.numeric(gsub( " .*$", "", lat_lon )), 4),
         lon = round(as.numeric(gsub( ".* ", "", lat_lon )), 4)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


tree_in_tract <- st_join(df_lakes, tt2, join = st_within)

df_lakes$lake_index <- st_nearest_feature(df_lakes, tt2)
df_lakes$dist = st_distance(df_lakes, tt2[df_lakes$lake_index,], by_element=TRUE)

df_lakes = left_join(df_lakes, lake_name_index, by = "lake_index")

df_test = tree_in_tract %>%
  dplyr::select(lake_name, LAKNAMEEN, area_sqkm)

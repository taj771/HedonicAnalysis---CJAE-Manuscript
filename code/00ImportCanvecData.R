
rm(list=ls(all=TRUE))

# https://www.hydrosheds.org/page/hydrolakes
# https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/

canvec_data_directory = c("C:/Users/pal453/OneDrive - University of Saskatchewan/Canvec/canvec_50K_ON_Hydro_shp/canvec_50K_ON_Hydro")

df_house = readRDS("data/df_house.rds") %>%
  filter(!is.na(latitude))

lat_house_max = max(df_house$latitude) + 2
lat_house_min = min(df_house$latitude) - 2
lon_house_max = max(df_house$longitude) + 2
lon_house_min = min(df_house$longitude) - 2

df_study_area_filter <- sf::st_bbox(c(xmin = ifelse(is.na(lon_house_min), -180, lon_house_min), 
                           ymin = ifelse(is.na(lat_house_min),  -90,  lat_house_min), 
                           xmax = ifelse(is.na(lon_house_max), +180, lon_house_max), 
                           ymax = ifelse(is.na(lat_house_max),  +90, lat_house_max)), 
                         crs = st_crs(4326)) %>% 
  sf::st_as_sfc(.)

df_water1 <- st_read(paste0(canvec_data_directory,"/waterbody_2_1.shp"))

df_water2 <- st_read(paste0(canvec_data_directory,"/waterbody_2_2.shp"))

df_water3 <- st_read(paste0(canvec_data_directory,"/waterbody_2_3.shp"))

df_water4 <- st_read(paste0(canvec_data_directory,"/waterbody_2_4.shp"))

df_water5 <- st_read(paste0(canvec_data_directory,"/waterbody_2_5.shp"))

df_water1 <- df_water1 %>%
  #  filter(definit_en == "Lake") %>%  
  st_transform(4326, crs(CRS(4326)))

df_water2 <- df_water2 %>%
  #  filter(definit_en == "Lake") %>%  
  st_transform(4326, crs(CRS(4326)))

df_water3 = df_water3 %>%
  bind_rows(df_water4, df_water5) %>%
  #  filter(definit_en == "Lake") %>%  
  st_transform(4326, crs(CRS(4326)))

find_data <- sf::st_intersects(df_water1, df_study_area_filter)
#> although coordinates are longitude/latitude, st_within assumes that they are planar
filt_data1 <- df_water1[which(lengths(find_data) != 0), ]

find_data <- sf::st_intersects(df_water2, df_study_area_filter)
filt_data2 <- df_water2[which(lengths(find_data) != 0), ]

find_data <- sf::st_intersects(df_water3, df_study_area_filter)
filt_data3 <- df_water2[which(lengths(find_data) != 0), ]

df_canvec = bind_rows(filt_data1, filt_data2, filt_data3)

rm(df_water1, df_water2, df_water3, df_water4, df_water5, filt_data1, filt_data2, filt_data3)

saveRDS(df_canvec, "C:/Users/pal453/OneDrive - University of Saskatchewan/Canvec/df_canvec.rds")
saveRDS(df_study_area_filter, "data/df_study_area_filter.rds")

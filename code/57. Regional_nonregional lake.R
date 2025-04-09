# LOad libraries
library(pacman)
pacman::p_load(sf,sp,rgdal,rgeos, tidyverse,leaflet,maptools) 



df <- df_main%>%
  select(region_lakes, lake_name_canvec, lake_index_canvec, lake_latlon_canvec)

df_region_lake <- df%>%
  subset(region_lakes == 1)%>%
  distinct(lake_latlon_canvec, .keep_all = T)

df_region_lake <- df_region_lake%>%
  separate(lake_latlon_canvec, into = c("latitude","longitude"), sep = ",")


df_region_lake <-  st_as_sf(df_region_lake, coords = c("longitude", "latitude"), crs = 4326)%>%
  distinct(lake_index_canvec, .keep_all = T)

st_write(df_region_lake, "./shapefile/regional_lake_points.shp")



df_non_region_lake <- df%>%
  subset(region_lakes == 0)%>%
  distinct(lake_latlon_canvec, .keep_all = T)


df_non_region_lake <- df_non_region_lake%>%
  separate(lake_latlon_canvec, into = c("latitude","longitude"), sep = ",")


df_non_region_lake <-  st_as_sf(df_non_region_lake, coords = c("longitude", "latitude"), crs = 4326)%>%
  distinct(lake_index_canvec, .keep_all = T)

st_write(df_non_region_lake, "./shapefile/non_regional_lake_points.shp")


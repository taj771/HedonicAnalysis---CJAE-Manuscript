rm(list=ls(all=TRUE))

library(pacman)
pacman::p_load(sf,sp,rgdal,tidyverse,leaflet,maptools) 




# parameter definition
buffer_1 <- 250
buffer_2 <- 500
individual_houses_ub <- 167 #square meters
individual_houses_lb <- 70 #square meters


# read shape file
on_lake <- st_read("./Chapter I/meta_analysis/data/processed/on_lakes.shp")

# read NS building foot print
on_fp <- st_read("./Chapter I/meta_analysis/Building_footprint/ON/on.shp")

# read census file db or da
on_census_da <- st_read( "./Chapter I/meta_analysis/Census/ON/census_on_da.shp")


# projection - make sure planer
on_lakes_epsg <- st_transform(on_lake, crs = 3348)


# creating buffer - meters (projection ensure this) - 250m
#on_lake_buffer_250 <- st_buffer(on_lakes_epsg, dist = buffer_1)

# save shapefile as it will take time and load it for furture analysis
st_write(on_lake_buffer_250, "./OntarioHouseSales/shapefile/on_lake_buffer_250.shp")

on_lake_buffer_250 <- st_read("./OntarioHouseSales/shapefile/on_lake_buffer_250.shp")


#projection - for map purpose - 250m
on_lake_buffer_250 <- st_transform(on_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
on_fp <- st_transform(on_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(on_lake_buffer_250)==st_crs(on_fp)

# filter building footprint data to make sure single houses (remove other type of buildings)
# based on polygon area
# calculate polygon area
# 50m
sf_use_s2(FALSE)
on_fp$area <- st_area(on_fp) 
on_fp$area<- as.numeric(on_fp$area, units="m")



# clip - building foot prints within buffer - buffer_1 (250m)
on_lake_buffer_250_epsg <- st_transform(on_lake_buffer_250, crs = 3348)

# projection
on_fp_epsg <- st_transform(on_fp, crs = 3348)


# This data layer consist with lake id and adjacent
#on_lake_home_250 <- st_intersection(on_lake_buffer_250_epsg, on_fp_epsg)

# save shapefile as it will take time and load it for furture analysis
st_write(on_lake_home_250, "./OntarioHouseSales/shapefile/on_lake_home_250.shp")

on_lake_home_250 <- st_read("./OntarioHouseSales/shapefile/on_lake_home_250.shp")



# merge census DA data and building foot print data within 250 buffer
# projection 
on_lake_home_250_espg <- st_transform(on_lake_home_250, crs = 3348)
on_census_da_espg <- st_transform(on_census_da, crs = 3348)
on_census_da_bfp_250 <- st_join(on_census_da_espg,on_lake_home_250_espg, all = TRUE )


# remove GeoUID where FID = 0, to make sure that GeoUID with zero building foot print does not count

on_census_da_bfp_250 <- on_census_da_bfp_250%>%
  subset(FID > 0)

# filter polygons based on house area
on_census_da_bfp_250 <- on_census_da_bfp_250%>%
  filter(area.1 < individual_houses_ub)
on_census_da_bfp_250 <- on_census_da_bfp_250%>%
  filter(area.1 > individual_houses_lb)

# count number of houses within buffer
on_cen_da_bfp_count_250 <- on_census_da_bfp_250%>%
  count(GeoUID)

# join census data
on_cen_da_bfp_count_250 <-on_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)

on_cen_da_bfp_count_250 <- left_join(on_census_da_espg,on_cen_da_bfp_count_250,by = "GeoUID")


# accounting for non-waterfront homes (homes located between the distance between 250m and 500m)

# creating buffer - meters (projection ensure this)
#on_lake_buffer_500 <- st_buffer(on_lakes_epsg, dist = buffer_2)

# save shapefile as it will take time and load it for furture analysis
st_write(on_lake_buffer_500, "./OntarioHouseSales/shapefile/on_lake_buffer_500.shp")

on_lake_buffer_500 <- st_read("./OntarioHouseSales/shapefile/on_lake_buffer_500.shp")



#projection - for map purpose
on_lake_buffer_500 <- st_transform(on_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(on_lake_buffer_500)==st_crs(on_fp)

# clip - building foot prints within buffer - non-waterfront (500m)

on_lake_buffer_500_epsg <- st_transform(on_lake_buffer_500, crs = 3348)


# This data layer consist with lake id and adjacent

#on_lake_home_500 <- st_intersection(on_lake_buffer_500_epsg, on_fp_epsg)


# save shapefile as it will take time and load it for furture analysis
st_write(on_lake_home_500, "./OntarioHouseSales/shapefile/on_lake_home_500.shp")

on_lake_home_500 <- st_read("./OntarioHouseSales/shapefile/on_lake_home_500.shp")



# The issue with shape file of ns_lake_home_500 has the houses that are also within
# the buffer of ns_lake_home_250 shape file. Thus in order to avoid double counting 
# will detect the houses within 250m buffer and remove it from 500m buffer

on_lake_home_500 <- on_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% on_lake_home_250$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))

# select the subset that ensure houses only within 250m-500m distance

on_lake_home_500 <- on_lake_home_500%>%
  subset(dup_record == "For analysis")



# merge census DA data and building foot print data within 500 buffer
# projection 
on_lake_home_500_espg <- st_transform(on_lake_home_500, crs = 3348)
on_census_da_bfp_500 <- st_join(on_census_da_espg,on_lake_home_500_espg, all = TRUE )


# remove GeoUID where FID = 0, to make sure that GeoUID with zero building foot print does not count

on_census_da_bfp_500 <- on_census_da_bfp_500%>%
  subset(FID > 0)

# filter polygons based on house area
on_census_da_bfp_500 <- on_census_da_bfp_500%>%
  filter(area.1 < individual_houses_ub)
on_census_da_bfp_500 <- on_census_da_bfp_500%>%
  filter(area.1 > individual_houses_lb)

# count number of houses within buffer
on_cen_da_bfp_count_500 <- on_census_da_bfp_500%>%
  count(GeoUID)


# join census data
on_cen_da_bfp_count_500 <-on_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)

on_cen_da_bfp_count_500 <- left_join(on_census_da_espg,on_cen_da_bfp_count_500,by = "GeoUID")


colnames(on_cen_da_bfp_count_250)[which(names(on_cen_da_bfp_count_250) == "n")] <- "wf_house"
colnames(on_cen_da_bfp_count_500)[which(names(on_cen_da_bfp_count_500) == "n")] <- "nwf_house"



study_area <- st_read("./OntarioHouseSales/shapefile/studyarea.shp")
study_area_epsg <- st_transform(study_area, crs = 3348)

on_cen_da_bfp_count_250_espg <-  st_transform(on_cen_da_bfp_count_250, crs = 3348)
on_cen_da_bfp_count_500_espg <-  st_transform(on_cen_da_bfp_count_500, crs = 3348)


sa_home_250 <- st_intersection(on_cen_da_bfp_count_250_espg, study_area_epsg)
sa_home_500 <- st_intersection(on_cen_da_bfp_count_500_espg, study_area_epsg)

st_write(sa_home_250, "./OntarioHouseSales/shapefile/sa_home_filter_250.shp")
st_write(sa_home_500, "./OntarioHouseSales/shapefile/sa_home_filter_500.shp")

#################################################################################
# Run above when need or load the saved files 

# without house area filter
sa_home_all_250 <- st_read("./OntarioHouseSales/shapefile/sa_home_all_250.shp")
sa_home_all_500 <- st_read("./OntarioHouseSales/shapefile/sa_home_all_500.shp")

# with house area filter
sa_home_filter_250 <- st_read("./OntarioHouseSales/shapefile/sa_home_filter_250.shp")
sa_home_filter_500 <- st_read("./OntarioHouseSales/shapefile/sa_home_filter_500.shp")

# number of houses 250 buffer - without filter

sum(sa_home_all_250$wf_house, na.rm = T)
# number of houses 500 buffer - without filter
sum(sa_home_all_500$nwf_house, na.rm = T)

# number of houses 250 buffer - with filter
sum(sa_home_filter_250$wf_house, na.rm = T)
# number of houses 500 buffer - with filter
sum(sa_home_filter_500$nwf_house, na.rm = T)



# number of houses within buffers using a house area threshholds are get from the 
# meta analysis analyis as it is already processesd.

sa_home_250_all <- st_read("./OntarioHouseSales/shapefile/sa_home_all_250.shp")
sa_home_500_all <- st_read("./OntarioHouseSales/shapefile/sa_home_all_500.shp")

sa_home_250_filter <- st_read("./Chapter I/meta_analysis/ON/sa_home_all_250.shp")
sa_home_500_filter <- st_read("./OntarioHouseSales/shapefile/sa_home_all_500.shp")






# calculate the cost or benefit based on predefined % change
ns_cen_da_bfp_count_250 <- ns_cen_da_bfp_count_250%>%
  mutate(value_wf = elast_est_wf*v_Avod.*percentate_chane*n)



# calculate the cost or benefit based on predefined % change
ns_cen_da_bfp_count_500 <- ns_cen_da_bfp_count_500%>%
  mutate(valuen_wf = elast_est_nwf*v_Avod.*percentate_chane*n)

#st_write(ns_cen_da_bfp_count_500, "./check_arcGIS/ns_census_da_bfp_count_500.shp")

# obtaining total value by combing value in waterfront and non-waterfront homes

colnames(ns_cen_da_bfp_count_250)[which(names(ns_cen_da_bfp_count_250) == "n")] <- "wf_house"
colnames(ns_cen_da_bfp_count_500)[which(names(ns_cen_da_bfp_count_500) == "n")] <- "nwf_house"

df <-ns_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,valuen_wf,GeoUID)

ns_cen_da_bfp_count_tot <- left_join(ns_cen_da_bfp_count_250,df, by =c("GeoUID"))

ns_cen_da_bfp_count_tot <- ns_cen_da_bfp_count_tot%>%
  mutate(total_value = value_wf+valuen_wf)

#st_write(ns_cen_da_bfp_count_tot, "./check_arcGIS/ns_census_db_bfp_count_tot.shp")



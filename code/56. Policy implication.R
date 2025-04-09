# Calibration of building footprint data with census data

# LOad libraries
library(pacman)
pacman::p_load(sf,sp,rgdal,rgeos, tidyverse,leaflet,maptools) 

# Import data

# shapefile with number of filterd building footprint (based on Hedonic ON 
# actual data set's information)

bfp_csd <- st_read( "./shapefile/bfp_filtered_count_at_csd.shp")

# get the ratio overall - number of building footprint within csd (filtered)

bfp_csd <- bfp_csd%>%
  mutate(ratio = Dwllngs/Join_Count)

bfp_csd <- bfp_csd%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID, ratio)
  

# import shapefile that has number of building foot print (filtered) within 250m
# boundary of lakes within study area

census_on_da_sa_bfp_count_filter_250m <- st_read( "./shapefile/census_on_da_sa_bfp_count_filter_250m.shp")

# merge filw with ratio of dwellings/ bfp count

census_on_da_sa_bfp_count_filter_250m  <- census_on_da_sa_bfp_count_filter_250m%>%
  left_join(bfp_csd,by = "GeoUID")

# calibrated bfp count

census_on_da_sa_bfp_count_filter_250m  <- census_on_da_sa_bfp_count_filter_250m%>%
  mutate(calibrated_bfp = ratio*Join_Count)

census_on_da_sa_bfp_count_filter_250m$calibrated_bfp  <- round(census_on_da_sa_bfp_count_filter_250m$calibrated_bfp)


# save file

st_write(census_on_da_sa_bfp_count_filter_250m, "./shapefile/calibrated_bfp_count_0_250m.shp")


# import shapefile that has number of building foot print (filtered) within 500m
# boundary of lakes within study area

census_on_da_sa_bfp_count_filter_500m <- st_read( "./shapefile/census_on_da_sa_bfp_count_filter_500.shp")

census_on_da_sa_bfp_count_filter_500m <- census_on_da_sa_bfp_count_filter_500m %>% 
  rename("bfp_count_0_500" = "Join_Count")

census_on_da_sa_bfp_count_filter_250m <- census_on_da_sa_bfp_count_filter_250m %>% 
  as.data.frame()%>%
  select(-geometry)%>%
  rename("bfp_count_0_250" = "Join_Count")%>%
  select(bfp_count_0_250, GeoUID)
  

# Filter out the bfp count within 250-500 boundary

census_on_da_sa_bfp_count_filter_500m  <- census_on_da_sa_bfp_count_filter_500m%>%
  left_join(census_on_da_sa_bfp_count_filter_250m,by = "GeoUID")

census_on_da_sa_bfp_count_filter_500m  <- census_on_da_sa_bfp_count_filter_500m%>%
  mutate(bfp_count_250_500 = bfp_count_0_500 - bfp_count_0_250)

# merge filw with ratio of dwellings/ bfp count

census_on_da_sa_bfp_count_filter_500m  <- census_on_da_sa_bfp_count_filter_500m%>%
  left_join(bfp_csd,by = "GeoUID")

# calibrated bfp count

census_on_da_sa_bfp_count_filter_500m  <- census_on_da_sa_bfp_count_filter_500m%>%
  mutate(calibrated_bfp = bfp_count_250_500)

census_on_da_sa_bfp_count_filter_500m$calibrated_bfp  <- round(census_on_da_sa_bfp_count_filter_500m$calibrated_bfp)
  
# save file

st_write(census_on_da_sa_bfp_count_filter_500m, "./shapefile/calibrated_bfp_count_250_500m.shp")


# Attach dwelling value at csd level

census_on_da_sa_bfp_count_filter_500m <- st_read("./shapefile/calibrated_bfp_count_250_500m.shp")%>%
  select(clbrtd_)%>%
  rename(bfp_250_500 = clbrtd_ )

# save bfp_250_500 to sum them at 

###################################################################################

# 0-250m
census_on_da_sa_bfp_count_filter_250m <- st_read("./shapefile/calibrated_bfp_count_0_250m.shp")
# 250m-500m
census_on_da_sa_bfp_count_filter_500m <- st_read("./shapefile/calibrated_bfp_count_250_500m.shp")


e_250m <- 0.063
e_500m <- 0.056

change <- 0.1

###########################################################################
# add value from census property value

# 250
census_on_da_sa_bfp_count_filter_250m <- census_on_da_sa_bfp_count_filter_250m%>%
  mutate(value_250m_census = clbrtd_*e_250m *v_Avod_*change)

# 250-500
census_on_da_sa_bfp_count_filter_500m <- census_on_da_sa_bfp_count_filter_500m%>%
  mutate(value_500m_census = clbrtd_*e_500m *v_Avod_*change)

# value within 250m - census dwelling value
sum(census_on_da_sa_bfp_count_filter_250m$value_250m_census, na.rm = T)


# value within 500m - census dwelling value
sum(census_on_da_sa_bfp_count_filter_500m$value_500m_census, na.rm = T)



# save files with values - including values estimations from based on both MPAC 
# average house values and census dwelling values 

census_on_da_sa_bfp_count_filter_250m <- census_on_da_sa_bfp_count_filter_250m%>%
  select(value_250m_census, GeoUID)

census_on_da_sa_bfp_count_filter_500m <- census_on_da_sa_bfp_count_filter_500m%>%
  select(value_500m_census, GeoUID)

census_on_da_sa_bfp_count_filter_250m <- census_on_da_sa_bfp_count_filter_250m%>%
  as.data.frame()%>%
  select(-geometry)

census_on_da_sa_bfp_count_filter_merge <- census_on_da_sa_bfp_count_filter_500m%>%
  left_join(census_on_da_sa_bfp_count_filter_250m, by = "GeoUID")%>%
  mutate(total_value = value_250m_census + value_500m_census)



st_write(census_on_da_sa_bfp_count_filter_500m, "./shapefile/census_on_da_sa_bfp_count_filter_500m_value.shp")

#st_write(census_on_da_sa_bfp_count_filter_250m, "./shapefile/census_on_da_sa_bfp_count_filter_250m_value.shp")

st_write(census_on_da_sa_bfp_count_filter_merge, "./shapefile/census_on_da_sa_bfp_count_filter_merge_value.shp")

################################################################################

################################################################################

df_sd_rollnum <- df_main %>%
  select(roll_num, sd_interp, year, sd_average)%>%
  group_by(roll_num)%>%
  mutate(sd_aver = mean(sd_interp))%>%
  distinct(roll_num, .keep_all = T)

df_hous_cordinat <- read.csv("./data/HouseData/df_house_coordinates_final.csv")%>%
  drop_na()%>%
  distinct(roll_num, .keep_all = T)


df_house_points <-  st_as_sf(df_hous_cordinat, coords = c("longitude", "latitude"), crs = 4326)%>%
  distinct(roll_num, .keep_all = T)

df_house_points_sd <- df_house_points%>%
  left_join(df_sd_rollnum, by = "roll_num")

# save file

st_write(df_house_points_sd, "./shapefile/house_point_with_SD.shp")

###########################################################################
# SD reading points

df <- df_main %>%
  drop_na(sd_interp)


df <- df %>%
  group_by(station_lat_lon,year)%>%
  mutate(sd_ave = mean(sd_interp))%>%
  distinct(station_lat_lon, year, .keep_all = T )


df_stations <- df_stations%>%
  left_join(df, by = "station_lat_lon" )%>%
  distinct(station_lat_lon, .keep_all = T)

st_write(df_stations, "./shapefile/df_stations_wt_sd.shp")


##############################################################################

df_subset = readRDS("data/analysis_sample.rds")

df <- df_subset%>%
  select(area_tot)

df <- df%>%
  mutate(area_sqm = area_tot*0.092903)

summary(df$area_sqm)

ggplot(df) +
  aes(x = "", y = area_sqm ) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


df_1 <- boxplot.stats(df$area_sqm)$out%>%
  as.data.frame()

out_ind <- which(df$area_sqm %in% c(df_1$.))

df_2 <- df[out_ind, ]




min(df_1)

max(df_1)


hist(df$area_sqm,
     xlab = "area",
     main = "Histogram of house area",
     breaks = 1000
) # set number of bins
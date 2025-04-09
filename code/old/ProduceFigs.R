# re-estimate distance to gta variable
# check out palgrave sd_dist, Innis lake. Need correct 

rm(list=ls(all=TRUE))

library(tidyverse)
library(janitor)



df_sd = read_csv("data/sd_merged_data.csv") %>%
  filter(!is.na(sd_interp)) %>% 
  filter(!is.na(lat_lon)) %>%
  rename(station_lat_lon = lat_lon) %>%
  distinct(year, station_lat_lon, .keep_all = T) %>%
  filter(!is.na(sd_average))

summary(df_sd)

df_sd %>%
  group_by(station_lat_lon) %>%
  tally()


df_subset = readRDS("data/analysis_sample.rds")

df_subset = df_subset  %>%
  filter(closest_type == "water", data_type == "station")

tt = df_subset  %>%
  filter(log_sd_500 != 0) %>%
  group_by(year, station_lat_lon) %>%
  summarise(count = n())

summary(df_subset$log_sd_500)

df = readRDS("data/analysis_sample.rds")

df_house_latlon =  read_csv("data/HouseData/df_house_coordinates_final.csv")


df_lake_lake = df %>%
  filter(closest_type == "lake", data_type == "lake") 

df_region = df_lake_lake %>%
  filter(dist_house_lake <= 500, !is.na(sd_average)) %>%
  mutate(lake_area_sqkm = round(lake_area_sqkm, 1)) %>%
  # mutate(close = ifelse(dist_house_lake <= 1000, "near", "far")) %>%
  group_by(lake_index_canvec, lake_name_canvec, lake_area_sqkm) %>%
  summarise(n_homes = length(unique(roll_num)),
            price000s = round(mean(price_real)/1000, 0),
            sd_mean = round(mean(sd_average), 1),
            sd_sd = round(sd(sd_average), 1),
            sd_min = round(min(sd_average), 1),
            sd_max = round(max(sd_average), 1)) %>%
  filter(n_homes > 5)

df_region = df_lake_lake %>%
  filter(dist_house_lake <= 500, !is.na(sd_average)) %>%
  # mutate(close = ifelse(dist_house_lake <= 1000, "near", "far")) %>%
  group_by(region, township, ward) %>%
  summarise(n_homes = length(unique(roll_num)),
            n_lakes = length(unique(lake_index_canvec)))


df_region = df_lake_lake %>%
  group_by(township) %>%
  summarise(n_homes = length(unique(roll_num)),
            n_lakes = length(unique(lake_index_canvec)))

df_summary = df %>%
  filter(closest_type == "lake", data_type == "lake") %>%
  group_by(region, county, township, ward) %>%
  summarise(n_homes = length(unique(roll_num)),
            n_lakes = length(unique(lake_index_canvec)))


df_summary = df %>%
  group_by(subset) %>%
  summarise(n_homes = length(unique(roll_num)),
            n_lakes = length(unique(lake_index_canvec)))

df_summary = df %>%
mutate(sd_missing = ifelse(is.na(sd_average), 1, 0),
       close = ifelse(dist_house_lake <= 1000, "near", "far"),
       lake_area_sqkm = round(lake_area_sqkm, 2)) %>%
  group_by(subset, sd_missing, close) %>%
  summarise(n_homes = length(unique(roll_num)),
            n_lakes = length(unique(lake_index_canvec)))
  
  
df_summary = df %>%
  filter(dist_house_lake <= 1000, closest_type != "water") %>%
  group_by(subset, lake_index_canvec, lake_name_canvec) %>%
  summarise(n_homes = length(unique(roll_num)),
            n_readings = sum(n_readings, na.rm = T))

house_dist_station_summary = df_subset %>%
  filter(dist_house_station > 20000, dist_house_lake <= 1000) %>%
  group_by(closest_type, data_type, lake_index_canvec, lake_name_canvec, region_lakes) %>%
  summarise(homes = n(),
            dist_km_mean  = round(mean(dist_house_station, na.rm = T) / 1000,0),
            dist_km_max  = round(max(dist_house_station, na.rm = T) / 1000,0),
            lake_size = round(mean(lake_area_sqkm), 1))

df_summary = df %>%
  filter(dist_house_station > 20000 & dist_house_lake <= 1000) %>%
  group_by(subset, lake_name_canvec) %>%
  summarise(n_homes = length(unique(roll_num)))

n_homes = df %>%
  group_by(subset) %>%
  summarise(homes = length(unique(roll_num)))

df = df  %>%
  left_join(df_house_latlon)


tt = df %>%
  mutate(sd_missing = ifelse(is.na(sd_average), 1, 0),
         close = ifelse(dist_house_lake <= 500, "near", "far"),
         lake_area_sqkm = round(lake_area_sqkm, 2)) %>%
  group_by(subset, lake_index_canvec, lake_name_canvec, water_type, 
           lake_area_sqkm, sd_missing, close) %>%
  summarise(homes = n(),
            lat_lon = paste0(mean(latitude, na.rm = T),", ", 
                             mean(longitude, na.rm = T)))



df_subset = df  %>%
  filter(closest_type == "lake", data_type == "stations") %>%
  left_join(df_house_latlon)

tt = df_subset %>%
  mutate(sd_missing = ifelse(is.na(sd_average), 1, 0),
         close = ifelse(dist_house_lake <= 500, "near", "far"),
         lake_area_sqkm = round(lake_area_sqkm, 2)) %>%
  group_by(lake_index_canvec, lake_name_canvec, water_type, lake_area_sqkm, sd_missing, close) %>%
  summarise(homes = n(),
            lat_lon = paste0(mean(latitude, na.rm = T),", ", 
                             mean(longitude, na.rm = T)))
            
tt_sd = df_subset %>%
  mutate(sd_missing = ifelse(is.na(sd_average), 1, 0),
         close = ifelse(dist_house_lake <= 500, "near", "far")) %>%
  group_by(lake_index_canvec, lake_name_canvec, year, lake_name_sd, sd_missing, close) %>%
  summarise(homes = n(),
            sum_readings = sum(n_readings) / homes,
            lat_lon = paste0(mean(latitude, na.rm = T),", ", 
                             mean(longitude, na.rm = T)))
  
  
tt1  = tt %>%
  filter(sd_missing == 1, close == "near")

tt = df %>%
  group_by(county) %>%
  summarise(count = n())

tt = df %>%
  group_by(year) %>%
  summarise(count = n())


tt = df_house_wq  %>%
  group_by(prop_type) %>%
  tally()

tt = df %>%
  group_by(county, sd_distance_1000m) %>%
  summarise(count = n())

tt = df_house_wq %>%
  mutate(close = ifelse(as.numeric(lake_dist_sd) <= 1000, "close", "far")) %>%
  group_by(county, year) %>%
  summarise(sd_average = mean(sd_average, na.rm = T),
            count = n())


tt = df %>%
  mutate(sd_aveage_count = ifelse(!is.na(sd_average), 1, 0)) %>%
  group_by(county, lake_name_sd, sd_distance_1000m) %>%
  summarise(sd_average = mean(sd_average, na.rm = T),
            count_sd = sum(sd_aveage_count),
            dist_station = mean(dist_house_station, na.rm=T),
            houses = n()) %>%
  group_by(county, sd_distance_1000m) %>%
  summarise(sd_average = round(mean(sd_average, na.rm = T),1),
            dist_station = mean(dist_station, na.rm=T),
            lakes = n(),
            count_sd = sum(count_sd),
            houses = sum(houses)) 

table(df_house_wq$interpolation)


df_propcode_summary = df_house_wq %>%
  group_by(propcode) %>%
  summarise(count = n())

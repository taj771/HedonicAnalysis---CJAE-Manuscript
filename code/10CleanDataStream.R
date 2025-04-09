
rm(list=ls(all=TRUE))

library(tidyverse)
library(janitor)
library(lubridate)

df_sd = read_csv("data/sd_merged_data.csv") 

df = read_csv("data/wq_data/DataStream-d30a75c.csv") %>%
  clean_names(.) %>%
  mutate(result_value = ifelse(result_unit == "cm", result_value / 100, result_value)) %>%
  select(dataset_name, std_id = monitoring_location_id, lake_name = monitoring_location_name,
         latitude = monitoring_location_latitude, longitude = monitoring_location_longitude,
         type = monitoring_location_type, date = activity_start_date, sd = result_value) %>%
  mutate(year = year(date),
         lake_name = tolower(lake_name)) %>%
  filter(!is.na(sd)) %>%
  group_by(dataset_name, std_id, lake_name, latitude, longitude, type, year) %>%
  summarise(sd_average = mean(sd),
            n_readings = n()) %>%
  filter(year > 2000, type != "Wetland")

df_nonlpp = df %>%
  filter(dataset_name != "Ontario Lake Partner Program (LPP)",
         type != "River/Stream")

df_nonlpp_name = df_nonlpp %>%
  distinct(dataset_name, lake_name, latitude, longitude) 


df_sd_name = df_sd %>%
  distinct(lake_index_sd, lake_name_sd, lat_lon) %>%
  rename(lake_name = lake_name_sd) %>%
  separate(lat_lon, into = c("latitude", "longitude"), sep = " ")

library(sf)
df_new_sf <- st_as_sf(df_nonlpp_name, coords = c("longitude", "latitude"),
                    # Change to your CRS
                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
st_is_longlat(df_new_sf)

df_sd_sf <- st_as_sf(df_sd_name, coords = c("longitude", "latitude"),
                      # Change to your CRS
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
st_is_longlat(df_sd_sf)

dist = as_tibble(st_distance(df_new_sf, df_sd_sf)) %>%
  mutate(new_id = seq(1:nrow(.))) %>%
  pivot_longer(-new_id, values_to = "distance", names_to = "df_sd")

df_sd_sf = as_tibble(df_sd_sf) %>%
  select(-geometry) %>%
  mutate(df_sd = paste0("V", seq(1:nrow(.))))

dist2 = dist %>%
  mutate(distance = as.numeric(distance)) %>%
  filter(distance < 100000) %>%
  group_by(new_id) %>%
  slice_min(distance, n=1) %>%
  arrange(new_id) %>%
  bind_cols(df_nonlpp_name) %>%
  left_join(df_sd_sf, by = "df_sd")

dist2 %>%
  group_by(dataset_name) %>%
  summarise(mean = mean(distance),
            count = n())




df_house_year = readRDS("data/df_house.rds") %>%
  distinct(roll_num, year) 

df_house_latlon =  read_csv("data/HouseData/df_house_coordinates_final.csv") %>%
  filter(!is.na(latitude))

df_house_year  = df_house_latlon %>%
  left_join(df_house_year) %>%
  select(-latitude, -longitude)

df_house_lake_dist = readRDS("data/df_house_lake_dist.rds")

df_house_lake_dist$roll_num <- as.character(df_house_lake_dist$roll_num)

df_house_year_lake =  df_house_year %>% 
  left_join(df_house_latlon) %>%
  left_join(df_house_lake_dist) %>% 
  filter(!is.na(lake_index_canvec))


#df_house_unique =  df_house_year_lake %>% 
#  distinct(roll_num, year, latitude, longitude, lake_index_canvec)  

#rm(df_house_year, df_house_latlon, df_house_lake_dist)

df_sd_latlon_year_lake = read_csv("data/df_sd_latlon_year_lake.csv")%>%
  rename(station_lat_lon = lat_lon)

df_sd_sdindex_latlon_year = read_csv("data/sd_merged_data.csv") %>%
  filter(!is.na(sd_interp)) %>% 
  filter(!is.na(lat_lon)) %>%
  rename(station_lat_lon = lat_lon) %>%
  distinct(year, station_lat_lon, .keep_all = T)

df_lakes_year_sd = df_sd_latlon_year_lake %>%
  left_join(df_sd_sdindex_latlon_year) %>%
  filter(!is.na(sd_average)) %>%
  select(lake_index_sd, station_lat_lon, year, lake_index_canvec)

# Calc distance to stations for canvec
df_house_station = df_house_year_lake %>%
  left_join(df_sd_latlon_year_lake) %>%
  mutate(data_type = "station") 

df_house_station = df_house_year_lake %>%
  left_join(df_lakes_year_sd)  %>%
  mutate(data_type = "lake") %>%
  bind_rows(df_house_station) 

df_house_station_missinglatlon = df_house_station %>%
  filter(is.na(station_lat_lon)) %>%
  mutate(dist_house_station = NA)

df_house_station = df_house_station %>%
  filter(!is.na(station_lat_lon)) 

rm(df_house_lake_dist, df_house_latlon, df_house_year, df_house_year_lake, df_lakes_year_sd,
   df_sd_latlon_year_lake, df_sd_sdindex_latlon_year)

# need to split by grouping variable
df_house_station = split(df_house_station, f = df_house_station$year) 

gc()

plan(multisession, workers = 6)

start = Sys.time()

with_progress({
  p <- progressor(steps = length(df_house_station))
  
  df_house_station = future_map_dfr(df_house_station, function(x){
    p()
    x = x %>%
      mutate(lat_sd = round(as.numeric(gsub( " .*$", "", station_lat_lon )), 6),
             lon_sd = round(as.numeric(gsub( ".* ", "", station_lat_lon )), 6)) %>%
      mutate(dist_house_station = spatialrisk::haversine(latitude, longitude, lat_sd, lon_sd)) %>%
      group_by(roll_num, year, closest_type, data_type) %>%
      slice_min(dist_house_station, n = 1)  %>%
      ungroup(.)
    
    return(x)
  }, .options = furrr_options(seed = TRUE))
  
})
closeAllConnections()
gc()
end = Sys.time()
end - start

df_house_station = df_house_station %>%
  bind_rows(df_house_station_missinglatlon) %>%
  select(roll_num, year, lake_index_canvec, dist_house_lake, station_lat_lon, 
         dist_house_station, closest_type, data_type)

saveRDS(df_house_station, "data/df_house_station.rds")


rm(list=ls(all=TRUE))

library(tidyverse)
library(tidystringdist)
library(sf)
library(furrr)
library(progressr)

minimum_lake_size = 0.04 # sq km

df_canvec = readRDS("C:/Users/pal453/OneDrive - University of Saskatchewan/Canvec/df_canvec.rds")

# minimum lake size
df_canvec = df_canvec %>%
  mutate(area_sqkm = as.numeric(st_area(.))*1e-6) %>%
  filter(area_sqkm > minimum_lake_size )

df_canvec = st_as_sf(df_canvec) %>%  
  st_transform(4326, crs(CRS(4326)))

df_canvec = df_canvec %>%
  select(namelk1en, area_sqkm) %>%
  mutate(canvec_lake_id = seq(1:nrow(.)))

gc()

df_study_area_filter = readRDS("data/df_study_area_filter.rds")

df_house = readRDS("data/df_house_new.rds")

df_house_latlon =  read_csv("data/HouseData/df_house_coordinates_final.csv") %>%
  filter(!is.na(latitude))

df_house_year = df_house  %>%
  distinct(roll_num, year) 

df_house_year  = df_house_latlon %>%
  left_join(df_house_year) %>%
  select(-latitude, -longitude)

df_house_latlon_sf = df_house_latlon   %>%
  dplyr::select(roll_num, longitude, latitude) %>%
#    sample_n(1000)  %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

df_house_latlon_sf$lake_index_canvec = st_nearest_feature(df_house_latlon_sf, df_canvec)

df_sd_latlon_year_lake = read_csv("data/df_sd_latlon_year_lake.csv") %>%
  distinct(lake_index_canvec)



plan(multisession, workers = 6)

temp = split(df_house_latlon_sf, (seq(nrow(df_house_latlon_sf))-1) %/% 10)
#temp = temp[1:10]

out <- vector("list", length = length(temp))

start = Sys.time()

for(i in 1:length(df_house_latlon_sf)){
  print(i)
  start1 = Sys.time()
  plan(multisession, workers = 5)
  temp = split(temp[[i]], (seq(nrow(temp[[i]]))-1) %/% 2) 
  
  with_progress({
    p <- progressor(steps = length(temp))
    
    out[[i]] = future_map_dfr(temp, function(x, y = df_canvec){
      p()
      x$house_lake_dist = st_distance(x, 
                                   y[x$lake_index_canvec,], 
                                   by_element=TRUE)
      
      return(x)
    }, .options = furrr_options(seed = TRUE, scheduling = 20))
  })
  rm(temp)
  closeAllConnections()
  saveRDS(out[[i]], paste0("data/df_house_wq_fresh4",i,".rds"))
  gc()
  end1 = Sys.time()
  print(end1 - start1)
}

end = Sys.time()
end - start


df_house_latlon_sf$dist = st_dist(df_house_latlon_sf, df_canvec)

  
df_sd_latlon_year_lake = read_csv("data/df_sd_latlon_year_lake.csv")

df_house_latlon_lake = as_tibble(df_house_latlon_sf) %>%
  select(-geometry)

str(df_sd_sdindex_latlon_year)

df_all =  df_house_year %>% 
  left_join(df_house_latlon_lake) %>% 
  left_join(df_house_latlon) %>%
  left_join(df_sd_latlon_year_lake) %>%
  filter(!is.na(lat_lon))

df_house = df_all %>%
  select(roll_num, year, lat_lon)


df_all$dist = st_distance(df_all$,y[x$lake_index_canvec, ], by_element = T)

plan(multisession, workers = 6)

temp = split(df_all, (seq(nrow(df_all))-1) %/% 2)
temp = temp[1:10]
start = Sys.time()


with_progress({
  p <- progressor(steps = length(temp))
  
  df_sdlatlon_lake_dist = future_map_dfr(temp, function(x, y = df_canvec){
    p()

    x$dist = st_distance(x,y[x$lake_index_canvec, ], by_element = T)
    return(x)
  }, .options = furrr_options(seed = TRUE))
})
closeAllConnections()
gc()
end = Sys.time()
end - start

df_house_join = df_house %>%
  dplyr::select(sale_id, year, latitude, longitude)

df_house_wq = as_tibble(df_house)  %>%
  filter(!is.na(latitude)) %>%
  dplyr::select(sale_id, longitude, latitude) %>%
#  sample_n(1650)  %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# randomizes rows to more equalize distance calcs
df_house_wq = df_house_wq[sample(nrow(df_house_wq)),]

df_house_wq$lake_index_canvec_keep = st_nearest_feature(df_house_wq, df_canvec_keep)
df_house_wq = split(df_house_wq, (seq(nrow(df_house_wq))-1) %/% round(nrow(df_house_wq) / 3))

rm(df_house, df_canvec, find_data, 
   df_lake_canvec, df_lake_sd_canvec_index, df_sdlatlon_lake,
   df_lake_sd_canvec_index_remove, df_lake_sd_canvec_index_remove2,
   df_lake_sd_distinct, df_lake_sd_distinct_keep, df_lakes_sd, 
   df_study_area_filter)

gc()

out <- vector("list", length = length(df_house_wq))

start = Sys.time()

for(i in 1:length(df_house_wq)){
  print(i)
  start1 = Sys.time()
plan(multisession, workers = 5)
  temp = split(df_house_wq[[i]], (seq(nrow(df_house_wq[[i]]))-1) %/% 20) 

with_progress({
  p <- progressor(steps = length(temp))
  
out[[i]] = future_map_dfr(temp, function(x, y = df_canvec_keep){
  p()
  x$lake_dist_sd = st_distance(x, 
                               y[x$lake_index_canvec_keep,], 
                               by_element=TRUE)

  return(x)
}, .options = furrr_options(seed = TRUE, scheduling = 20))
})
rm(temp)
closeAllConnections()
saveRDS(out[[i]], paste0("data/df_house_wq_fresh",i,".rds"))
gc()
end1 = Sys.time()
print(end1 - start1)
}

end = Sys.time()
end - start

df_house_wq <-	bind_rows(out) 

df_house_wq = df_house_wq %>% 
#  sample_n(1000) %>%
  arrange(sale_id) 

saveRDS(df_house_wq, "data/df_house_wq_fresh.rds")

df_house_wq = readRDS("data/df_house_wq_fresh.rds")

df_house_wq2 = as_tibble(df_house_wq) %>%
  left_join(df_house_join, by = "sale_id") %>%
  left_join(as_tibble(df_lakes_sd_join2), by = "lake_index_canvec_keep") %>% # ensure every house has lake index/name
  left_join(as_tibble(df_lakes_sd_join), 
            by = c("lake_index_canvec_keep", "lake_name_sd", "lake_index_sd", "year"))


#df_lakes_sd_join2 = df_lakes_sd_join %>%
#  distinct(lake_index_canvec_keep, lake_name_sd, lake_name_canvec)
#df_missing_sd = df_house_wq %>%
#  filter(is.na(sd_interp)) %>%
#  group_by(lake_index_canvec_keep, year) %>%
#  tally() %>%
#  left_join(df_lakes_sd_join2)

df_house_wq_duplicates = df_house_wq2 %>%
  group_by(sale_id) %>%
  filter(n()>1) %>%
  filter(!is.na(lat_lon)) %>% # drop missing stations for lake for that year
    mutate(lat_sd = round(as.numeric(gsub( " .*$", "", lat_lon )), 4),
         lon_sd = round(as.numeric(gsub( ".* ", "", lat_lon )), 4)) %>%
  mutate(dist_station = spatialrisk::haversine(latitude, longitude, lat_sd, lon_sd)) %>%
  filter(dist_station == min(dist_station, na.rm = T)) %>%
  dplyr::select(-lat_sd, -lon_sd)

df_house_wq3 = df_house_wq2 %>%
  group_by(sale_id) %>%
  filter(n() == 1) %>%
#  filter(!is.na(lat_lon)) %>% # drop missing stations for lake for that year
  mutate(lat_sd = round(as.numeric(gsub( " .*$", "", lat_lon )), 4),
         lon_sd = round(as.numeric(gsub( ".* ", "", lat_lon )), 4)) %>%
  mutate(dist_station = ifelse(is.na(lat_lon), NA, spatialrisk::haversine(latitude, longitude, lat_sd, lon_sd))) %>%
  dplyr::select(-lat_sd, -lon_sd) %>%
  bind_rows(df_house_wq_duplicates) %>%
  dplyr::select(-lat_lon, -geometry) %>%
  as_tibble(.)


df_house_wq_id = as_tibble(df_house_wq3) %>%
  distinct(sale_id, year, lake_name_sd)

df_house_id = df_house_join %>%
  distinct(sale_id, year) %>%
  left_join(df_house_wq_id) %>%
  filter(is.na(lake_name_sd))

df_house_wq_missing  =as_tibble( df_house_wq) %>%
  filter(sale_id %in% df_house_id$sale_id) %>%
  left_join(as_tibble(df_house_wq2))

lakes_missing = as_tibble(df_house_wq_missing) %>%
  distinct(sale_id, lake_index_canvec_keep, year, .keep_all = T) %>%
  filter(as.numeric(lake_dist_sd) < 2000) %>%
  group_by(lake_name_sd, lake_index_canvec_keep, year) %>%
  tally()

saveRDS(df_house_wq3, "data/df_house_wq.rds")

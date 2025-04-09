
rm(list=ls(all=TRUE))

library(tidyverse)
library(tidystringdist)
library(sf)
library(furrr)
library(progressr)

minimum_lake_size = 0.04 # sq km

df_canvec = readRDS("C:/Users/pal453/OneDrive - University of Saskatchewan/Canvec/df_canvec.rds")
df_study_area_filter = readRDS("data/df_study_area_filter.rds")

df_house = readRDS("data/df_house_new.rds")

df_house_latlon =  read_csv("data/HouseData/df_house_coordinates_final.csv") %>%
  filter(!is.na(latitude))

df_house = df_house %>%
  left_join(df_house_coordinates)

df_house_year = df_house %>%
  distinct(roll_num, year)

df_house_latlon_sf = df_house_latlon   %>%
  dplyr::select(roll_num, longitude, latitude) %>%
  #  sample_n(1650)  %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#-

# minimum lake size
df_canvec = df_canvec %>%
  mutate(area_sqkm = as.numeric(st_area(.))*1e-6) %>%
  filter(area_sqkm > minimum_lake_size )

df_canvec = st_as_sf(df_canvec) %>%  
  st_transform(4326, crs(CRS(4326)))

df_canvec = df_canvec %>%
  select(namelk1en, area_sqkm)

gc()

df_house_latlon_lake =
  
  
  
df_lakes_sd = read_csv("data/sd_merged_data.csv") %>%
  filter(!is.na(sd_interp)) %>% 
  filter(!is.na(lat_lon))

# check lake data return within bbox

df_lake_sd_distinct_all = df_lakes_sd  %>%
  distinct(lake_index_sd, lat_lon, .keep_all = T) %>%
  mutate(lat = round(as.numeric(gsub( " .*$", "", lat_lon )), 4),
         lon = round(as.numeric(gsub( ".* ", "", lat_lon )), 4)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(lake_index_sd, lake_name_sd, lat_lon, geometry)

find_data <- sf::st_within(df_lake_sd_distinct_all, df_study_area_filter)
df_lake_sd_distinct <- df_lake_sd_distinct_all[which(lengths(find_data) != 0), ]

#df_canvec_river = df_canvec %>%
#  filter(definit_en != "Lake" & 
#           definit_en != "Pond" & 
#           definit_en != "Reservoir") 

#summary(df_canvec$area_sqkm)

df_lake_canvec = tibble(lake_name_canvec = df_canvec$namelk1en,
                        lake_area = df_canvec$area_sqkm,
                        lake_index_canvec = seq(1:length(df_canvec$namelk1en)))

df_lake_sd_distinct_keep = df_lake_sd_distinct

df_lake_sd_distinct_keep$lake_index_canvec = st_nearest_feature(df_lake_sd_distinct_keep, 
                                                                df_canvec)

df_lake_sd_distinct_keep$dist = st_distance(df_lake_sd_distinct_keep, 
                                    df_canvec[df_lake_sd_distinct_keep$lake_index_canvec,], 
                                    by_element=TRUE)

df_lake_sd_canvec_index_all = df_lake_sd_distinct_keep %>%
    left_join(df_lake_canvec, by = "lake_index_canvec") %>%
  mutate(lake_name_canvec_lowercase = tolower(lake_name_canvec)) %>%
  tidy_stringdist(lake_name_sd, lake_name_canvec_lowercase, method = "jw") %>%
  mutate(dist = as.numeric(dist)) %>%
  mutate(lake_name_canvec_lowercase_nolake = trimws(str_remove(lake_name_canvec_lowercase, "lake")),
         lake_name_sd_nolake = trimws(str_remove(lake_name_sd, "lake"))) %>%
  rename(jw1 = jw) %>%
  tidy_stringdist(lake_name_sd_nolake, lake_name_canvec_lowercase_nolake, method = "jw") 

df_lake_sd_canvec_index = df_lake_sd_canvec_index_all %>%
  filter(dist < 25 | jw1 < .25 | jw < .25 | 
           lake_name_sd == "dill lake (t lake)" |
           lake_name_sd == "neighick lake (beaver)" |
           lake_name_sd == "cranberry lake" |
           lake_name_sd == "little joe lake" |
           lake_name_sd == "kusk lake (rat)") %>%
  dplyr::select(lake_index_sd, lake_index_canvec)

df_lake_sd_canvec_index_remove = df_lake_sd_canvec_index_all %>%
  filter(!(lake_index_sd %in% df_lake_sd_canvec_index$lake_index_sd))

# Check removal numbers
df_lake_sd_canvec_index_remove2 = df_lake_sd_canvec_index_remove %>%
  left_join(df_lakes_sd, by = c("lake_name_sd", "lat_lon"))

df_lake_canvec = df_lake_canvec %>%
  mutate(index = ifelse(lake_index_canvec %in% df_lake_sd_canvec_index$lake_index_canvec, 
                                   1, 0)) %>%
  group_by(index) %>%
  mutate(lake_index_canvec_keep = row_number()) %>%
  mutate(lake_index_canvec_keep = ifelse(index == 0, NA, lake_index_canvec_keep))

find_data = as.list(df_lake_canvec$index)

df_lake_canvec = df_lake_canvec %>%
  ungroup(.) %>%
  dplyr::select(-index)

df_canvec_keep <- df_canvec[which(find_data != 0), ]

df_lakes_sd_join = df_lakes_sd %>%
  left_join(df_lake_sd_canvec_index, by = "lake_index_sd") %>%
  #  dplyr::select(lake_name_sd, lake_index_sd, lake_index_canvec, lat_lon, year, sd_interp) %>%
  left_join(df_lake_canvec, by = "lake_index_canvec") %>%
  filter(!is.na(lake_index_canvec_keep))

# ensure all houses have lake names
df_lakes_sd_join2 = df_lakes_sd_join %>%
  distinct(lake_index_canvec_keep, lake_name_sd, lake_index_sd)

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
   df_lake_canvec, df_lake_sd_canvec_index, df_lake_sd_canvec_index_all,
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

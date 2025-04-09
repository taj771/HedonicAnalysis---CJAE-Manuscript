
rm(list=ls(all=TRUE))

library(tidyverse)
library(tidystringdist)
library(sf)
library(furrr)
library(progressr)

df_canvec = readRDS("F:/Data/canvec_50K_ON_Hydro_shp/df_canvec_teranet.rds")
df_study_area_filter = readRDS("data/df_study_area_filter_teranet.rds")

df_house = read.csv("data/TeranetData.csv", header = T)

df_lakes = read_csv("data/sd_merged_data.csv") %>%
  filter(!is.na(sd_interp))

# check lake data return within bbox

df_lake_distinct = df_lakes %>% 
  filter(!is.na(lat_lon)) %>%
  mutate(lat_lon = ifelse(lake_name == "ahmic lake", paste0("45.64752558569451 -79.71676039705075"), lat_lon)) %>%
  distinct(lake_index_sd, lat_lon, .keep_all = T) %>%
  mutate(lat = round(as.numeric(gsub( " .*$", "", lat_lon )), 4),
         lon = round(as.numeric(gsub( ".* ", "", lat_lon )), 4)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(lake_index_sd, lake_name, lat_lon, geometry)

find_data <- sf::st_within(df_lake_distinct, df_study_area_filter)
df_lake_distinct <- df_lake_distinct[which(lengths(find_data) != 0), ]

# reformulate canvec
df_canvec$area_sqkm <- as.numeric(st_area(df_canvec))*1e-6

#summary(df_canvec$area_sqkm)

df_canvec <- df_canvec %>%
  filter(area_sqkm > 0.1)

#summary(df_canvec$area_sqkm)

df_canvec= st_as_sf(df_canvec) %>%  
  st_transform(4326, crs(CRS(4326)))

df_canvec = df_canvec %>%
  select(namelk1en, area_sqkm)

lake_name_index_canvec = tibble(lake_name_canvec = df_canvec$namelk1en,
                                lake_area = df_canvec$area_sqkm,
                                lake_index_canvec = seq(1:length(df_canvec$namelk1en)))

df_lake_distinct_keep = df_lake_distinct

df_lake_distinct_keep$lake_index_canvec <- st_nearest_feature(df_lake_distinct_keep, df_canvec)
df_lake_distinct_keep$dist = st_distance(df_lake_distinct_keep, 
                                    df_canvec[df_lake_distinct_keep$lake_index_canvec,], 
                                    by_element=TRUE)

df_lake_distinct_keep = left_join(df_lake_distinct_keep, lake_name_index_canvec, by = "lake_index_canvec")

df_lake_distinct_keep = df_lake_distinct_keep %>%
  mutate(lake_name_canvec2 = tolower(lake_name_canvec)) %>%
  tidy_stringdist(lake_name, lake_name_canvec2, method = "jw") %>%
  mutate(dist = as.numeric(dist)) %>%
  filter(dist < 20 | jw < .2) %>%
  dplyr::select(lake_index_sd, lake_name, lat_lon, lake_index_canvec, lake_name_canvec, dist)

lake_name_index_canvec = lake_name_index_canvec %>%
  mutate(index = as.integer(ifelse(lake_index_canvec %in% df_lake_distinct_keep$lake_index_canvec, 
                                   1, 0))) %>%
  group_by(index) %>%
  mutate(lake_index_canvec_keep = row_number()) %>%
  mutate(lake_index_canvec_keep = ifelse(index == 0, NA, lake_index_canvec_keep))

find_data = as.list(lake_name_index_canvec$index)

lake_name_index_canvec = lake_name_index_canvec %>%
  ungroup(.) %>%
  dplyr::select(-index)

df_canvec_keep <- df_canvec[which(find_data != 0), ]

#ttt = tibble(lake_name = df_canvec_keep$namelk1en)

df_house = as_tibble(df_house) %>%
  mutate(id = row_number()) %>%
  rename_all(tolower)


df_house_wq = df_house %>%
  dplyr::select(id, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# randomizes rows to more equalize distance calcs
df_house_wq = df_house_wq[sample(nrow(df_house_wq)),]

df_house_wq$lake_index_canvec_keep = st_nearest_feature(df_house_wq, df_canvec_keep)
df_house_wq = split(df_house_wq, (seq(nrow(df_house_wq))-1) %/% round(nrow(df_house_wq) / 1))

#rm(df_house, df_lakes, df_canvec, df_lake_distinct, find_data, lake_name_index_canvec, df_study_area_filter)

gc()

out <- vector("list", length = length(df_house_wq))

start = Sys.time()

for(i in 1:length(df_house_wq)){
  print(i)
  start1 = Sys.time()
plan(multiprocess, workers = 6)
  temp = split(df_house_wq[[i]], (seq(nrow(df_house_wq[[i]]))-1) %/% 200) 

with_progress({
  p <- progressor(steps = length(temp))
  
out[[i]] = future_map_dfr(temp, function(x, y = df_canvec_keep){
  p()
  x$lake_dist_sd = st_distance(x, 
                               y[x$lake_index_canvec_keep,], 
                               by_element=TRUE)

  return(x)
}, .options = furrr_options(seed = TRUE, scheduling = 5))
})
rm(temp)
closeAllConnections()
gc()
end1 = Sys.time()
print(end1 - start1)
}

end = Sys.time()
end - start

df_house_wq <-	bind_rows(out) 

df_house_wq = df_house_wq %>%
  arrange(id)

saveRDS(df_house_wq, "data/df_house_wq_teranet.rds")





df_house_wq = readRDS("data/df_house_wq_teranet.rds")

df_house_wq  = left_join(df_house_wq, lake_name_index_canvec, by = "lake_index_canvec_keep") %>%
  dplyr::select(id, lake_index_canvec_keep, lake_index_canvec, lake_dist_sd, lake_name_canvec, lake_area)

df_lake_distinct_join = tibble(lake_index_sd = df_lake_distinct_keep$lake_index_sd,
                               lake_index_canvec = df_lake_distinct_keep$lake_index_canvec)

df_lakes_join = df_lakes %>%
  dplyr::select(lake_index_sd, year, sd_interp, lat_lon)

df_house_join = df_house %>%
  select(id, latitude, longitude, year)

df_house_wq = df_house_wq %>%
  left_join(df_house_join, by = "id") %>%
  left_join(df_lake_distinct_join, by = "lake_index_canvec") %>%
  left_join(df_lakes_join, by = c("lake_index_sd", "year"))

df_house_wq_duplicates = df_house_wq %>%
  group_by(id) %>%
  filter(n()>1) %>%
    mutate(lat_sd = round(as.numeric(gsub( " .*$", "", lat_lon )), 4),
         lon_sd = round(as.numeric(gsub( ".* ", "", lat_lon )), 4)) %>%
  mutate(dist_station = spatialrisk::haversine(latitude, longitude, lat_sd, lon_sd)) %>%
  filter(dist_station == min(dist_station, na.rm = T)) %>%
#  df_house_wq_duplicates = df_house_wq_duplicates %>%
#  filter(n()>1) %>%
  dplyr::select(-lat_sd, -lon_sd, -dist_station)

df_house_wq = df_house_wq %>%
  group_by(id) %>%
  filter(n() == 1) %>%
  bind_rows(df_house_wq_duplicates)
  
tt = df_house_wq %>%
#  filter(as.numeric(lake_dist_sd) < 5000) %>%
#  left_join(as_tibble(df_lake_distinct), by = "lake_index_sd") %>%
  filter(is.na(sd_interp)) %>%
  group_by(lake_name_canvec, lake_index_sd, lake_name) %>%
  tally() %>%
  arrange(-n)

#tt = df_house_wq %>%
#  filter(lake_index_sd == 1389)

df_house_town =  df_house %>%
  select(id, real_price:longitude)  %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

df_township <- st_read(
  "data/Geographic_Township_Improved/Geographic_Township_Improved.shp") %>%  
  st_transform(4326, crs(CRS(4326)))

df_house_town2 = st_join(df_house_town, df_township, join = st_within)  %>%
  rename(township = OFFICIAL_N) %>%
  select(id, real_price:c_id, township) %>%
  as_tibble(.)


df_house_wq = df_house_wq %>%
  select(id, year, lake_dist_sd, sd_interp, lake_area ) %>%
  left_join(df_house_town2, by = c("id", "year"))


df_dist = df_house_wq %>%
  as_tibble() %>%
#  select(-geometry) %>%
  filter(as.numeric(lake_dist_sd) <= 10000) %>%
  mutate(sd_distance_100m = ifelse(as.numeric(lake_dist_sd) <= 100, 1, 0),
         #sd_distance_200m = ifelse(as.numeric(lake_dist_sd) > 50 & as.numeric(lake_dist_sd) <= 200, 1, 0),
         sd_distance_500m = ifelse(as.numeric(lake_dist_sd) > 100 & as.numeric(lake_dist_sd) <= 500, 1, 0),
         sd_distance_1000m = ifelse(as.numeric(lake_dist_sd) > 500 & as.numeric(lake_dist_sd) <= 1000, 1, 0),
         sd_distance_2000m = ifelse(as.numeric(lake_dist_sd) > 1000 & as.numeric(lake_dist_sd) <= 2000, 1, 0),
         sd_distance_2000m_over = ifelse(as.numeric(lake_dist_sd) > 2000, 1, 0))#,
    #    sd_interp = ifelse(is.na(sd_interp), 0, sd_interp))





library(fixest)

df_subset = df_dist %>%
  filter(between(real_price, quantile(real_price, .01), 
                 quantile(real_price, .99)))

summary(df_subset)


#---------
x_struct = c("log(lot_size)", "log(lake_area)", "city_distance")

f_sd <- paste("log(real_price)  ~",
          #    paste("sd_interp"),"+",
              paste("i(sd_interp, sd_distance_100m, ref = 0)"),"+",
          #    paste("i(sd_interp, sd_distance_200m, ref = 0)"),"+",
              paste("i(sd_interp, sd_distance_500m, ref = 0)"),"+",
              paste("i(sd_interp, sd_distance_1000m, ref = 0)"),"+",
              paste("i(sd_interp, sd_distance_2000m, ref = 0)"),"+",
  #            paste("i(sd_interp, sd_distance_20km, ref = 0)"),"+",
  #            paste("i(sd_distance_50m)"),"+",
      paste("i(sd_distance_100m)"),"+",
      paste("i(sd_distance_500m)"),"+",
      paste("i(sd_distance_1000m)"),"+",
      paste("i(sd_distance_2000m)"),"+",
  #            paste("i(sd_distance_10km)"),"+",
   #           paste("i(sd_distance_20km)"),"+",
              paste(x_struct, collapse=" + "), 
              "  | year + csw0(township)")


res_multi = feols(as.formula(f_sd), 
                  df_subset)
etable(res_multi)




library(lubridate)
my_stamp<-stamp( "2019-02", 
                 orders = "ym") 

df_house_town2 = df_house_town %>%
  group_split(year)

df_house_new = read.csv("data/df_house_wq.csv", header = T)

df_house_new2 = df_house_new %>%
  mutate(id = row_number()) %>%
  select(id, longitude, latitude, date, sale_amt) %>%
  mutate(date = as.Date(date, format = c("%Y-%m-%d"))) %>%
#  mutate(date = my_stamp(date)) %>%
  filter(date > "2004-12-31", date < "2015-01-01")

df_study_area_filter = readRDS("data/df_study_area_filter_teranet.rds")
df_house_new2 = df_house_new2 %>%
  arrange(date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

find_data <- sf::st_within(df_house_new2, df_study_area_filter)
df_house_new2 <- df_house_new2[which(lengths(find_data) != 0), ]

df_house_new2 = df_house_new2 %>%
  mutate(year = year(date)) %>%
 # sample_n(1000) %>%
  group_split(year)

library(nngeo)

gc()

out <- vector("list", length = length(df_house_new2))

start = Sys.time()

for(i in 1:length(df_house_new2)){
  print(i)
  start1 = Sys.time()
  
  out[[i]] = st_nn(x = df_house_town2[[i]], y = df_house_new2[[i]], 
                                   k =1, 
                                   returnDist = T,
                                 #  maxdist = 1000, 
                                   parallel = 4)
  
  gc()
  end1 = Sys.time()
  print(end1 - start1)
}
  closeAllConnections()

end = Sys.time()
end - start


gc()

out2 <- vector("list", length = length(df_house_new2))

start = Sys.time()

for(i in 1:length(df_house_new2)){
  print(i)
  start1 = Sys.time()
  
  out2[[i]] = df_house_town2[[i]]
  id2 = unlist(out[[i]]$dist)
  out2[[i]]$match_id = df_house_new2[[i]][id2,]$id
  y
    
    
    st_nn(x = df_house_town2[[i]], y = df_house_new2[[i]], 
                   k =1, 
                   returnDist = T,
                   #  maxdist = 1000, 
                   parallel = 4)
  
  gc()
  end1 = Sys.time()
  print(end1 - start1)
}
closeAllConnections()

end = Sys.time()
end - start



df_house_dist <-	bind_rows(out) 

df_house_new2 = bind_rows(df_house_new2) %>%
  rename(id_match = id) %>%
  as_tibble(.)
df_house_town2 = bind_rows(df_house_town2)

df_house_town2 = df_house_town2 %>%
  mutate(id_match = unlist(df_house_dist$nn),
         id_match_dist = unlist(df_house_dist$dist)) %>%
  left_join(df_house_new2, by = c("id_match", "year"))

df_
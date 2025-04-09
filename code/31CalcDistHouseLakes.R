library(dplyr)
library(sf)
library(tidyr)
library(purrr)
library(future)
library(progressr)
library(furrr)

df_canvec = readRDS("data/df_canvec.rds")
df_canvec_sd = readRDS("data/df_canvec_sd.rds")

df_canvec_lake <-  df_canvec %>%
  filter(water_type == "Lake" | water_type == "Reservoir")

df_sd_sdindex_latlon_year_lake = read.csv("data/df_sd_latlon_year_lake.csv")

df_house_latlon =  read.csv("data/HouseData/df_house_coordinates_final.csv") %>%
  filter(!is.na(latitude))

df_house_latlon_sf = df_house_latlon   %>%
  dplyr::select(roll_num, longitude, latitude) %>%
#  sample_n(10)  %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

df_house_latlon_sf$lake_index_canvec = st_nearest_feature(df_house_latlon_sf, df_canvec)
df_house_latlon_sf$df_canvec_lake_row = st_nearest_feature(df_house_latlon_sf, df_canvec_lake)
df_house_latlon_sf$df_canvec_sd_row = st_nearest_feature(df_house_latlon_sf, df_canvec_sd)

df_canvec_sd_index = as_tibble(df_canvec_sd) %>%
  select(lake_index_canvec_sd = lake_index_canvec) %>%
  mutate(df_canvec_sd_row = seq(1:nrow(.)))

df_canvec_lake_index = as_tibble(df_canvec_lake) %>%
  select(lake_index_canvec_lake = lake_index_canvec) %>%
  mutate(df_canvec_lake_row = seq(1:nrow(.)))

df_house_latlon_sf = df_house_latlon_sf %>%
  left_join(df_canvec_sd_index) %>%
  left_join(df_canvec_lake_index)

gc()

#df_export = df_house_latlon_sf
#df_export$house_lake_dist = st_distance(df_export, df_canvec[df_export$lake_index_canvec,], by_element=TRUE)

#df_export$house_lake_dist_sd = ifelse(df_export$lake_index_canvec == df_export$lake_index_canvec_sd,df_export$house_lake_dist, NA)

#x_missing = df_export %>% filter(is.na(house_lake_dist_sd))

#x_missing$house_lake_dist_sd = as.numeric(st_distance(x_missing, df_canvec_sd[x_missing$df_canvec_sd_row,], by_element=TRUE))

#df_export = df_export %>% filter(!is.na(house_lake_dist_sd)) %>%
#  bind_rows(x_missing) %>% select(-df_canvec_sd_row)


# randomizes rows to more equalize distance calcs
df_house_latlon_sf = df_house_latlon_sf[sample(nrow(df_house_latlon_sf)),]
#temp1 = df_house_latlon_sf[1:100,]
temp = split(df_house_latlon_sf, (seq(nrow(df_house_latlon_sf))-1) %/% 200000)
#temp = temp[1:10]


out <- vector("list", length = length(temp))

start = Sys.time()

for(i in 1:length(temp)){
  print(i)
  start1 = Sys.time()
  plan(multisession, workers = 6)
  temp1 = split(temp[[i]], (seq(nrow(temp[[i]]))-1) %/% 20) 
  
  with_progress({
    p <- progressor(steps = length(temp1))
    
    out[[i]] = future_map_dfr(temp1, function(x, 
                                              y = df_canvec, 
                                              z = df_canvec_sd,
                                              y_lake = df_canvec_lake){
      p()

      x$house_water_dist = as.numeric(st_distance(x, y[x$lake_index_canvec,], 
                                   by_element=TRUE))
      
      x$house_water_dist_sd = ifelse(x$lake_index_canvec == x$lake_index_canvec_sd,
                                    x$house_water_dist, NA)
      
      x_missing = x %>%
        filter(is.na(house_water_dist_sd))
      
      x_missing$house_water_dist_sd = as.numeric(st_distance(x_missing, z[x_missing$df_canvec_sd_row,], 
                                                by_element=TRUE))

      x = x %>%
        filter(!is.na(house_water_dist_sd)) %>%
        bind_rows(x_missing) %>%
        select(-df_canvec_sd_row)
      
      x$house_lake_dist = ifelse(y[x$lake_index_canvec,][['water_type']] == "Lake" | 
                                   y[x$lake_index_canvec,][['water_type']] =="Reservoir",
                                    x$house_water_dist, NA)     
      
      x_missing = x %>%
        filter(is.na(house_lake_dist))

      x_missing$house_lake_dist = as.numeric(st_distance(x_missing, y_lake[x_missing$lake_index_canvec_lake,], 
                                                         by_element=TRUE))
      
      x = x %>%
        filter(!is.na(house_lake_dist)) %>%
        bind_rows(x_missing) %>%
        select(-df_canvec_lake_row)
      
      return(x)
    }, .options = furrr_options(seed = TRUE, scheduling = 20))
  })
  closeAllConnections()
  saveRDS(out[[i]], paste0("data/df_house_wq_fresh4",i,".rds"))
  gc()
  end1 = Sys.time()
  print(end1 - start1)
}

end = Sys.time()
end - start
df_export = bind_rows(out)

df_export = as_tibble(df_export) %>%
  select(-geometry) %>%
  ungroup(.)

df_export2 = df_export %>%
  select(-contains("_index")) %>%
  pivot_longer(-c(roll_num), 
               values_to = "dist_house_lake",
               names_to = "name") %>%
  mutate(closest_type = ifelse(str_detect(name, "sd"), "water_sd", 
                        ifelse(str_detect(name, "lake"), "lake", "water"))) %>%
  select(-name)

df_export = df_export %>%
  select(-contains("_dist")) %>%
  pivot_longer(-c(roll_num), 
               values_to = "lake_index_canvec",
               names_to = "name") %>%
  mutate(closest_type = ifelse(str_detect(name, "_sd"), "water_sd", 
                               ifelse(str_detect(name, "_lake"), "lake", "water"))) %>%
  select(-name) %>%
  left_join(df_export2)

saveRDS(df_export, "data/df_house_lake_dist.rds")

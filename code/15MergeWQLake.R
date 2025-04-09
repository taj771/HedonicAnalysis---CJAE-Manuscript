
df_canvec = readRDS("data/df_canvec.rds")

df_study_area_filter = readRDS("data/df_study_area_filter.rds")

df_sd_sdindex_latlon_year = read_csv("data/sd_merged_data.csv") %>%
  filter(!is.na(sd_interp)) %>% 
  filter(!is.na(lat_lon))

df_sdlatlon = df_sd_sdindex_latlon_year %>%
  distinct(lake_index_sd, lake_name_sd, lat_lon) %>%
  mutate(lat = round(as.numeric(gsub( " .*$", "", lat_lon )), 4),
         lon = round(as.numeric(gsub( ".* ", "", lat_lon )), 4)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(lake_index_sd, lake_name_sd, lat_lon, geometry)

find_data <- sf::st_within(df_sdlatlon, df_study_area_filter)
df_sdlatlon_lake <- df_sdlatlon[which(lengths(find_data) != 0), ]

# TAKE TIME
df_sdlatlon_lake  = df_sdlatlon_lake # %>%
#  sample_n(10)
df_sdlatlon_lake$lake_index_canvec = st_nearest_feature(df_sdlatlon_lake, df_canvec)

# NEED to calc distance to shore for each station
#https://github.com/r-spatial/sf/issues/1290
#df_sdlatlon_lake$station_within_lake = st_within(df_sdlatlon_lake, 
#                                                 df_canvec[df_sdlatlon_lake$lake_index_canvec,], 
#                                                 by_element=TRUE)

df_sdlatlon_lake$dist = st_distance(df_sdlatlon_lake, 
                                    df_canvec[df_sdlatlon_lake$lake_index_canvec,], 
                                    by_element=TRUE)

df_sdlatlon_lake_dist = df_sdlatlon_lake

#df_lake_sd_distinct_keep = df_lake_sd_distinct
#df_house_latlon_sf

df_lake_canvec = tibble(lake_name_canvec = df_canvec$lake_name_canvec,
                        lake_area = df_canvec$lake_area_sqkm,
                        lake_index_canvec = df_canvec$lake_index_canvec)

df_sdlatlon_lake_index = df_sdlatlon_lake_dist %>%
  left_join(df_lake_canvec, by = "lake_index_canvec") %>%
  mutate(lake_name_canvec_lowercase = tolower(lake_name_canvec)) %>%
  tidy_stringdist(lake_name_sd, lake_name_canvec_lowercase, method = "jw") %>%
  mutate(dist = as.numeric(dist)) %>%
  mutate(lake_name_canvec_lowercase_nolake = trimws(str_remove(lake_name_canvec_lowercase, "lake")),
         lake_name_sd_nolake = trimws(str_remove(lake_name_sd, "lake"))) %>%
  rename(jw1 = jw) %>%
  tidy_stringdist(lake_name_sd_nolake, lake_name_canvec_lowercase_nolake, method = "jw") 

df_sdlatlon_lake_index_temp = df_sdlatlon_lake_index %>%
  filter(dist < 25 | jw1 < .25 | jw < .25 | 
           lake_name_sd == "dill lake (t lake)" |
           lake_name_sd == "neighick lake (beaver)" |
           lake_name_sd == "cranberry lake" |
           lake_name_sd == "little joe lake" |
           lake_name_sd == "kusk lake (rat)") %>%
  dplyr::select(lake_index_sd, lake_index_canvec)

df_sdlatlon_lake_index_remove = df_sdlatlon_lake_index %>%
  filter(!(lake_index_sd %in% df_sdlatlon_lake_index_temp$lake_index_sd))

# Check removal numbers
df_sdlatlon_lake_index_remove2 = df_sdlatlon_lake_index_remove %>%
  left_join(df_sd_sdindex_latlon_year, by = c("lake_name_sd", "lat_lon"))

df_lake_canvec = df_lake_canvec %>%
  mutate(index = ifelse(lake_index_canvec %in% df_sdlatlon_lake_index_temp$lake_index_canvec, 
                        1, 0)) %>%
  group_by(index) %>%
  mutate(lake_index_canvec_sd = row_number()) %>%
  mutate(lake_index_canvec_sd = ifelse(index == 0, NA, lake_index_canvec_sd))

find_data = as.list(df_lake_canvec$index)

df_lake_canvec = df_lake_canvec %>%
  ungroup(.) %>%
  dplyr::select(-index)

df_canvec_sd <- df_canvec[which(find_data != 0), ]

df_sd_sdindex_latlon_year_lake = df_sd_sdindex_latlon_year %>%
  left_join(df_sdlatlon_lake_index_temp, by = "lake_index_sd") %>%
  left_join(df_lake_canvec, by = "lake_index_canvec") %>%
  filter(!is.na(lake_index_canvec_sd)) %>%
  select(lake_index_sd, lat_lon, year, lake_index_canvec)

# ensure all houses have lake names
#df_sd_sdindex_latlon_year_join2 = df_sd_sdindex_latlon_year_join %>%
#  distinct(lake_index_canvec_sd, lake_name_sd, lake_index_sd)

write_csv(df_sd_sdindex_latlon_year_lake, "data/df_sd_latlon_year_lake.csv")

saveRDS(df_canvec_sd, "data/df_canvec_sd.rds")




minimum_lake_size = 0.04 # sq km 0.04 is 4 has

df_canvec = readRDS("C:/Users/pal453/OneDrive - University of Saskatchewan/Canvec/df_canvec.rds")

# minimum lake size
df_canvec = df_canvec %>%
  mutate(lake_area_sqkm = as.numeric(st_area(.))*1e-6) %>%
  filter(lake_area_sqkm > minimum_lake_size ) %>%
  mutate(lake_index_canvec = seq(1:nrow(.))) %>%
  rename(lake_name_canvec = name_en,
         water_type = definit_en)
  
#df_canvec = readRDS("data/df_canvec.rds")

df_canvec = st_as_sf(df_canvec) %>%  
  st_transform(4326, crs(CRS(4326)))

df_canvec_export = df_canvec  %>%
  select(lake_name_canvec, lake_area_sqkm, lake_index_canvec, water_type)

gc()

saveRDS(df_canvec_export, "data/df_canvec.rds")

centroid = st_centroid(df_canvec)

lat_lon = as_tibble(centroid$geometry) %>%
  mutate(geometry = str_remove(as.character(geometry), "[c(]")) %>%
  separate(geometry, into  = c("lon", "lat"), sep = ",") %>%
  mutate(lat = gsub("[^0-9.-]", "", lat),
         lon = gsub("[^0-9.-]", "", lon)) %>%
  unite("lat_lon", lat:lon, sep = ", ")
  
  

df_canvec_lake_index = as_tibble(df_canvec)  %>%
  select(-ends_with("fr")) %>%
  select(lake_name_canvec, lake_index_canvec,
         water_type, lake_area_sqkm) %>%
  mutate(lake_latlon_canvec = lat_lon$lat_lon)

summary(df_canvec_lake_index)

saveRDS(df_canvec_lake_index, "data/df_canvec_lake_index.rds")

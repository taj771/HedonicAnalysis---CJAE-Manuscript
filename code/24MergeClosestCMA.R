

# Download Statistics Canada data from Cansim by table name
#df = get_cansim(1710013501) %>% 
#  normalize_cansim_values() %>%
#  clean_names(.)

#table(df$geo)

#df = df %>%
#  filter(age_group == "All ages",
#         sex == "Both sexes") %>%
#  filter(str_detect(geo, "Ontario" )) %>%
#  select(geo, geo_uid, date, population = value) 


df_cma = st_read("data/lcma000b16a_e/lcma000b16a_e.shp") %>%
  filter(PRNAME == "Ontario") %>%
  filter(CMATYPE == "B") %>% # only include CMAs
  select(cma_name = CMANAME, geometry)

df_cma_sf = df_cma %>%
  st_transform(4326, crs(CRS(4326)))

df_cma = as_tibble(df_cma) %>%
  select(-geometry) %>%
  mutate(cma_id = paste0("V", seq(1:nrow(.))))

df_house = read_csv("data/HouseData/df_house_coordinates_final.csv")

df_house_sf = as_tibble(df_house)  %>%
  filter(!is.na(latitude)) %>%
  distinct(roll_num, longitude, latitude) %>%
  mutate(new_id = seq(1:nrow(.))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


plan(multisession, workers = 6)

temp = split(df_house_sf, (seq(nrow(df_house_sf))-1) %/% 20000) 
start = Sys.time()

with_progress({
  p <- progressor(steps = length(temp))
  
  out = future_map_dfr(temp, function(x, y = df_cma_sf){
    p()

    dist = as_tibble(st_distance(x,y)) %>%
      mutate(new_id = x$new_id) %>%
      pivot_longer(-new_id, values_to = "distance", names_to = "cma_id")
    
    df_cma_closest = dist %>%
      mutate(distance = as.numeric(distance)) %>%
      group_by(new_id) %>%
      slice_min(distance, n=1) %>%
      left_join(df_cma) %>%
      rename(distance_cma_metres = distance) %>%
      select(-cma_id) %>%
      rename(name_closest_cma = cma_name)
    
    df_toronto_closest = dist %>%
      mutate(distance = as.numeric(distance)) %>%
      left_join(df_cma) %>%
      filter(cma_name == "Toronto")%>%
      rename(distance_toronto_metres = distance) %>%
      select(-cma_id, -cma_name) %>%
      left_join(df_cma_closest)
    
    return(df_toronto_closest)
  }, .options = furrr_options(seed = TRUE, scheduling = 20))
})
closeAllConnections()
gc()
end = Sys.time()
end - start

df_export = as_tibble(df_house_sf) %>%
  select(-geometry) %>%
  left_join(out) %>%
  select(-new_id)

write_csv(df_export, "data/HouseData/cma_merge.csv")

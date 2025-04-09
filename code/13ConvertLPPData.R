

## Water quality data
## Import lake data_Secchi_depth (Lake_2)


df_sd <-read_csv("data/wq_data/Lake_Partners2.csv")

df_sd_latlon = read_csv("data/wq_data/Lake_Partners2_correctlatlon.csv") %>%
  clean_names(.) %>%
  distinct(lake_name, stn, site_id, latitude_dms, longitude_dms) %>%
  rename(lat_dms_new = latitude_dms,
         lon_dms_new = longitude_dms) %>%
  mutate(lon_dms_new = ifelse(lake_name == "EAGLE LAKE" & stn == 1299 & site_id == 5, 933259, lon_dms_new), # need to switch lat/lon
         lat_dms_new = ifelse(lake_name == "EAGLE LAKE" & stn == 1299 & site_id == 5, 494737, lat_dms_new))

df_sd = clean_names(df_sd) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         year = year(date)) %>%
  distinct(.) %>%
  filter(!is.na(year)) %>%
  mutate(latitude_dms = ifelse(lake_name == "ALLAN LAKE", 494130, latitude_dms),
         longitude_dms = ifelse(lake_name == "ALLAN LAKE", 824623, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 1, 450304, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 1, 793418, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 2, 450032, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 2, 793554, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 3, 450114, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 3, 792605, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 4, 445704, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 4, 792302, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 5, 450239, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 5, 792812, longitude_dms),  
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 6, 450252, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 6, 793648, longitude_dms),  
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 7, 450353, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 7, 793044, longitude_dms),  
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 8, 445520, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 8, 792310, longitude_dms),  
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 9, 445647, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 9, 792349, longitude_dms),  
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 10, 445512, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 10, 792315, longitude_dms),  
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 11, 450252, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 11, 793648, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 12, 445144, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 12, 792812, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 13, 450355, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 13, 793658, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 14, 450032, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 14, 793554, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 15, 445520, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 15, 792310, longitude_dms),
         latitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 16, 450355, latitude_dms),
         longitude_dms = ifelse(lake_name == "MUSKOKA LAKE" & site_id == 16, 793658, longitude_dms),
         latitude_dms = ifelse(lake_name == "LOWER BUCKHORN LAKE" & site_id == 5, 443259, latitude_dms),
         longitude_dms = ifelse(lake_name == "LOWER BUCKHORN LAKE" & site_id == 5, 781420, longitude_dms),         
         latitude_dms = ifelse(lake_name == "SIMCOE LAKE" & site_id == 7, 443317, latitude_dms),
         longitude_dms = ifelse(lake_name == "SIMCOE LAKE" & site_id == 7, 792022, longitude_dms),         
         latitude_dms = ifelse(lake_name == "HURON LAKE" & site_id == 7, 433411, latitude_dms),
         longitude_dms = ifelse(lake_name == "HURON LAKE" & site_id == 7, 814228, longitude_dms),
         latitude_dms = ifelse(lake_name == "HURON LAKE" & site_id == 5, 434449, latitude_dms),
         longitude_dms = ifelse(lake_name == "HURON LAKE" & site_id == 5, 814359, longitude_dms),
         site_id = ifelse(lake_name == "LOWER BUCKHORN LAKE" & site_id == 2, 8, site_id),         
         site_id = ifelse(lake_name == "FOURTEEN ISLAND LAKE" & site_id == 1, 2, site_id),
         site_id = ifelse(lake_name == "ST. CHARLES LAKE", 5, site_id),
         site_id = ifelse(lake_name == "PAPINEAU LAKE" & stn == 4240, 5, site_id),
         site_id = ifelse(lake_name == "STURGEON LAKE" & site_id == 6, 9, site_id)) %>%
  filter(!(lake_name == "STORMY LAKE" & site_id == 4)) %>%
  left_join(df_sd_latlon, by = c("stn", "site_id", "lake_name")) %>%
  mutate(longitude_dms = ifelse(!is.na(lon_dms_new), lon_dms_new, longitude_dms),
         latitude_dms = ifelse(!is.na(lat_dms_new), lat_dms_new, latitude_dms),
         latitude_dms = ifelse(lake_name == "LORIMER LAKE" & site_id < 4, 453313, latitude_dms),
         longitude_dms = ifelse(lake_name == "LORIMER LAKE" & site_id < 4, 795645, longitude_dms),              
         latitude_dms = ifelse(lake_name == "LORIMER LAKE" & site_id == 5, 453243, latitude_dms),
         longitude_dms = ifelse(lake_name == "LORIMER LAKE" & site_id == 5, 795752, longitude_dms), 
         latitude_dms = ifelse(lake_name == "LOWER BUCKHORN LAKE" & site_id == 7, 443133 , latitude_dms),
         longitude_dms = ifelse(lake_name == "LOWER BUCKHORN LAKE" & site_id == 7, 781623, longitude_dms),         
         latitude_dms = ifelse(lake_name == "LOWER BUCKHORN LAKE" & site_id == 8, 443338 , latitude_dms),
         longitude_dms = ifelse(lake_name == "LOWER BUCKHORN LAKE" & site_id == 8, 781647, longitude_dms))


df_sd_new = read_csv("data/wq_data/Lake_Partners2022_09_26.csv") %>%
  clean_names(.)  %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         year = year(date),
         latitude_dms = latitude,
         longitude_dms = longitude,
         secchi_depth_metres = secchi_depth_m) %>%
  distinct(.) %>%
  filter(!is.na(year))

df_sd = df_sd_new

# Should we include site_id as unique grouping
df_sd2 = df_sd %>%
  ungroup(.) %>%
  group_by(stn, latitude_dms, longitude_dms, year) %>%
  summarise(wq_mean = mean(secchi_depth_metres),
                   sd_min = min(secchi_depth_metres),
                   sd_max = max(secchi_depth_metres),
                   n_readings = n())


df_sd_export = df_sd %>%
  distinct(lake_name, district_county, stn, latitude_dms, longitude_dms, year) %>%
  left_join(df_sd2, by = c("stn", "latitude_dms", "longitude_dms", "year"))  %>%
  filter(!is.na(longitude_dms), !is.na(latitude_dms)) %>%
#  filter(year > 2001, year < 2017) %>%
  mutate(longitude_dms = as.character(as.integer(longitude_dms)),
         latitude_dms = as.character(as.integer(latitude_dms))) %>%
#   filter(stn < 200) %>%
  mutate(longitude_dms = paste0(substr(longitude_dms,1,2),":",substr(longitude_dms,3,4),":",substr(longitude_dms,5,6)),
         latitude_dms = paste0(substr(latitude_dms,1,2),":",substr(latitude_dms,3,4),":",substr(latitude_dms,5,6)),
         lon = -parse_lon(longitude_dms),
         lat = parse_lat(latitude_dms),
         source = "lpp") %>%
  select(-longitude_dms, -latitude_dms)
                        
write_csv(df_sd_export, "data/df_wq_lpp_sd.csv")

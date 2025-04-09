

df_huron = read_csv("D:/Data/LAKE_HURON_Water_Quality_2000-present.csv") 
df_ontario = read_csv("D:/Data/LAKE_ONTARIO_Water_Quality_2000-present.csv") 
df_erie = read_csv("D:/Data/LAKE_ERIE_Water_Quality_2000-present.csv") 
df_superior = read_csv("D:/Data/LAKE_SUPERIOR_Water_Quality_2000-present.csv") 
df_georgian = read_csv("D:/Data/GEORGIAN_BAY_Water_Quality_2000-present.csv") 

df_gl = bind_rows(df_huron, df_ontario, df_erie, df_superior, df_georgian) %>%
  clean_names(.) %>%
  filter(abbrev == "SECCHI") %>%
  mutate(lake_name = tolower(paste0(str_remove(water_body, "LAKE "), " lake")),
         year = year(stn_date),
         lat_lon = paste0(latitude_dd, " ", longitude_dd)) %>%
  group_by(year, lake_name, lat_lon) %>%
  summarise(sd_average = mean(value),
            n_readings = n())  %>%
  mutate(source = "eccc gl wq") %>%
  dplyr::select(lake_name, year, lat_lon, sd_average, n_readings, source)

write_csv(df_gl, "data/df_gl.csv")

xl_data = "data/GLIS_WATER_CHEMISTRY.xlsx"

tab_names <- excel_sheets(path = xl_data)

list_all <- lapply(tab_names, function(x) read_excel(path = xl_data, sheet = x))
list_all <- lapply(list_all, function(x){
  x = clean_names(x) %>%
    distinct(body_of_water, collect_date, station_description, latitude_in_dec, longitude_in_dec, secchi_depth)
  })

df_ontario_moe = bind_rows(list_all) %>%
  mutate(secchi_depth = as.numeric(secchi_depth),
         year = year(collect_date),
         lat_lon = paste0(latitude_in_dec, " ", longitude_in_dec),
         lake_name = tolower(paste0(body_of_water, ": ", station_description)),
         lake_name = ifelse(lake_name == "lake ontario: trenton pdgps",
                            "bay of quinte: trenton pdgps", lake_name)) %>%
  filter(!is.na(secchi_depth)) %>%
  group_by(lake_name, lat_lon, year) %>%
  summarise(sd_average = mean(secchi_depth),
            n_readings = n())  %>%
  mutate(source = "ontario moe gln") %>%
  dplyr::select(lake_name, year, lat_lon, sd_average, n_readings, source)

write_csv(df_ontario_moe, "data/df_ontario_moe.csv")



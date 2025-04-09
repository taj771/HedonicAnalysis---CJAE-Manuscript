

df_municipal = read.csv("data/HouseData/municipal_codes.csv") %>%
  clean_names(.) %>%
  distinct(upper_tier_municipality, official_municipal_name, 
           municipal_name_shortform, assessment_code)  %>%
  mutate(county = ifelse(upper_tier_municipality == "", official_municipal_name, upper_tier_municipality))

df_house_coordinates = read_csv("data/HouseData/df_house_coordinates_missing.csv")

df_house = readRDS("data/df_house.rds")

df = df_house %>%
  distinct(roll_num, address) %>%
  left_join(df_house_coordinates) %>%
  select(roll_num, address, latitude, longitude) %>%
  mutate(county2 = substr(roll_num, 1, 2),
         city = str_sub(roll_num, 3, 4), 
         ward = str_sub(roll_num, 5, 7),
         area = str_sub(roll_num, 8, 10),
         street = str_sub(roll_num, 11, 15),
         assessment_code = as.integer(substr(roll_num, 1, 4))) %>%
  left_join(df_municipal)


df_missing = df %>%
  filter(is.na(latitude)) %>%
  select(roll_num, address, municipal_name_shortform)

df_missing_ggmap = df_missing %>%
  filter(address != "", address != "0") %>%
  distinct(roll_num, .keep_all = T) %>%
  mutate(gg_address = paste0(address, ", ", municipal_name_shortform, ", Ontario")) %>%
  rename(address_old = address)


# Register Google API key
register_google(key = "AIzaSyD6BVXB-T-1qw7sqtzewcl5KPjb5wHoYfQ")
geocode("1301 S University Parks Dr, Waco, TX 76798")

# Wrap mapdist in `safely` so that it doesn't fail when query comes back empty
safe_mutate_geocode <- safely(mutate_geocode)

df_missing_ggmap.geo = safe_mutate_geocode(df_missing_ggmap, 
                                           location = gg_address, 
                                           output = "latlona")
df_missing_ggmap.geo1 = df_missing_ggmap.geo[[1]]

write_csv(df_missing_ggmap.geo1, "data/HouseData/HouseData/ggmap_missinglatlon_house_all.csv")
df_missing_ggmap.geo1 = read_csv("data/HouseData/ggmap_missinglatlon_house_all.csv")

df_errors_us = df_missing_ggmap.geo1 %>%
  filter(address == "us-2, united states") %>%
  mutate(address2 = str_extract(address_old, "[0-9]+")) %>%
  filter(address2 != "2") %>%
  mutate(address2 = paste0(address2, 
                           " Stormont, Dundas and Glengarry County Road 2, ontario")) %>%
  select(roll_num, address2)

df_missing_ggmap.us = safe_mutate_geocode(df_errors_us, 
                                          location = address2, 
                                          output = "latlona")

df_missing_ggmap.geo2 = df_missing_ggmap.geo1  %>%
  filter(str_detect(address, "on, canada"))

df_missing_ggmap.geo1 = df_missing_ggmap.geo1  %>%
  filter(!str_detect(address, "on, canada")) %>%
  filter(address != "us-2, united states") %>%
  bind_rows(df_errors_us) %>%
  select(roll_num, year, lat, lon, gg_address = address)

write_csv(df_missing_ggmap.geo1, "data/HouseData/ggmap_missinglatlon_house_correct.csv")

df_missing_found = read_csv("data/HouseData/ggmap_missinglatlon_house_correct.csv") %>%
  select(roll_num, latitude = lat, longitude = lon)

df_missing = df %>%
  filter(is.na(latitude)) %>%
  select(-latitude, -longitude) %>%
  left_join(df_missing_found) #%>%
#  filter(!is.na(latitude))

df_export = df %>%
  filter(!is.na(latitude)) %>%
  bind_rows(df_missing)  %>%
  distinct(roll_num, latitude, longitude)


write_csv(df_export, "data/HouseData/df_house_coordinates_final.csv")


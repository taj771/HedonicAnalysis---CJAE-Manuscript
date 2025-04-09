


# Cleans environment
rm(list=ls(all=TRUE))

library(tidyverse)
library(haven)
library(janitor)

df_municipal = read.csv("data/HouseData/municipal_codes.csv") %>%
  clean_names(.) %>%
  distinct(upper_tier_municipality, official_municipal_name, 
           municipal_name_shortform, assessment_code)  %>%
  mutate(county = ifelse(upper_tier_municipality == "", official_municipal_name, upper_tier_municipality))


df_house = readRDS("data/df_house.rds")

df_house_stata = read_dta("data/HouseData/Rural res sales - 2002-16.dta")

df_house_old = read_csv("data/HouseData/rural_sales_0216.csv") %>%
  select(sold_year, sale_amt, county, township)

df = df_house_stata %>%
  select(roll_num, address, latitude, longitude, ward, pac) %>%
  bind_cols(df_house_old) %>%
  mutate(county2 = substr(roll_num, 1, 2),
         city = str_sub(roll_num, 3, 4), 
         ward = str_sub(roll_num, 5, 7),
         area = str_sub(roll_num, 8, 10),
         street = str_sub(roll_num, 11, 15),
         assessment_code = as.integer(substr(roll_num, 1, 4))) %>%
  left_join(df_municipal)


df_missing = df_house %>%
  filter(is.na(latitude)) %>%
  select(roll_num, year, address, propcode) %>%
  mutate(county2 = substr(roll_num, 1, 2),
         city = str_sub(roll_num, 3, 4), 
         ward = str_sub(roll_num, 5, 7),
         area = str_sub(roll_num, 8, 10),
         street = str_sub(roll_num, 11, 15),
         assessment_code = as.integer(substr(roll_num, 1, 4))) %>%
  left_join(df_municipal)


df_test21 = df_house %>%
  filter(!is.na(latitude), 
         !is.na(longitude)) %>%
  ungroup(.) %>%
  group_by(#county, township, 
    county2, city, ward, area) %>% #, ward, area) %>%
  summarise(latitude_mean = mean(latitude),
            lat_max = max(latitude),
            lat_min = min(latitude),
            lon_max = max(longitude),
            lon_min = min(longitude),
            lat_diff = lat_max-lat_min,
            lon_diff = lon_max-lon_min,
            longitude_mean = mean(longitude),
            count = n()) %>%
  mutate(test= 1)

df_missing_ggmap = df_missing %>%
  filter(address != "", address != "0") %>%
#  filter(#propcode == 301 | propcode == 313 | propcode == 391 |
        #   propcode == 392 | propcode == 395) %>%
  distinct(roll_num, .keep_all = T)

df_missing_ggmap = df_missing_ggmap %>%
  mutate(gg_address = paste0(address, ", ", municipal_name_shortform, ", Ontario")) %>%
  rename(address_old = address)
  
library(ggmap)

# Register Google API key
register_google(key = "AIzaSyD6BVXB-T-1qw7sqtzewcl5KPjb5wHoYfQ")
geocode("1301 S University Parks Dr, Waco, TX 76798")

# Wrap mapdist in `safely` so that it doesn't fail when query comes back empty
safe_mutate_geocode <- safely(mutate_geocode)

df_missing_ggmap.geo = safe_mutate_geocode(df_missing_ggmap, 
                                      location = gg_address, 
                                      output = "latlona")
df_missing_ggmap.geo1 = df_missing_ggmap.geo[[1]]

write_csv(df_missing_ggmap.geo1, "data/missinglatlon_house_all.csv")
df_missing_ggmap.geo1 = read_csv("data/missinglatlon_house_all.csv")

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

write_csv(df_missing_ggmap.geo1, "data/missinglatlon_house_correct.csv")

df_missing_found = read_csv("data/missinglatlon_house_correct.csv") %>%
  select(roll_num, latitude = lat, longitude = lon)

df_missing = df_house %>%
  filter(is.na(latitude)) %>%
  select(-latitude, -longitude) %>%
  left_join(df_missing_found) %>%
  filter(!is.na(latitude))

df_house = df_house %>%
  filter(!is.na(latitude)) %>%
  bind_rows(df_missing)


saveRDS(df_house, "data/df_house.rds")



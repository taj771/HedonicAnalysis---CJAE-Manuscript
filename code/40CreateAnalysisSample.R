
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  sf,
  stringr, 
  tidyr, 
  janitor,
  lubridate
)



# see list of potential MPAC variables
# need to include in 21CleanHousingData.R script if want to include
var.labels = readRDS("data/HouseData/house_02_16_variable_key.rds")



# check out palgrave sd_dist, Innis lake. Need correct 
# https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getCET_Page&VD=318020&Item=314459
nipissing = c("TOWNSHIP OF BONFIELD", 
              "TOWNSHIP OF CALVIN", 
              "TOWNSHIP OF CHISHOLM", 
              "TOWNSHIP OF MATTAWAN",
              "TOWNSHIP OF PAPINEAU-CAMERON",
              "MUNICIPALITY OF TEMAGAMI",
              "THE CORPORATION OF THE MUNICIPALITY OF EAST FERRIS",
              "MUNICIPALITY OF WEST NIPISSING")


east = c("UNITED COUNTIES OF PRESCOTT AND RUSSELL",
         "UNITED COUNTIES OF LEEDS AND GRENVILLE",
         "UNITED COUNTIES OF STORMONT, DUNDAS AND GLENGARRY",
         "CITY OF BROCKVILLE",
         "CITY OF CORNWALL")

region_lakes = c("CITY OF KAWARTHA LAKES", "NIPISSING DISTRICT", 
          "DISTRICT MUNICIPALITY OF MUSKOKA", "COUNTY OF SIMCOE",
          "CITY OF ORILLIA", "COUNTY OF PETERBOROUGH")


#Choose interpolation limts
 interp_lo = -5
 interp_hi = 5

df_house_station = readRDS("data/df_house_station.rds") 
df_canvec_lake_index = readRDS("data/df_canvec_lake_index.rds")

df_sd = read.csv("data/sd_merged_data.csv") %>%
  filter(!is.na(sd_interp)) %>% 
  filter(!is.na(lat_lon)) %>%
  rename(station_lat_lon = lat_lon) %>%
  distinct(year, station_lat_lon, .keep_all = T)  %>%
  mutate(sd_extrap_years = str_remove(interpolation, "year"),
         sd_extrap_years = replace_na(as.numeric(sd_extrap_years), 0),
         sd_interp = ifelse(sd_extrap_years > interp_hi | sd_extrap_years < interp_lo, NA, sd_interp)) 

df_house = readRDS("data/df_house.rds") 

## Import Housing price index (HPI) data (Base year 2020)
hpi_year = "2020-12-01"


df_hpi = read.csv("data/HouseData/house_price_index_ontario.csv")

df_hpi = df_hpi %>%  ## Do we need this as above defined date format is similar
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
  select(date, hpi)

hpi_year = df_hpi %>%
  filter(date == hpi_year)

df_hpi = df_hpi %>%
  mutate(hpi = hpi_year$hpi / hpi )
  

df_cma_dist = read.csv("data/HouseData/cma_merge.csv")

df_municipal = read.csv("data/HouseData/municipal_codes.csv") %>%
  clean_names(.) %>%
  distinct(upper_tier_municipality, official_municipal_name, 
           municipal_name_shortform, assessment_code) %>%
  mutate(county = ifelse(upper_tier_municipality == "", 
                         official_municipal_name, 
                         upper_tier_municipality),
         region = case_when(county %in% nipissing ~ "NIPISSING DISTRICT", 
                            county == "CITY OF ORILLIA" ~ "COUNTY OF SIMCOE",
                            county == "TOWN OF SMITHS FALLS" | 
                              county == "CITY OF OTTAWA"~ "COUNTY OF LANARK",
                            county == "TOWNSHIP OF PELEE" ~  "COUNTY OF ESSEX",
                            county == "CITY OF KINGSTON" ~  "COUNTY OF FRONTENAC",
                            county == "CITY OF PEMBROKE" ~  "COUNTY OF RENFREW",
                            county == "CITY OF BELLEVILLE" |
                              county == "CITY OF QUINTE WEST" ~  "COUNTY OF HASTINGS",
                            county == "REGIONAL MUNICIPALITY OF PEEL" |
                              county == "REGIONAL MUNICIPALITY OF HALTON" |
                              county == "CITY OF HAMILTON" |
                              county == "REGIONAL MUNICIPALITY OF YORK" ~ "PEEL-YORK-HALTON-HAMILTON",
                            county == "COUNTY OF MIDDLESEX" |
                              county == "COUNTY OF LAMBTON" ~ "LAMBTON-MIDDLESEX",
                            county == "COUNTY OF PERTH" |
                              county == "COUNTY OF OXFORD" |
                              county == "REGIONAL MUNICIPALITY OF WATERLOO" |
                              county == "COUNTY OF BRANT" ~ "PERTH-OXFORD-BRANT-WATERLOO",
                            county == "COUNTY OF DUFFERIN" |
                              county == "COUNTY OF WELLINGTON" ~ "DUFFERIN-WELLINGTON",
                            county %in% east ~ "EASTERN",
                            TRUE ~ county),
         economic_region = case_when(region == "COUNTY OF LANARK" |
                                       region == "EASTERN" ~ "OTTAWA",   
                                     region == "COUNTY OF HASTINGS" |
                                       region == "COUNTY OF RENFREW" |
                                       region == "COUNTY OF PRINCE EDWARD" |
                                       region == "COUNTY OF LENNOX AND ADDINGTON" |
                                       region == "COUNTY OF FRONTENAC" ~ "KINGSTON-PEMBROKE",   
                                     region == "CITY OF KAWARTHA LAKES" |
                                       region == "COUNTY OF PETERBOROUGH" |
                                       region == "COUNTY OF NORTHUMBERLAND" |
                                       region == "DISTRICT MUNICIPALITY OF MUSKOKA" ~ "MUSKOKA-KAWARTHAS",   
                                     region == "REGIONAL MUNICIPALITY OF DURHAM" | 
                                     county == "REGIONAL MUNICIPALITY OF PEEL" |
                                       county == "REGIONAL MUNICIPALITY OF HALTON" |
                                       county == "REGIONAL MUNICIPALITY OF YORK" ~ "TORONTO",   
                                     region == "DUFFERIN-WELLINGTON" | 
                                       county == "REGIONAL MUNICIPALITY OF WATERLOO" |
                                       region == "COUNTY OF SIMCOE" ~ "KITCHENER-WATERLOO-BARRIE",   
                                     county == "CITY OF HAMILTON" |
                                     region == "REGIONAL MUNICIPALITY OF NIAGARA" | 
                                       region == "NORFOLK COUNTY" |
                                       region == "HALDIMAND COUNTY" |
                                       county == "COUNTY OF BRANT" ~ "HAMILTON-NIAGARA",   
                                     county == "COUNTY OF OXFORD" |
                                     county == "COUNTY OF ELGIN" |
                                       county == "COUNTY OF MIDDLESEX" ~ "LONDON",   
                                     county == "MUNICIPALITY OF CHATHAM-KENT" |
                                       county == "COUNTY OF LAMBTON" |
                                       region == "COUNTY OF ESSEX" ~ "WINDSOR-SARNIA",   
                                     county == "COUNTY OF PERTH" |
                                       county == "COUNTY OF GREY" |
                                       county == "COUNTY OF HURON" |
                                       county == "COUNTY OF BRUCE" ~ "STRATFORD-BRUCE",   
                                     region == "NIPISSING DISTRICT" ~ "NIPISSING",
                                     TRUE ~ NA_character_),
         region_lakes = ifelse(county %in% region_lakes | region == 
                                 "NIPISSING DISTRICT", 1, 0)) %>%
  select(assessment_code, economic_region, region, county, township = municipal_name_shortform, region_lakes)

df_subset = df_house_station %>%
  filter(closest_type == "water", data_type == "station") %>%
  left_join(df_sd) %>%
  left_join(df_house, by = c("roll_num", "year")) %>%
  filter(!is.na(sale_amt)) %>%
#  filter(lake_name_sd != "palgrave mill pond")  %>%
  filter(!(between(propcode, 200, 300))) %>% # exclude farming operations
  filter(!(between(propcode, 400, 600))) %>% # exclude commercial and institutional
  filter(propcode != 383) %>% # exclude bed and breakfasts
  filter(propcode != 130,# exclude non-buildable land
         propcode != 102, # conservation authority
         propcode != 101, # vacant institutional
         propcode != 103, # parks
         propcode != 111, # island
         propcode != 134 #Land designated and zoned for open space
  ) %>%
  filter(saletype == "" | saletype == "0" | saletype == "00" |
           saletype == "02" | #forced sale
           saletype == "03" | #quitclaim
           saletype == "04" # spec sale
  )%>%
  filter(year < 2021) 

x_replace = c("fireplcs", "area_tot",
              "baths", "bedrooms", "storeys",
              "quality")

great_lakes_name = c("Lake Huron","Lake Superior","Lake Ontario")
non_gl_lake_name = c("Lake Simcoe","Lake Nipissing","Lake Abitibi")
size_cutoff_sqkm = 200 #

df_greatlakes = df_canvec_lake_index %>%
  mutate(great_lake = ifelse(lake_name_canvec %in% great_lakes_name |
                               lake_area_sqkm > size_cutoff_sqkm, 1, 0),
         great_lake = ifelse(great_lake == 1 & lake_name_canvec %in% non_gl_lake_name, 0,
                             great_lake))

df_subset = df_subset %>%
  mutate(prop_type = case_when(between(propcode, 301, 305) |
                                 propcode == 313 |propcode == 314 ~ "detached_house",
                               propcode == 311 | propcode == 322 ~ "semi_detached_house",       
                               propcode == 309 |propcode == 350| propcode == 352 ~ "townhouse_house",         
                               propcode == 312 | propcode == 340 | propcode == 341 | 
                                 propcode == 306 | between(propcode, 332, 336) ~ "multi_house", 
                               propcode == 381 | propcode == 382 ~ "mobile_home", 
                               between(propcode, 391, 395) ~ "seasonal_prop",
                               propcode == 100 |propcode == 125 | propcode == 127 |
                                 propcode == 169 |propcode == 110 | propcode == 112 ~ "vacant_lot",
                               TRUE ~ NA_character_),
         house = ifelse(prop_type == "detached_house" | prop_type == "semi_detached_house" |   
                        prop_type == "seasonal_prop", 1, 0)) %>%
  mutate(across(all_of(x_replace), ~ifelse(prop_type == "vacant_lot", replace_na(., 0), .))) %>%
  rename(price_nominal = sale_amt)   %>%
  left_join(df_hpi, by = "date") %>%
  mutate(price_real = price_nominal * hpi) %>%
  select(-hpi) %>%
  group_by(closest_type, data_type)  %>%
  ungroup(.) %>%
  mutate(age = year - yr_blt,
         renovation_effect = ifelse(prop_type == "vacant_lot", 0, year - yrblteff),
         lbasement_area = ifelse(bsmtarea == 0, 0, log(bsmtarea)),
         lbasement_area = replace_na(lbasement_area, 0),
         sale_id = 1:n(),
         county2 = substr(roll_num, 1, 2),
         city = str_sub(roll_num, 3, 4), 
         ward = str_sub(roll_num, 5, 7),
         area = str_sub(roll_num, 8, 10),
         street = str_sub(roll_num, 11, 15),
         street2= str_sub(roll_num, 16, 19),
         total_pool = pooliara + pooloara,
         air_cond = ifelse(aircond == "Y", 1, 0),
         heat_elect = ifelse(heattype == "EL", 1, 0),
         heat_forcedair = ifelse(heattype == "FA", 1, 0),
         heat_hw = ifelse(heattype == "HW", 1, 0),
         pool = ifelse(total_pool > 0 , 1 ,0),
         fireplcs = replace_na(fireplcs, 0),
         age = ifelse(is.na(yr_blt), 0, year - yr_blt)) %>%
  distinct(roll_num, year, closest_type, data_type, .keep_all = T) %>%
  mutate(assessment_code = as.integer(paste0(county2, city))) %>%
  mutate(roll_num = as.numeric(roll_num))%>%
  left_join(df_municipal) %>%
  left_join(df_cma_dist)  %>%
  left_join(df_canvec_lake_index) %>%
  left_join(df_greatlakes)

df_subset = df_subset %>%
  mutate(sale_month = month(date),
         #waterfront = ifelse(wfront_l == 1, 1, 0),
         waterfront = ifelse(wfront_l == 1 & dist_house_lake <= 250, 1, 0),
         log_sd = ifelse(is.na(sd_average), 0, log(sd_average)),
         log_sd_inter = ifelse(is.na(sd_interp), 0, log(sd_interp)),
         dist_200m = ifelse(dist_house_lake <= 200, 1, 0),
         dist_250m = ifelse(dist_house_lake <= 250, 1, 0),
         dist_300m = ifelse(dist_house_lake <= 300, 1, 0),
         dist_400m = ifelse(dist_house_lake <= 400, 1, 0),
         dist_500m = ifelse(dist_house_lake <= 500, 1, 0),
         dist_1000m = ifelse(dist_house_lake <= 1000, 1, 0),
         dist_2000m = ifelse(dist_house_lake <= 2000, 1, 0),
         dist_250m_500m = ifelse(between(dist_house_lake, 250,500),1,0),
         dist_0m_100m = ifelse(between(dist_house_lake, 0,100),1,0),
         dist_100m_200m = ifelse(between(dist_house_lake, 100,200),1,0),
         dist_200m_300m = ifelse(between(dist_house_lake, 200,300),1,0),
         dist_300m_400m = ifelse(between(dist_house_lake, 300,400),1,0),
         dist_400m_500m = ifelse(between(dist_house_lake, 400,500),1,0),
         dist_500m_1000m = ifelse(between(dist_house_lake, 500,1000),1,0),
         rob_dist_0_50m = ifelse(between(dist_house_lake, 0, 50),1,0),
         rob_dist_50_200m = ifelse(between(dist_house_lake, 50, 200),1,0),
         rob_dist_0_100m = ifelse(between(dist_house_lake, 0, 100),1,0),
         rob_dist_100_200m = ifelse(between(dist_house_lake, 100, 200),1,0),
         rob_dist_100_300m = ifelse(between(dist_house_lake, 100, 300),1,0),
         rob_dist_0_200m = ifelse(between(dist_house_lake, 0, 200),1,0),
         rob_dist_200_500m = ifelse(between(dist_house_lake, 200, 500),1,0),
         rob_dist_0_300m = ifelse(between(dist_house_lake, 0, 300),1,0),
         rob_dist_300_1000m = ifelse(between(dist_house_lake, 300, 1000),1,0),
         dist_250m_wf_exclude = ifelse(waterfront ==0 & dist_house_lake <= 250,1,0),
         log_sd_front = ifelse(waterfront == 1, log(sd_average), 0),
         log_sd_200 = ifelse(dist_200m == 1, log(sd_average), 0),
         log_sd_250 = ifelse(dist_250m == 1, log(sd_average), 0),
         log_sd_300 = ifelse(dist_300m == 1, log(sd_average), 0),
         log_sd_400 = ifelse(dist_400m == 1, log(sd_average), 0),
         log_sd_250_500 = ifelse(dist_250m_500m == 1, log(sd_average), 0),
         log_sd_500 = ifelse(dist_500m == 1, log(sd_average), 0),
         log_sd_1000 = ifelse(dist_1000m == 1, log(sd_average), 0),
         log_sd_2000 = ifelse(dist_2000m == 1, log(sd_average), 0),
         log_sd_inter_gl = log_sd_inter*great_lake,
         log_sd_inter_gl_no = log_sd_inter*(1-great_lake),
         log_sd_250_gl = log_sd_250*great_lake,
         log_sd_250_gl_no = log_sd_250*(1-great_lake),
         log_sd_250_500_gl = log_sd_250_500*great_lake,
         log_sd_250_500_gl_no = log_sd_250_500*(1-great_lake),
         log_sd_500_gl = log_sd_500*great_lake,
         log_sd_500_gl_no = log_sd_500*(1-great_lake),
         log_sd_1000_gl = log_sd_1000*great_lake,
         log_sd_1000_gl_no = log_sd_1000*(1-great_lake),
         log_sd_2000_gl = log_sd_2000*great_lake,
         log_sd_2000_gl_no = log_sd_2000*(1-great_lake),
         log_sd_250_area = log_sd_250*log(lake_area_sqkm),
         log_sd_500_area = log_sd_500*log(lake_area_sqkm),
         log_sd_1000_area = log_sd_1000*log(lake_area_sqkm),
         log_sd_2000_area = log_sd_2000*log(lake_area_sqkm),
         log_sd_inter_front = ifelse(waterfront ==1, log(sd_interp),0),
         log_sd_inter_200 = ifelse(dist_200m == 1, log(sd_interp), 0),
         log_sd_inter_250 = ifelse(dist_250m == 1, log(sd_interp), 0),
         log_sd_inter_300 = ifelse(dist_300m == 1, log(sd_interp), 0),
         log_sd_inter_400 = ifelse(dist_400m == 1, log(sd_interp), 0),
         log_sd_inter_250_500 = ifelse(dist_250m_500m == 1, log(sd_interp), 0),
         log_sd_inter_500 = ifelse(dist_500m == 1, log(sd_interp), 0),
         log_sd_inter_1000 = ifelse(dist_1000m == 1, log(sd_interp), 0),
         log_sd_inter_2000 = ifelse(dist_2000m == 1, log(sd_interp), 0),
         log_sd_inter_100 = ifelse(dist_0m_100m ==1, log(sd_interp), 0),
         log_sd_inter_100_200 = ifelse(dist_100m_200m ==1, log(sd_interp), 0),
         log_sd_inter_200_300 = ifelse(dist_200m_300m ==1, log(sd_interp), 0),
         log_sd_inter_300_400 = ifelse(dist_300m_400m ==1, log(sd_interp), 0),
         log_sd_inter_400_500 = ifelse(dist_400m_500m ==1, log(sd_interp), 0),
         log_sd_inter_500_1000 = ifelse(dist_500m_1000m ==1, log(sd_interp), 0),
         rob_log_sd_inter_0_50 = ifelse(rob_dist_0_50m ==1, log(sd_interp), 0),
         rob_log_sd_inter_50_200 = ifelse(rob_dist_50_200m ==1, log(sd_interp), 0),
         rob_log_sd_inter_0_100 = ifelse(rob_dist_0_100m ==1, log(sd_interp), 0),
         rob_log_sd_inter_100_200 = ifelse(rob_dist_100_200m ==1, log(sd_interp), 0),
         rob_log_sd_inter_100_300 = ifelse(rob_dist_100_300m ==1, log(sd_interp), 0),
         rob_log_sd_inter_0_200 = ifelse(rob_dist_0_200m ==1, log(sd_interp), 0),
         rob_log_sd_inter_200_500 = ifelse(rob_dist_200_500m ==1, log(sd_interp), 0),
         rob_log_sd_inter_0_300 = ifelse(rob_dist_0_300m ==1, log(sd_interp), 0),
         rob_log_sd_inter_300_1000 = ifelse(rob_dist_300_1000m ==1, log(sd_interp), 0),
         log_sd_inter_250_gl = log_sd_inter_250*great_lake,
         log_sd_inter_250_gl_no = log_sd_inter_250*(1-great_lake),
         log_sd_inter_250_500_gl = log_sd_inter_250_500*great_lake,
         log_sd_inter_250_500_gl_no = log_sd_inter_250_500*(1-great_lake),
         log_sd_inter_500_gl = log_sd_inter_500*great_lake,
         log_sd_inter_500_gl_no = log_sd_inter_500*(1-great_lake),
         log_sd_inter_1000_gl = log_sd_inter_1000*great_lake,
         log_sd_inter_1000_gl_no = log_sd_inter_1000*(1-great_lake),
         log_sd_inter_2000_gl = log_sd_inter_2000*great_lake,
         log_sd_inter_2000_gl_no = log_sd_inter_2000*(1-great_lake),
         log_sd_inter_250_area = log_sd_inter_250*log(lake_area_sqkm),
         log_sd_inter_500_area = log_sd_inter_500*log(lake_area_sqkm),
         log_sd_inter_1000_area = log_sd_inter_1000*log(lake_area_sqkm),
         log_sd_inter_2000_area = log_sd_inter_2000*log(lake_area_sqkm),
         log_sd_inter_250_wf_ex = ifelse(dist_250m_wf_exclude == 1, log(sd_interp), 0),
         log_sd_inter_waterfront = log_sd_inter*waterfront,
         log_dist_toronto = log(distance_toronto_metres+1),
         log_dist_cma = log(distance_cma_metres+1),
         log_area_tot = log(area_tot+1),
         log_lakearea = log(lake_area_sqkm),
         subset = paste0(closest_type, "-", data_type)) 

saveRDS(df_subset, "data/analysis_sample.rds")


# re-estimate distance to gta variable
# check out palgrave sd_dist, Innis lake. Need correct 

rm(list=ls(all=TRUE))

library(fixest)
library(tidyverse)

df_house_wq = readRDS("data/df_house_wq.rds")
df_house = readRDS("data/df_house.rds")


df_house_wq = df_house_wq %>%
  select(sale_id, year, lake_name_sd, lake_name_canvec, lake_dist_sd, sd_interp, 
         n_readings, source, interpolation, sd_average, lake_area, dist_station ) %>%
  left_join(df_house, by = c("sale_id", "year")) %>%
  mutate(log_sd = log(sd_interp),
         sd_lo = ifelse(sd_interp <= 2, 1, 0),
         sd_mi = ifelse(sd_interp> 2 & sd_interp <=4, 1, 0),
         sd_hi = ifelse(sd_interp > 4, 1, 0),
         fireplcs = replace_na(fireplcs, 0),
         lbasement_area = replace_na(lbasement_area, 0)) %>%
  filter(lake_name_sd != "lake ontario: trenton pdgps")

#duplicates = df_house_wq %>%
#  group_by(sale_id) %>%
#  filter(n() > 1)



#tt = df_house_wq %>%
#   filter(as.numeric(lake_dist_sd) < 1000) %>%
#  mutate(missing = ifelse(is.na(sd_interp), "missing", "notmissing")) %>%
#  group_by(lake_name_sd, missing) %>%
#  summarise(n = n(),
#            dist = mean(lake_dist_sd),
#            price = mean(price_real),
#            sd = mean(sd_interp, na.rm = T)) %>%
#  arrange(-n) %>%
 # mutate(sd = ifelse(missing == "notmissing", sd, 0))) %>%
#  pivot_wider(names_from = "missing", values_from = c("n", "dist", "sd", "price"))


table(df_house_wq$interpolation)

df_sd_stations = df_house_wq %>%
 # filter(interpolation == "none")
  group_by(interpolation) %>%
  summarise(count = n(),
            n_readings = sum(n_readings))

df_sd_summary = df_house_wq %>%
  group_by(interpolation) %>%
  summarise(count = n(),
            sd_mean = mean(sd_interp),
            sd_median = median(sd_interp))



df_propcode_summary = df_house_wq %>%
  group_by(propcode) %>%
  summarise(count = n())


#---------
x_replace = c(#"larea", 
            #  "air","heat",
              "fireplcs", 
               "baths", "bedrooms", "storeys",
               "quality","lbasement_area",
            #   "pool","lgar_area"
              "renovation_effect")


df_subset = df_house_wq %>%
  mutate(sd_extrap_years = str_remove(interpolation, "year"),
         sd_extrap_years = replace_na(as.numeric(sd_extrap_years), 0)) %>%
#filter(between(sd_extrap_years, -5 , 5)) %>%
#  filter(interpolation == "none") %>%
#  filter(as.numeric(lake_dist_sd) <= 5000) %>%
  filter(lake_name_sd != "palgrave mill pond") %>% # check with Innis lake
#  filter(as.numeric(dist_station) < 10000) %>%
  filter(!(between(propcode, 200, 300))) %>% # exclude farming operations
  filter(propcode != 383) %>% # exclude bed and breakfasts
  filter(propcode != 130,# exclude non-buildable land
         propcode != 102, # conservation authority
         propcode != 101, # vacant institutional
         propcode != 103, # parks
         propcode != 111, # island
         propcode != 134) %>% 
  #  filter(saletype == "00") %>% # exclude family sales
  filter(saletype != "01") %>% # exclude family sales
  as_tibble() %>%
#  filter(as.numeric(lake_dist_sd) <= 2500) %>%
#  filter(propcode == 301) %>%
  mutate(renovation_effect = ifelse(renovation_effect > 200, 0, renovation_effect),
         detached_house = ifelse(between(propcode, 301, 305) |
                                   propcode == 313 |
                                   propcode == 314, 1, 0),
         semi_detached_house = ifelse(propcode == 311 |
                                   propcode == 322, 1, 0),         
         townhouse_house = ifelse(propcode == 309 |
                                        propcode == 350|
                                    propcode == 352, 1, 0),           
         multi_house = ifelse(propcode == 312 |
                                between(propcode, 332, 336), 1, 0), 
         apartment_house = ifelse(propcode == 340 | propcode == 306, 1, 0),          
         seasonal_prop = ifelse(between(propcode, 391, 395), 1, 0),
         vacant_lot = ifelse(propcode == 100 |
                               propcode == 125 |
                               propcode == 127 |
                               propcode == 169 |
                               propcode == 110, 1, 0),
   # sd_distance_100m = ifelse(as.numeric(lake_dist_sd) <= 100, 1, 0),
        sd_waterfront = ifelse(wfront_l == 1 & as.numeric(lake_dist_sd) <= 100, 1, 0),
         sd_distance_500m = ifelse(#wfront_l == 0 &  
           as.numeric(lake_dist_sd) <= 500, 1, 0),
         sd_distance_1000m = ifelse(#wfront_l == 0 & 
           as.numeric(lake_dist_sd) > 500 & 
                                      as.numeric(lake_dist_sd) <= 1000, 1, 0),
         sd_distance_2000m = ifelse(as.numeric(lake_dist_sd) > 1000 & 
                                      as.numeric(lake_dist_sd) <= 2000, 1, 0)) %>%
  mutate(across(all_of(x_replace), ~ifelse(vacant_lot == 1, replace_na(., 0), .)))

#summary(df_dist)

muskoka_counties = c("Muskoka")
kawartha_counties = c("Kawartha Lakes")
peter_counties = c("Peterborough")
north_counties = c("Nipissing", "Sudbury")
simcoe_counties = c("Simcoe")
outer_counties = c("Huron", "Grey", "Bruce", "Perth","Wellington",  "Dufferin")

lontario_counties = c("Hastings","Northumberland",
                  "Lennox and Addington", "Frontenac", "Prince Edward")

east_counties = c("Lanark","Stormont, Dundas and Glengarry", "Leeds and Grenville",
                  "Ottawa", "Prescott and Russell","Renfrew")
sw_counties = c("Oxford", 
                "Chatham-Kent", 
                "Elgin",
                "Middlesex",
                "Lambton",
                "Essex")
se_counties = c("Waterloo","Niagara", "Brant", "Hamilton", "Haldimand-Norfolk")
gta_counties = c("Halton","Durham", "York", "Peel", "Toronto")



lakes_small_houses = df_subset %>%
#  filter(as.numeric(lake_dist_sd) <= 500) %>%
  group_by(lake_name_sd, county) %>%
  summarise(count = n(),
            sd = mean(sd_interp),
            lake_area = mean(lake_area),
            price = mean(price_real),
            dist = mean(lake_dist_sd)) %>%
  filter(count < 5)


#df_subset = df_dist # %>%
#  filter(!(lake_name_sd %in% lakes_small_houses$lake_name_sd)) 
#  filter(wfront_l == 1) 
strings <- c("huron","ontario","erie","georgian bay")

tt2 = df_subset %>%
  mutate(great_lakes = case_when(str_detect(lake_name_sd, paste(strings, collapse = "|" )) ~ 1,
                                 TRUE ~ 0)) %>%
  group_by(county, great_lakes) %>%
  filter(!is.na(sd_interp)) %>%
  summarise(count = n(),
            mean_sd = mean(sd_interp, na.rm = T),
            min_sd = min(sd_interp),
            max_sd = max(sd_interp),
            dist_lake = mean(lake_dist_sd),
            dist_sd = mean(dist_station),
            sd_sd = sd(sd_interp),
            price = mean(price_real))
            

tt %>%
  distinct(lake_name_sd)
  summarise(count = n(),
            mean_sd = mean(sd_interp, na.rm = T),
            min_sd = min(sd_interp),
            max_sd = max(sd_interp),
            sd_sd = sd(sd_interp),
            price = mean(price_real))
  
  table(df_subset$county)
  
df_subset = df_subset %>%
  #  filter((county %in% include_counties)) %>%
#  filter(!(county %in% c(sw_counties, gta_counties))) %>%
  filter(between(price_real, quantile(price_real, .01), 
                 quantile(price_real, .99))) %>%
  mutate(region = case_when(county %in% muskoka_counties ~ "muskoka",
                            county %in% outer_counties ~ "outer",
                            county %in% lontario_counties ~ "lontario",
                            county %in% east_counties ~ "east",
                            county %in% kawartha_counties ~ "kawartha",
                            county %in% peter_counties ~ "peter",
                            county %in% simcoe_counties ~ "simcoe",
                                   county %in% north_counties ~ "north",
                            county %in% sw_counties ~ "sw",
                            county %in% se_counties ~ "se",
                            county %in% gta_counties ~ "gta", 
                            TRUE ~ "other")) %>%
  filter(region != "other") %>%
  mutate(county_merged = case_when(
          county == "Perth" | 
          county == "Oxford" |
          county == "Middlesex" |
          county == "Huron" |
          county == "Lambton" ~ "Huron-Lambton-Perth-Oxford-Middlesex",
          county == "Chatham-Kent" |
          county == "Essex" ~ "Chatham-Kent-Essex",    
          county == "Prescott and Russell" |
          county == "Stormont, Dundas and Glengarry" |
          county == "Leeds and Grenville" |
          county == "Lanark" |
          county == "Renfrew" |
          county == "Ottawa" ~ "Ottawa-Plus",                                     
          county == "Northumberland" |
            county == "Durham" |
            county == "Kawartha Lakes" |
          county == "Peterborough"  ~ "Durham-Peterborough-Northumberland-Kawartha Lakes",
          county == "Toronto" |
          county == "Peel" |
          county == "York" ~ "Peel-Toronto-York", 
          county == "Haldimand-Norfolk" |
          county == "Elgin" |
          county == "Brant" ~ "Elgin-Haldimand-Norfolk-Brant",                                    
          county == "Halton" |
          county == "Hamilton" |
          county == "Niagara" ~ "Halton-Hamilton-Niagara",
          county %in% north_counties ~ "North",
          county == "Simcoe" |
            county == "Muskoka" ~ "Simcoe-Muskoka",
          county == "Frontenac" |
          county == "Hastings" |
          county == "Prince Edward" |
          county == "Lennox and Addington" ~ "Prince Edward-Hastings-Frontenac-Lennox and Addington",
          county == "Bruce" |
          county == "Grey" ~ "Bruce-Grey",                                  
          county == "Wellington" |
          county == "Dufferin" |
          county == "Waterloo" ~ "Waterloo-Wellington-Dufferin",
                             TRUE ~ county)) %>%
  mutate(sd_interp1 = sd_interp * sd_waterfront,
         sd_interp2 = sd_interp * sd_distance_500m,
         sd_interp3 = sd_interp * sd_distance_1000m,
         sd_interp4 = sd_interp * sd_distance_2000m) %>%
  mutate(sing_family_house = ifelse(propcode == 301, 1, 0))


df_townships = df_subset %>%
  group_by(region) %>%
  summarise(townships = length(unique(township)))
  summarise(count = n())

df_townships = df_subset %>%
  group_by(county, township) %>%
  summarise(count = n(),
            lakes = length(unique(lake_name_canvec)))


t_counties = df_subset %>%
  filter(!is.na(sd_interp)) %>%
  group_by(county2) %>%
  summarise(n_houses = n(),
            n_lakes = length(unique(lake_name_sd)),
            price = mean(price_real),
            cor = cor(sd_interp, price_real),
            mean_sd = mean(sd_interp, na.rm = T),
            min_sd = min(sd_interp, na.rm = T),
            max_sd = max(sd_interp, na.rm = T),
            sd_sd = sd(sd_interp, na.rm = T),
            price = mean(price_real, na.rm = T))


tt = df_house_wq %>%
  filter(is.na(sd_interp))

tt = df_subset %>%
  group_by(county, lake_name_sd) %>%
  tally()

#---------
x_struct1 = c("lsize", "larea","log(cdist_pc_2011_gte_100k)")
#---------
x_struct2 = c("log(lake_area)",
               "vacant_lot", "seasonal_prop",#sing_family_house",
              "air","heat","fireplcs", 
           #  "floodp_d", "floodp_r", 
         #  "log(cdist_pc_2011_toronto)",
          #   "view_pr", 
       #  "view_gd", "view_wf",
         #    "wtr_l_r", 
          #   "wtr_well", "wtr_mun", "wfront_l",# "wfront_r",
             "baths", "bedrooms", "storeys",
             "quality","lbasement_area",
             "pool","renovation_effect",#"lcity",
       #         "ab_ind","ab_comm","ab_inst","ab_educ","ab_golf",
             "lgar_area")


f_sd <- paste("log(price_real)  ~",
              #     paste("sd_interp1"),"+",
              paste("sd_interp2"),"+",
              paste("sd_interp3"),"+",
              paste("sd_interp4"),"+",
              
             #   paste("i(sd_waterfront)"),"+",
              paste("i(sd_distance_500m)"),"+",
              paste("i(sd_distance_1000m)"),"+",
             paste("i(sd_distance_2000m)"),"+",
             #      paste("i(sd_distance_1000m)"),"+",
              #            paste("i(sd_distance_2000m)"),"+",
              paste0(x_struct1, collapse=" + "), "+",
              paste0(x_struct2, collapse=" + "),
              "  | year + sw0(county, township)")

res_multi = feols(as.formula(f_sd), 
                  df_subset)
etable(res_multi)

f_sd <- paste("log(price_real)  ~",
              #     paste("i(sd_interp1, county_merged)"),"+",
           #   paste("i(sd_interp, county_merged)"),"+",
              paste("i(sd_interp2, county_merged)"),"+",
              paste("i(sd_interp3, county_merged)"),"+",
      #     paste("i(sd_interp4, county_merged)"),"+",
           #  paste("i(sd_waterfront)"),"+",
              paste("i(sd_distance_500m)"),"+",
              paste("i(sd_distance_1000m)"),"+",
              #      paste("i(sd_distance_1000m)"),"+",
      #                    paste("i(sd_distance_2000m)"),"+",
              paste0(x_struct1, collapse=" + "), "+",
              paste0(x_struct2, collapse=" + "),
              "  | year + sw0(county, township)")

res_multi = feols(as.formula(f_sd), 
                  df_subset)
etable(res_multi)

f_sd <- paste("log(price_real)  ~",
            #  paste("sd_interp1"),"+",
              paste("sd_interp2"),"+",
              paste("sd_interp3"),"+",
           #   paste("i(sd_waterfront)"),"+",
              paste("i(sd_distance_500m)"),"+",
              paste("i(sd_distance_1000m)"),"+",
              paste0(x_struct1, collapse=" + "), "+",
              paste0(x_struct2, collapse=" + "),
              "  | year + township")

res_multi = feols(as.formula(f_sd), 
                  df_subset, fsplit = ~county_merged)
etable(res_multi)

table(df_subset$county_merged)

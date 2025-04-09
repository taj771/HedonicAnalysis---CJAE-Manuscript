# re-estimate distance to gta variable
# check out palgrave sd_dist, Innis lake. Need correct 

rm(list=ls(all=TRUE))
library(brms)
library(fixest)
library(tidyverse)
library(janitor)

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

df_municipal = read.csv("data/HouseData/municipal_codes.csv") %>%
  clean_names(.) %>%
  distinct(upper_tier_municipality, official_municipal_name, 
           municipal_name_shortform, assessment_code) %>%
  mutate(county = ifelse(upper_tier_municipality == "", 
                         official_municipal_name, 
                         upper_tier_municipality),
         county = case_when(county %in% nipissing ~ "NIPISSING DISTRICT", 
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
                            TRUE ~ county)) %>%
  select(assessment_code, county, township = municipal_name_shortform)
  


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
  filter(lake_name_sd != "palgrave mill pond")  %>%
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
  filter(year < 2020) %>% 
  as_tibble() %>%
  mutate(renovation_effect = ifelse(renovation_effect > 200, 0, renovation_effect),
         prop_type = case_when(between(propcode, 301, 305) |
                                        propcode == 313 |propcode == 314 ~ "detached_house",
                                        propcode == 311 | propcode == 322 ~ "semi_detached_house",       
                                        propcode == 309 |propcode == 350| propcode == 352 ~ "townhouse_house",         
                                        propcode == 312 | propcode == 340 | propcode == 341 | 
                                        propcode == 306 | between(propcode, 332, 336) ~ "multi_house", 
                                        propcode == 381 | propcode == 382 ~ "mobile_home", 
                                        between(propcode, 391, 395) ~ "seasonal_prop",
                                        propcode == 100 |propcode == 125 | propcode == 127 |
                                          propcode == 169 |propcode == 110 | propcode == 112 ~ "vacant_lot",
                                        TRUE ~ NA_character_)) %>%
  mutate(across(all_of(x_replace), ~ifelse(prop_type == "vacant_lot", replace_na(., 0), .))) %>%
  mutate(assessment_code = as.integer(paste0(county2, city))) %>%
  left_join(df_municipal) 
  

table(df_subset$county)
length(unique(df_subset$county))

tt = df_subset %>%
  group_by(county) %>%
  summarise(townships = length(unique(township)),
            houses = n())

# houses by lake name
tt = df_subset %>%
  group_by(lake_name_sd) %>%
  summarise(lake_size  =mean(lake_area, na.rm = T),
            count = n())


lakes = c("CITY OF KAWARTHA LAKES", "NIPISSING DISTRICT", 
          "DISTRICT MUNICIPALITY OF MUSKOKA","COUNTY OF SIMCOE",
          "CITY OF ORILLIA", "COUNTY OF PETERBOROUGH")

df_subset = df_subset  %>%
  filter(!is.na(price_real)) %>%
#  filter(between(price_real, quantile(price_real, .01), 
#                 quantile(price_real, .99))) %>%
  mutate(sd_distance_100m = ifelse(as.numeric(lake_dist_sd) <= 100, 1, 0),
         sd_waterfront = ifelse(wfront_l == 1 & as.numeric(lake_dist_sd) <= 100, 1, 0),
         sd_distance_500m = ifelse(wfront_l == 0 &  
                                     as.numeric(lake_dist_sd) <= 500, 1, 0),
         sd_distance_1000m = ifelse(wfront_l == 0 & 
                                      as.numeric(lake_dist_sd) > 500 & 
                                      as.numeric(lake_dist_sd) <= 1000, 1, 0),
         sd_distance_2000m = ifelse(as.numeric(lake_dist_sd) > 1000 & 
                                      as.numeric(lake_dist_sd) <= 2000, 1, 0),
         sd_interp1 = log(sd_interp) * sd_waterfront,
         sd_log_average = ifelse(sd_distance_500m == 1, log(sd_average), 0),
         sd_interp2 = log(sd_interp) * sd_distance_500m,
         sd_interp3 = log(sd_interp) * sd_distance_1000m,
         sd_interp4 = log(sd_interp) * sd_distance_2000m,
         inverse_dist = (1 / lake_dist_sd),
         inverse_dist_close = inverse_dist * sd_distance_500m) %>%
  mutate(large_lake = ifelse(lake_area > 100, 1, 0)* sd_distance_500m) 



tt = df_subset  %>%
  group_by(interpolation, sd_distance_500m) %>%
  tally(.)

tt = df_subset  %>%
  group_by(county) %>%
  tally(.)
#---------
x_struct1 = c("lsize", "log(area_tot)")#,"log(cdist_pc_2011_gte_100k)")
#---------
x_struct2 = c(#"log(lake_area)",
               "prop_type",
            #  "air","heat","fireplcs", 
          #   "floodp_d", "floodp_r", 
         #  "log(cdist_pc_2011_toronto)",
       #      "view_pr", 
       #  "view_gd", "view_wf",
         #    "wtr_l_r", 
        #     "wtr_well", "wtr_mun",#, "wfront_l",# "wfront_r",
             "baths", "bedrooms", "storeys",
             "quality","lbasement_area")#,
          #   "pool","renovation_effect",#"lcity",
        #        "ab_ind","ab_comm","ab_inst","ab_educ","ab_golf")#,
        #     "lgar_area")

f_sd <- paste("log(price_real)  ~",
                   paste("sd_interp1"),"+",
           #        paste("sd_log_average"),"+",
               paste("sd_interp2"),"+",
           #   paste("sd_interp3"),"+",
           #   paste("sd_interp4"),"+",
              "inverse_dist +",# + inverse_dist_close +",
              paste("sd_waterfront"),"+",
              paste("sd_distance_500m"),"+",
          #    paste("sd_distance_1000m"),"+",
          #   paste("sd_distance_2000m"),"+",
              paste0(x_struct1, collapse=" + "), "+",
              paste0(x_struct2, collapse=" + "),
             "  | year + township")

df_analysis = df_subset %>%
  filter(as.numeric(lake_dist_sd) < 2000) %>%
  mutate(lprice = log(price_real))

read1 <- brm(data = df_analysis,
             family = gaussian,
             formula = lprice ~ lsize + 
               sd_interp1+ sd_interp2+inverse_dist + sd_waterfront + sd_distance_500m,
       #      prior = c(prior(normal(0, 10), class = Intercept),
        #               prior(cauchy(0, 1), class = sd),
         #              prior(cauchy(0, 1), class = sigma)),
             iter = 1000, warmup = 500, chains = 4, cores = 4,
          #   control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)

summary(read1)
plot(read1)

res_multi = feols(as.formula(f_sd), df_subset)
etable(res_multi)

df_subset = df_subset %>%
  group_by(lake_name_sd) %>%
  mutate(count = n()) %>%
  filter(count > 500)

res_multi = feols(as.formula(f_sd), 
                  df_subset, fsplit = ~lake_name_sd)
etable(res_multi)


res_multi = feols(as.formula(f_sd), 
                  df_subset, fsplit = ~year)
etable(res_multi)


df_subset = df_subset %>%
  mutate(simcoe = ifelse(lake_name_sd == "lake simcoe", 1, 0))

res_multi = feols(as.formula(f_sd), 
                  df_subset, fsplit = ~simcoe)
etable(res_multi)



df_subset = df_subset %>%
  mutate(cabin = ifelse(propcode == 391 | propcode == 392 | propcode == 395, 1, 0))
res_multi = feols(as.formula(f_sd), df_subset, fsplit = ~cabin)
etable(res_multi)

###############################################################################
# CJAE Manuscript - Appendix Table 2
################################################################################

# load data set
df_subset = readRDS("data/analysis_sample.rds")

# to filter data only within interpolation limits
interp_lo = -5
interp_hi = 5

df_main <- df_subset%>%
  mutate(sd_extrap_years = str_remove(interpolation, "year"),
         sd_extrap_years = replace_na(as.numeric(sd_extrap_years), 0))

df_main <- filter(df_main, sd_extrap_years >= interp_lo )
df_main <- filter(df_main, sd_extrap_years <= interp_hi )


# remove outlines of sale price 
#quantile(df_main$price_real,probs=c(0.01,0.99))

price_lo <- 56596.6
price_high <- 1008363.7 



df_main <- filter(df_main, price_real >= price_lo )
df_main <- filter(df_main, price_real <= price_high )

# deal with missing lotsize
# replace missing lot size with average lot size

mean(df_main$lotsize) #1228.112


# deal with missing totalarea
# replace missing lot size with average lot size

mean(df_main$area_tot, na.rm = T) #1456.58

df_main$area_tot[df_main$area_tot == "0"] <- 1228.11


# remove outlier of bedrooms
# create dummy variable to represent the houses which has bedrooms higher than 10

df_main <- df_main%>%
  mutate(bedrooms_dum = case_when(bedrooms >= 10 ~ 1,
                                  bedrooms < 10~ 0))

# remove outlier of bathrooms
# create dummy variable to represent the houses which has bedrooms higher than 10

df_main <- df_main%>%
  mutate(baths_dum = case_when(baths >= 5 ~ 1,
                               baths < 5~ 0))

# filter only open market sales

df_main <- df_main%>%
  filter(saletype == "00")



# load file with distance to hwy400 

library(sf)
library(tidyverse)

hwydist <- st_read("./shapefile/house_n400hwy_meters.shp")

library(dplyr)


hwydist <- hwydist%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(roll_num,NEAR_DIST)%>%
  rename("NEAR_DIST_HWY"="NEAR_DIST")

hwydist$roll_num <- as.numeric(hwydist$roll_num)

df_main <- df_main%>%
  left_join(hwydist)

df1 <- subset(df_main, df_main$dist_500m==1)

df2 <- subset(df_main, df_main$dist_500m==0)


df1 <- df1%>%
  filter(!is.na(price_real))%>%
  filter(!is.na(sd_interp))%>%
  filter(!is.na(lotsize))%>%
  filter(!is.na(log_area_tot))%>%
  filter(!is.na(log_dist_toronto))%>%
  filter(!is.na(log_dist_cma))%>%
  filter(!is.na(prop_type))%>%
  filter(!is.na(pool))%>%
  filter(!is.na(air_cond))%>%
  filter(!is.na(heat_elect))%>%
  filter(!is.na(heat_forcedair))%>%
  filter(!is.na(heat_hw))%>%
  filter(!is.na(floodp_d))%>%
  filter(!is.na(floodp_r))%>%
  filter(!is.na(view_pr))%>%
  filter(!is.na(view_gd))%>%
  filter(!is.na(view_wf))%>%
  filter(!is.na(wtr_l_r))%>%
  filter(!is.na(wtr_well))%>%
  filter(!is.na(wtr_mun))%>%
  filter(!is.na(wfront_l))%>%
  filter(!is.na(wfront_r))%>%
  filter(!is.na(ab_ind))%>%
  filter(!is.na(ab_comm))%>%
  filter(!is.na(ab_inst))%>%
  filter(!is.na(ab_educ))%>%
  filter(!is.na(ab_golf))%>%
  filter(!is.na(fireplcs))%>%
  filter(!is.na(baths))%>%
  filter(!is.na(bedrooms))%>%
  filter(!is.na(storeys))%>%
  filter(!is.na(quality))%>%
  filter(!is.na(lbasement_area))%>%
  filter(!is.na(age))%>%
  filter(!is.na(bedrooms_dum))%>%
  filter(!is.na(baths_dum))


df2 <- df2%>%
  filter(!is.na(price_real))%>%
  filter(!is.na(lotsize))%>%
  filter(!is.na(log_area_tot))%>%
  filter(!is.na(log_dist_toronto))%>%
  filter(!is.na(log_dist_cma))%>%
  filter(!is.na(prop_type))%>%
  filter(!is.na(pool))%>%
  filter(!is.na(air_cond))%>%
  filter(!is.na(heat_elect))%>%
  filter(!is.na(heat_forcedair))%>%
  filter(!is.na(heat_hw))%>%
  filter(!is.na(floodp_d))%>%
  filter(!is.na(floodp_r))%>%
  filter(!is.na(view_pr))%>%
  filter(!is.na(view_gd))%>%
  filter(!is.na(view_wf))%>%
  filter(!is.na(wtr_l_r))%>%
  filter(!is.na(wtr_well))%>%
  filter(!is.na(wtr_mun))%>%
  filter(!is.na(wfront_l))%>%
  filter(!is.na(wfront_r))%>%
  filter(!is.na(ab_ind))%>%
  filter(!is.na(ab_comm))%>%
  filter(!is.na(ab_inst))%>%
  filter(!is.na(ab_educ))%>%
  filter(!is.na(ab_golf))%>%
  filter(!is.na(fireplcs))%>%
  filter(!is.na(baths))%>%
  filter(!is.na(bedrooms))%>%
  filter(!is.na(storeys))%>%
  filter(!is.na(quality))%>%
  filter(!is.na(lbasement_area))%>%
  filter(!is.na(age))%>%
  filter(!is.na(bedrooms_dum))%>%
  filter(!is.na(baths_dum))


nrow(df1)+nrow(df2)

df12 <- rbind(df1,df2)%>%
  mutate(distance_cma_km = distance_cma_metres/1000,
         distance_toronto_km = distance_toronto_metres/1000 )


library(dplyr)
library(tidyr)

# Step 1: Summary statistics
summary <- df12 %>%
  select(price_real, sd_average, sd_interp, lotsize, dist_250m, dist_250m_500m, 
         distance_cma_km, distance_toronto_km, great_lake, aircond, heat_elect,
         baths, bedrooms, storeys, pool, area_tot) %>%
  rename(pricereal = price_real,
         sdaverage = sd_average,
         sdinterp = sd_interp,
         dist250m = dist_250m,
         dist250m500m = dist_250m_500m,
         distancecmakm = distance_cma_km,
         distancetorontokm = distance_toronto_km,
         greatlake = great_lake,
         heatelect = heat_elect,
         areatot=area_tot) %>%
  summarise(across(where(is.numeric),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE),
                        median = ~median(., na.rm = TRUE),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Step 2: Tidy to long then wide

df.stats.tidy <- summary %>%
  pivot_longer(cols = everything(), names_to = c("var", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  select(var, mean, sd, median, min, max) %>%
  mutate(across(c(mean, sd, median, min, max), ~ round(., 2))) %>%
  mutate(var = dplyr::recode(var,
                             pricereal = "Real Price ($2021)",
                             aircond = "Air Condition (1/0)",
                             distancecmakm = "Distance to nearest CMA (km)",
                             distancetorontokm = "Distance to nearest highway (Km)",
                             heatelect = "Electric Heat (1/0)",
                             dist250m = "Lakeshore within 250m (1/0)",
                             dist250m500m = "Lakeshore within 250m 500m (1/0)",
                             lotsize = "Lot size (m2)",
                             sdaverage = "Measured SD (m)",
                             sdinterp = "Measured SD+ Interpolated SD (m)",
                             baths = "Number of Bathrooms",
                             bedrooms = "Number of Bedrooms",
                             storeys = "Number of stories",
                             pool = "Pool (1/0)",
                             areatot = "Total Area (m2)"
  ))


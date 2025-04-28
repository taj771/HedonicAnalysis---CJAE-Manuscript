###############################################################################
# without removing outlier in sale price
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


df_main <- df_main%>%
  select(price_real,log_sd_inter_500,dist_500m,lotsize,log_area_tot,log_dist_toronto,log_dist_cma,prop_type,pool,air_cond, 
         heat_elect,heat_forcedair,heat_hw, floodp_d, floodp_r, view_pr, view_gd, view_wf,
         wtr_l_r, wtr_well, wtr_mun, wfront_l,wfront_r,ab_ind, ab_comm, ab_inst, ab_educ,
         ab_golf,fireplcs,baths,bedrooms, storeys, quality, lbasement_area,
         age, bedrooms_dum, baths_dum, NEAR_DIST_HWY,ward,year,sale_month)

# Property characteristics
x_struct = c("lotsize",  
             "log_area_tot",
             "log_dist_toronto", 
             "log_dist_cma",
             "prop_type",
             "pool", 
             "air_cond", 
             "heat_elect", 
             "heat_forcedair", 
             "heat_hw",
             "floodp_d", 
             "floodp_r", 
             "view_pr",
             "view_gd", 
             "view_wf",
             "wtr_l_r", 
             "wtr_well", 
             "wtr_mun",
             "wfront_l",
             "wfront_r",
             "ab_ind",
             "ab_comm",
             "ab_inst",
             "ab_educ",
             "ab_golf",
             #"lgar_area",
             "fireplcs",
             "baths",
             "bedrooms", 
             "storeys",
             "quality",
             "lbasement_area",
             'age',
             'bedrooms_dum',
             'baths_dum',
             'NEAR_DIST_HWY')


library(fixest)

stepwise_feols <- function(dep_var, base_vars, candidate_vars, fixed_effects, data, cluster_var = NULL) {
  selected_vars <- base_vars
  remaining_vars <- candidate_vars
  best_aic <- Inf
  best_model <- NULL
  
  repeat {
    results <- lapply(remaining_vars, function(var) {
      formula_str <- paste(dep_var, "~", paste(c(selected_vars, var), collapse = " + "), fixed_effects)
      model <- feols(as.formula(formula_str), data = data, vcov = as.formula(paste0("~", cluster_var)))
      list(var = var, model = model, aic = AIC(model))
    })
    
    aic_vals <- sapply(results, function(x) x$aic)
    best_candidate <- results[[which.min(aic_vals)]]
    
    if (best_candidate$aic < best_aic) {
      best_aic <- best_candidate$aic
      selected_vars <- c(selected_vars, best_candidate$var)
      remaining_vars <- setdiff(remaining_vars, best_candidate$var)
      best_model <- best_candidate$model
    } else {
      break
    }
  }
  
  return(best_model)
}

dep_var <- "log(price_real)"
base_vars <- c("log_sd_inter_500", "dist_500m")
candidate_vars <- x_struct  # All your property attributes
fixed_effects <- "| ward^year + sale_month"
cluster_var <- "ward"

best_model <- stepwise_feols(dep_var, base_vars, candidate_vars, fixed_effects, df_main, cluster_var)

summary(best_model)

modelsummary(best_model)

list1 <- c("baths","log_dist_toronto","quality","prop_type","log_dist_cma","wtr_mun","fireplcs",
           "wfront_l","lbasement_area","wtr_well","pool","view_gd","air_cond","storeys","log_area_tot",
           "ab_comm","age","ab_golf","bedrooms","wfront_r","baths_dum","NEAR_DIST_HWY","view_wf",
           "ab_educ","bedrooms_dum","view_pr","floodp_d","wtr_l_r","lotsize","ab_inst", "heat_hw")


list2 <- c("lotsize", "log_area_tot","log_dist_toronto","log_dist_cma","prop_type","pool", 
           "air_cond","heat_elect","heat_forcedair","heat_hw","floodp_d","floodp_r","view_pr","view_gd", 
           "view_wf", "wtr_l_r","wtr_well", "wtr_mun","wfront_l","wfront_r","ab_ind",
           "ab_comm","ab_inst","ab_educ","ab_golf","fireplcs","baths","bedrooms", 
           "storeys","quality","lbasement_area",'age','bedrooms_dum', 'baths_dum',
           'NEAR_DIST_HWY')

setdiff(list2, list1)

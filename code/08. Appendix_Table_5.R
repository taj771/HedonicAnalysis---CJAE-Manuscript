# Robustness of alternative functional specification
# Results of Appendix table 5

# load library
library(car)
library(fixest)
library(dplyr)
library(modelsummary)

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


# Transform WQ variable for Box-Cox (not yet applying transformation here)
df_main <- df_main %>%
  mutate(lin_sd_500 = ifelse(dist_500m == 1, sd_interp, 0))

library(MASS)

###########################################################
# Model 1: Linear model for Box-Cox estimation
###########################################################
model_lm <- lm(price_real ~ lin_sd_500 + dist_500m + lotsize + log_area_tot +
                 log_dist_toronto + log_dist_cma + prop_type + pool + air_cond +
                 heat_elect + heat_forcedair + heat_hw + floodp_d + floodp_r +
                 view_pr + view_gd + view_wf + wtr_l_r + wtr_well + wtr_mun +
                 wfront_l + wfront_r + ab_ind + ab_comm + ab_inst + ab_educ + ab_golf +
                 fireplcs + baths + bedrooms + storeys + quality + lbasement_area + age +
                 bedrooms_dum + baths_dum + ward * year - 1 + sale_month - 1 + NEAR_DIST_HWY,
               data = df_main)

# Estimate Box-Cox lambda
bc.car <- powerTransform(model_lm)
lambda <- as.numeric(bc.car$lambda)
cat("Estimated lambda:", lambda, "\n")

# Transform the dependent variable using Box-Cox
df_main$bc.price <- bcPower(df_main$price_real, lambda = lambda)

###############################################################
# Model 5: Box-Cox transformed price
##############################################################
x_500_inter <- paste("bc.price  ~ lin_sd_500  + dist_500m +", paste0(x_struct, collapse = " + "))
model_5 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_5, stars = T)


ll_raw <- logLik(model_5)


# Jacobian adjustment for Box-Cox
jacobian_bc <- if (lambda != 0) {
  (lambda - 1) * sum(log(df_main$price_real))
} else {
  sum(log(df_main$price_real))
}
ll_bc <- as.numeric(logLik(model_5)) + jacobian_bc

###################################
# Model 6: Linear function
#####################################
x_500_inter <- paste("price_real  ~ lin_sd_500  + dist_500m +", paste0(x_struct, collapse = " + "))
model_6 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_6, stars = T)


ll_lin <- as.numeric(logLik(model_6))

######################################
# Model 7: Double log fucntion
######################################
x_500_inter <- paste("log(price_real)  ~ log_sd_inter_500  + dist_500m +", paste0(x_struct, collapse = " + "))
model_7 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_7, stars = T)



# Jacobian adjustment 
jacobian_log <- -sum(log(df_main$price_real))
# Adjusted log-likelihood
ll_log <- as.numeric(logLik(model_7)) + jacobian_log


#######################################
# Print adjusted log-likelihoods
######################################
cat("Adjusted log-likelihoods:\n")
cat("Box-Cox (model_5):", ll_bc, "\n")
cat("Linear (model_6):", ll_lin, "\n")
cat("Log (model_7):", ll_log, "\n")

########################################
# Manually compute AICs with adjusted LL
# AIC = -2 * LL + 2 * k
########################################
k_5 <- length(coef(model_5))
k_6 <- length(coef(model_6))
k_7 <- length(coef(model_7))

aic_bc <- -2 * ll_bc + 2 * k_5
aic_lin <- -2 * ll_lin + 2 * k_6
aic_log <- -2 * ll_log + 2 * k_7

cat("\nAIC comparison:\n")
cat("Box-Cox (model_5):", aic_bc, "\n")
cat("Linear (model_6):", aic_lin, "\n")
cat("Log (model_7):", aic_log, "\n")

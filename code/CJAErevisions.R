# clear memory
rm(list = ls())

# load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  fixest,
  modelsummary,
  flextable,
  dataPreparation
)

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
# observations 3sd away
#df_main <- remove_sd_outlier(df_main, cols = "price_real", n_sigmas = 3, verbose = TRUE)

#quantile(df_main$price_real,probs=c(0.01,0.99))

#price_lo <- 56596.6
#price_high <- 1008363.7 


#df_main <- filter(df_main, price_real >= price_lo )
#df_main <- filter(df_main, price_real <= price_high )

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


# Remove vacant lot  - high corelation
#df_main <- df_main%>%
  #filter(prop_type != "vacant_lot")


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
             'baths_dum')

cm_limited <- c('log_sd_inter_500'    = 'Log(SD)X500m',
                'log_sd_inter_waterfront' = 'Log(SD)XWaterfront',
                'log_sd_inter_250'    = 'Log(SD)X250m',
                'log_sd_inter_250_500' = 'Log(SD)X250m-500m',
                'log_sd'    = 'Log(SD)',
                'log_sd_inter'    = 'Log(SD)',
                'log_sd_500'    = 'Log(SD)X500m',
                'log_sd_inter_250_gl' = 'GL log(SD)X250m',
                'log_sd_inter_250_gl_no' = 'Non-GL log(SD)X250m',        
                'log_sd_inter_250_500_gl'    = 'GL Log(SD)X250m-500m',
                'log_sd_inter_250_500_gl_no'    = 'Non-GL Log(SD)X250-500m',
                'log(sd_interp):dist:: (0-100)'= 'ln SD X Dist(0-100)',
                'log(sd_interp):dist:: (100-200)'= 'ln SD X Dist(100-200)',
                'log(sd_interp):dist:: (200-300)'= 'ln SD X Dist(200-300)',
                'log(sd_interp):dist:: (300-400)'= 'ln SD X Dist(300-400)',
                'log(sd_interp):dist:: (500+)'= 'ln SD X Dist(500+)',
                'log_sd_250_500' = 'Log(SD)X250m-500m',
                'log_sd_inter_0_250' = 'Log(SD)X0m-250m',
                'sd_inter_500' = 'SDX500m',
                'sd_inter_250' = 'SDX0m-250m',
                'sd_inter_250_500' = 'SDX250m-500m',
                'log_sd_inter_100' = 'SDX0m-100m',
                'log_sd_inter_100_200' = 'SDX100m-200m',
                'log_sd_inter_200_300' = 'SDX200m-300m',
                'log_sd_inter_300_400' = 'SDX300m-400m',
                'log_sd_inter_400_500' = 'SDX400m-500m',
                'log_sd_inter_250_wf_ex' = 'Log(SD)X250m'
)


cm_struc<- c(
  'lotsize' = 'Lot size (m2)',  
  "log_area_tot" = "Total area (m2)",
  "prop_type" = 'Property type',
  "pool" = 'Presence of pool (Yes/No)', 
  "air_cond" = 'Presence of air condition', 
  "heat_elect" = "Pelectric heat (Yes/No)", 
  "heat_forcedair" = 'Heat type: Force Air' , 
  "heat_hw" = 'Heat type: Hot water',
  "floodp_d" = 'Floodplain – Developable (Yes/No)', 
  "floodp_r" = 'Floodplain – Restricted (Yes/No)', 
  "fireplcs" = "Presence of fire place (Yes/No)",
  "baths" = "Number of baths",
  "bedrooms" = "Number of bedrooms", 
  "storeys" = "Total Number of Storeys",
  "quality" = "Quality",
  "lbasement_area" = "log (Area of basement (m2))", 
  'age' = " Age",
  'bedrooms_dum'= "Bedroom dummy",
  'baths_dum' = "Bathroo dummy"
)

cm_neigh <- c("view_pr" = "Predominant View – Obstructed (Yes/N0)",
              "view_gd" = "Predominant View- Panoramic (Yes/No)" , 
              "view_wf" = "Presence of waterfront view (Yes/No)",
              "wtr_l_r" = "Lake waterfront (Yes/No)", 
              "wtr_well" = "Private Well (Yes/No)", 
              "wtr_mun" = "Municipal water supply (Yes/No)",
              "wfront_l" = ". Lake waterfront (Yes/No)",
              "wfront_r" = "River Waterfront (Yes/No)",
              "ab_ind" = "Abuts Industrial (Yes/No)",
              "ab_comm" = "Abuts Commercial (Yes/No)",
              "ab_inst" = "Abuts Institutional (Yes/No)",
              "ab_educ" = "AAbuts Educational Institution (Yes/No)",
              "ab_golf" = "Abuts Golf Course (Yes/No)")

cm_envi <- c(        
  "log_dist_toronto" = "log(Distance to Toronto (km))", 
  "log_dist_cma" = "log(Distance to nearest CMA (km))")


################################################################################
# Table 2 - Basic model results
################################################################################

fes_ward = "| ward^year + sale_month"


x_500_inter = paste("log(price_real)  ~ log_sd_inter_500  + dist_500m +",
                    paste0(x_struct, collapse=" + "))
x_distbuf_2 = paste("log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500+ 
                  dist_250m + dist_250m_500m +",
                    paste0(x_struct, collapse=" + "))

x_distbuf_2_wf_sig_check = paste("log(price_real)  ~  log_sd_inter_250 + log_sd_inter_500 + 
                  dist_250m + dist_250m_500m +",
                                 paste0(x_struct, collapse=" + "))



# Repeat sales
df_repeat = df_main %>%
  dplyr::select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
         log_sd_250,log_sd_250_500, log_sd_inter_250,log_sd_inter_250_500, dist_250m, dist_250m_500m,
         log_sd_inter_250_wf_ex,log_sd_inter_waterfront)

df_repeat = df_subset  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


models_fes = list(
  model_1 = feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward),
  model_2 = feols(as.formula(paste(x_distbuf_2, fes_ward)), df_main, vcov = ~ward),
  model_3 = feols(as.formula(paste(x_distbuf_2_wf_sig_check, fes_ward)), df_main, vcov = ~ward),
  model_4 = df_repeat  %>%
    feols(log(price_real)  ~  log_sd_inter_500  + age | year + sale_month +roll_num, ., vcov = ~roll_num),
  model_5 = df_repeat  %>%
    feols(log(price_real)  ~  log_sd_inter_250 + log_sd_inter_250_500  + age | year + sale_month +roll_num, ., vcov = ~roll_num)
)

modelsummary(models_fes, 
             coef_map = cm_limited, 
             #gof_map = gm,
             stars = T)

#options(modelsummary_format_numeric_latex = "mathmode")


modelsummary(models_fes, 
             coef_map = cm_limited, 
             #gof_map = gm,
             stars = T,
             output = "./results/table2_with salevalueoutlier.docx")


################################################################################
# Box-Cox analysis 
################################################################################

library(MASS)

df_main <- df_main%>%
  mutate(bc_sd_500 = ifelse(dist_500m == 1, sd_interp, 0))
  #filter(bc_sd_500 > 0)%>%
  #drop_na(bc_sd_500)




# Model 1 - without log transformation of price_real

model_1 <- lm(price_real  ~ bc_sd_500  + dist_500m +lotsize + log_area_tot+
                log_dist_toronto+ log_dist_cma+prop_type+pool+ air_cond+ heat_elect+ heat_forcedair+ 
                heat_hw+floodp_d+ floodp_r+ view_pr+view_gd+ view_wf+wtr_l_r+ wtr_well+ wtr_mun+
                wfront_l+wfront_r+ab_ind+ab_comm+ab_inst+ab_educ+ab_golf+fireplcs+baths+
                bedrooms+ storeys+quality+lbasement_area+age+bedrooms_dum+baths_dum+
                ward*year-1 + sale_month-1+NEAR_DIST_HWY, df_main)


# chek robustness with car package
# Transfer box-cox
#bc_model_1 <- boxcox(model_1)

library(car)
bc.car <- powerTransform(model_1)
#print(bc.car$lambda)

print(bc.car$lambda)

# transform price

df_main$bc.price = bcPower(df_main$price_real, lambda = bc.car$lambda)


# predictor variable WQ


#boxTidwell(bc.price ~ bc_sd_500, other.x =~ dist_500m +lotsize + log_area_tot+
             #log_dist_toronto+ log_dist_cma+prop_type+pool+ air_cond+ heat_elect+ heat_forcedair+ 
             #heat_hw+floodp_d+ floodp_r+ view_pr+view_gd+ view_wf+wtr_l_r+ wtr_well+ wtr_mun+
             #wfront_l+wfront_r+ab_ind+ab_comm+ab_inst+ab_educ+ab_golf+fireplcs+baths+
             #bedrooms+ storeys+quality+lbasement_area+age+bedrooms_dum+baths_dum+
             #ward*year-1 + sale_month-1, data = df_main )




# transform WQ

#df_main$bc.sd_interp = bcPower(df_main$sd_interp, lambda = 1.1346 )


#df_main <- df_main%>%
  #mutate(bc_sd_500 = ifelse(dist_500m == 1, bc.sd_interp, 0))

# run the model with box-cox transformed 



x_500_inter = paste("bc.price  ~ bc_sd_500  + dist_500m +",
                    paste0(x_struct, collapse=" + "))

model_5 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_5, stars = T)


# linear 

df_main <- df_main%>%
  mutate(lin_sd_500 = ifelse(dist_500m == 1, sd_interp, 0))


x_500_inter = paste("price_real  ~ lin_sd_500  + dist_500m +",
                    paste0(x_struct, collapse=" + "))

model_6 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_6, stars = T)

# semi log

x_500_inter = paste("log(price_real)  ~ lin_sd_500  + dist_500m +",
                    paste0(x_struct, collapse=" + "))

model_7 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_7, stars = T)


################################################################################
# Cottage country statistical significance testing
################################################################################

# done in 54.Final results code.

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

#price_lo <- 56596.6
#price_high <- 1008363.7 



#df_main <- filter(df_main, price_real >= price_lo )
#df_main <- filter(df_main, price_real <= price_high )

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
library(dplyr)

hwydist <- st_read("./shapefile/house_n400hwy_meters.shp")


hwydist <- hwydist%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(roll_num,NEAR_DIST)%>%
  rename("NEAR_DIST_HWY"="NEAR_DIST")

hwydist$roll_num <- as.numeric(hwydist$roll_num)

df_main <- df_main%>%
  left_join(hwydist)

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

cm_limited <- c('log_sd_inter_500'    = 'Log(SD)X500m',
                'log_sd_inter_waterfront' = 'Log(SD)XWaterfront',
                'log_sd_inter_250'    = 'Log(SD)X250m',
                'log_sd_inter_250_500' = 'Log(SD)X250m-500m',
                'log_sd'    = 'Log(SD)',
                'log_sd_inter'    = 'Log(SD)',
                'log_sd_500'    = 'Log(SD)X500m',
                'log_sd_inter_250_gl' = 'GL log(SD)X250m',
                'log_sd_inter_250_gl_no' = 'Non-GL log(SD)X250m',        
                'log_sd_inter_250_500_gl'    = 'GL Log(SD)X250m-500m',
                'log_sd_inter_250_500_gl_no'    = 'Non-GL Log(SD)X250-500m',
                'log(sd_interp):dist:: (0-100)'= 'ln SD X Dist(0-100)',
                'log(sd_interp):dist:: (100-200)'= 'ln SD X Dist(100-200)',
                'log(sd_interp):dist:: (200-300)'= 'ln SD X Dist(200-300)',
                'log(sd_interp):dist:: (300-400)'= 'ln SD X Dist(300-400)',
                'log(sd_interp):dist:: (500+)'= 'ln SD X Dist(500+)',
                'log_sd_250_500' = 'Log(SD)X250m-500m',
                'log_sd_inter_0_250' = 'Log(SD)X0m-250m',
                'sd_inter_500' = 'SDX500m',
                'sd_inter_250' = 'SDX0m-250m',
                'sd_inter_250_500' = 'SDX250m-500m',
                'log_sd_inter_100' = 'SDX0m-100m',
                'log_sd_inter_100_200' = 'SDX100m-200m',
                'log_sd_inter_200_300' = 'SDX200m-300m',
                'log_sd_inter_300_400' = 'SDX300m-400m',
                'log_sd_inter_400_500' = 'SDX400m-500m',
                'log_sd_inter_250_wf_ex' = 'Log(SD)X250m',
                'rob_log_sd_inter_0_50' = 'Log(SD)X50m',
                'rob_log_sd_inter_50_200' = 'Log(SD)X50m-200m',
                'rob_log_sd_inter_0_100' = 'Log(SD)X100m',
                'rob_log_sd_inter_100_200' = 'Log(SD)X100m-200m',
                'rob_log_sd_inter_100_300' = 'Log(SD)X100m-300m',
                'rob_log_sd_inter_0_200' = 'Log(SD)X200m',
                'rob_log_sd_inter_200_500' = 'Log(SD)X200m-500m',
                'rob_log_sd_inter_0_300' = 'Log(SD)X300m',
                'rob_log_sd_inter_300_1000' = 'Log(SD)X300m-1000m',
                'log_sd_250_cottage' = 'Log(SD)X250mXCottage country',
                'log_sd_250_500_cottage' = 'Log(SD)X250m-500mXCottage country'
)







cm_struc<- c(
  'lotsize' = 'Lot size (m2)',  
  "log_area_tot" = "Total area (m2)",
  "prop_type" = 'Property type',
  "pool" = 'Presence of pool (Yes/No)', 
  "air_cond" = 'Presence of air condition', 
  "heat_elect" = "Pelectric heat (Yes/No)", 
  "heat_forcedair" = 'Heat type: Force Air' , 
  "heat_hw" = 'Heat type: Hot water',
  "floodp_d" = 'Floodplain – Developable (Yes/No)', 
  "floodp_r" = 'Floodplain – Restricted (Yes/No)', 
  "fireplcs" = "Presence of fire place (Yes/No)",
  "baths" = "Number of baths",
  "bedrooms" = "Number of bedrooms", 
  "storeys" = "Total Number of Storeys",
  "quality" = "Quality",
  "lbasement_area" = "log (Area of basement (m2))", 
  'age' = " Age",
  'bedrooms_dum'= "Bedroom dummy",
  'baths_dum' = "Bathroo dummy"
)

cm_neigh <- c("view_pr" = "Predominant View – Obstructed (Yes/N0)",
              "view_gd" = "Predominant View- Panoramic (Yes/No)" , 
              "view_wf" = "Presence of waterfront view (Yes/No)",
              "wtr_l_r" = "Lake waterfront (Yes/No)", 
              "wtr_well" = "Private Well (Yes/No)", 
              "wtr_mun" = "Municipal water supply (Yes/No)",
              "wfront_l" = ". Lake waterfront (Yes/No)",
              "wfront_r" = "River Waterfront (Yes/No)",
              "ab_ind" = "Abuts Industrial (Yes/No)",
              "ab_comm" = "Abuts Commercial (Yes/No)",
              "ab_inst" = "Abuts Institutional (Yes/No)",
              "ab_educ" = "AAbuts Educational Institution (Yes/No)",
              "ab_golf" = "Abuts Golf Course (Yes/No)")

cm_envi <- c(        
  "log_dist_toronto" = "log(Distance to Toronto (km))", 
  "log_dist_cma" = "log(Distance to nearest CMA (km))")


################################################################################
# Table 2 - Basic model results
################################################################################

fes_ward = "| ward^year + sale_month"


x_500_inter = paste("log(price_real)  ~ log_sd_inter_500  + dist_500m +",
                    paste0(x_struct, collapse=" + "))
x_distbuf_2 = paste("log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500+ 
                  dist_250m + dist_250m_500m +",
                    paste0(x_struct, collapse=" + "))

x_distbuf_2_wf_sig_check = paste("log(price_real)  ~  log_sd_inter_250 + log_sd_inter_500 + 
                  dist_250m + dist_250m_500m +",
                                 paste0(x_struct, collapse=" + "))



# Repeat sales
df_repeat = df_main %>%
  dplyr::select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
                log_sd_250,log_sd_250_500, log_sd_inter_250,log_sd_inter_250_500, dist_250m, dist_250m_500m,
                log_sd_inter_250_wf_ex,log_sd_inter_waterfront)

df_repeat = df_subset  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


models_fes = list(
  model_1 = feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward),
  model_2 = feols(as.formula(paste(x_distbuf_2, fes_ward)), df_main, vcov = ~ward),
  model_3 = feols(as.formula(paste(x_distbuf_2_wf_sig_check, fes_ward)), df_main, vcov = ~ward),
  model_4 = df_repeat  %>%
    feols(log(price_real)  ~  log_sd_inter_500  + age | year + sale_month +roll_num, ., vcov = ~roll_num),
  model_5 = df_repeat  %>%
    feols(log(price_real)  ~  log_sd_inter_250 + log_sd_inter_250_500  + age | year + sale_month +roll_num, ., vcov = ~roll_num)
)

modelsummary(models_fes, 
             coef_map = cm_limited, 
             #gof_map = gm,
             stars = T)

modelsummary(models_fes, 
             coef_map = cm_limited, 
             #gof_map = gm,
             stars = T,
             output = "./results/A_table_wo_p_outlier.docx")

#############################################################################
# Covid 19 - impact 
###########################################################################

# filter transaction happen up to 2019


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
quantile(df_main$price_real,probs=c(0.01,0.99))

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
library(dplyr)

hwydist <- st_read("./shapefile/house_n400hwy_meters.shp")


hwydist <- hwydist%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(roll_num,NEAR_DIST)%>%
  rename("NEAR_DIST_HWY"="NEAR_DIST")

hwydist$roll_num <- as.numeric(hwydist$roll_num)

df_main <- df_main%>%
  left_join(hwydist)


# filter year < 2020

df_main <- df_main%>%
  filter(year < 2020)


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

cm_limited <- c('log_sd_inter_500'    = 'Log(SD)X500m',
                'log_sd_inter_waterfront' = 'Log(SD)XWaterfront',
                'log_sd_inter_250'    = 'Log(SD)X250m',
                'log_sd_inter_250_500' = 'Log(SD)X250m-500m',
                'log_sd'    = 'Log(SD)',
                'log_sd_inter'    = 'Log(SD)',
                'log_sd_500'    = 'Log(SD)X500m',
                'log_sd_inter_250_gl' = 'GL log(SD)X250m',
                'log_sd_inter_250_gl_no' = 'Non-GL log(SD)X250m',        
                'log_sd_inter_250_500_gl'    = 'GL Log(SD)X250m-500m',
                'log_sd_inter_250_500_gl_no'    = 'Non-GL Log(SD)X250-500m',
                'log(sd_interp):dist:: (0-100)'= 'ln SD X Dist(0-100)',
                'log(sd_interp):dist:: (100-200)'= 'ln SD X Dist(100-200)',
                'log(sd_interp):dist:: (200-300)'= 'ln SD X Dist(200-300)',
                'log(sd_interp):dist:: (300-400)'= 'ln SD X Dist(300-400)',
                'log(sd_interp):dist:: (500+)'= 'ln SD X Dist(500+)',
                'log_sd_250_500' = 'Log(SD)X250m-500m',
                'log_sd_inter_0_250' = 'Log(SD)X0m-250m',
                'sd_inter_500' = 'SDX500m',
                'sd_inter_250' = 'SDX0m-250m',
                'sd_inter_250_500' = 'SDX250m-500m',
                'log_sd_inter_100' = 'SDX0m-100m',
                'log_sd_inter_100_200' = 'SDX100m-200m',
                'log_sd_inter_200_300' = 'SDX200m-300m',
                'log_sd_inter_300_400' = 'SDX300m-400m',
                'log_sd_inter_400_500' = 'SDX400m-500m',
                'log_sd_inter_250_wf_ex' = 'Log(SD)X250m',
                'rob_log_sd_inter_0_50' = 'Log(SD)X50m',
                'rob_log_sd_inter_50_200' = 'Log(SD)X50m-200m',
                'rob_log_sd_inter_0_100' = 'Log(SD)X100m',
                'rob_log_sd_inter_100_200' = 'Log(SD)X100m-200m',
                'rob_log_sd_inter_100_300' = 'Log(SD)X100m-300m',
                'rob_log_sd_inter_0_200' = 'Log(SD)X200m',
                'rob_log_sd_inter_200_500' = 'Log(SD)X200m-500m',
                'rob_log_sd_inter_0_300' = 'Log(SD)X300m',
                'rob_log_sd_inter_300_1000' = 'Log(SD)X300m-1000m',
                'log_sd_250_cottage' = 'Log(SD)X250mXCottage country',
                'log_sd_250_500_cottage' = 'Log(SD)X250m-500mXCottage country'
)







cm_struc<- c(
  'lotsize' = 'Lot size (m2)',  
  "log_area_tot" = "Total area (m2)",
  "prop_type" = 'Property type',
  "pool" = 'Presence of pool (Yes/No)', 
  "air_cond" = 'Presence of air condition', 
  "heat_elect" = "Pelectric heat (Yes/No)", 
  "heat_forcedair" = 'Heat type: Force Air' , 
  "heat_hw" = 'Heat type: Hot water',
  "floodp_d" = 'Floodplain – Developable (Yes/No)', 
  "floodp_r" = 'Floodplain – Restricted (Yes/No)', 
  "fireplcs" = "Presence of fire place (Yes/No)",
  "baths" = "Number of baths",
  "bedrooms" = "Number of bedrooms", 
  "storeys" = "Total Number of Storeys",
  "quality" = "Quality",
  "lbasement_area" = "log (Area of basement (m2))", 
  'age' = " Age",
  'bedrooms_dum'= "Bedroom dummy",
  'baths_dum' = "Bathroo dummy"
)

cm_neigh <- c("view_pr" = "Predominant View – Obstructed (Yes/N0)",
              "view_gd" = "Predominant View- Panoramic (Yes/No)" , 
              "view_wf" = "Presence of waterfront view (Yes/No)",
              "wtr_l_r" = "Lake waterfront (Yes/No)", 
              "wtr_well" = "Private Well (Yes/No)", 
              "wtr_mun" = "Municipal water supply (Yes/No)",
              "wfront_l" = ". Lake waterfront (Yes/No)",
              "wfront_r" = "River Waterfront (Yes/No)",
              "ab_ind" = "Abuts Industrial (Yes/No)",
              "ab_comm" = "Abuts Commercial (Yes/No)",
              "ab_inst" = "Abuts Institutional (Yes/No)",
              "ab_educ" = "AAbuts Educational Institution (Yes/No)",
              "ab_golf" = "Abuts Golf Course (Yes/No)")

cm_envi <- c(        
  "log_dist_toronto" = "log(Distance to Toronto (km))", 
  "log_dist_cma" = "log(Distance to nearest CMA (km))")


################################################################################
# Table 2 - Basic model results
################################################################################

fes_ward = "| ward^year + sale_month"


x_500_inter = paste("log(price_real)  ~ log_sd_inter_500  + dist_500m +",
                    paste0(x_struct, collapse=" + "))
x_distbuf_2 = paste("log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500+ 
                  dist_250m + dist_250m_500m +",
                    paste0(x_struct, collapse=" + "))

x_distbuf_2_wf_sig_check = paste("log(price_real)  ~  log_sd_inter_250 + log_sd_inter_500 + 
                  dist_250m + dist_250m_500m +",
                                 paste0(x_struct, collapse=" + "))



# Repeat sales
df_repeat = df_main %>%
  dplyr::select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
                log_sd_250,log_sd_250_500, log_sd_inter_250,log_sd_inter_250_500, dist_250m, dist_250m_500m,
                log_sd_inter_250_wf_ex,log_sd_inter_waterfront)

df_repeat = df_subset  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


models_fes = list(
  model_1 = feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward),
  model_2 = feols(as.formula(paste(x_distbuf_2, fes_ward)), df_main, vcov = ~ward),
  model_3 = feols(as.formula(paste(x_distbuf_2_wf_sig_check, fes_ward)), df_main, vcov = ~ward),
  model_4 = df_repeat  %>%
    feols(log(price_real)  ~  log_sd_inter_500  + age | year + sale_month +roll_num, ., vcov = ~roll_num),
  model_5 = df_repeat  %>%
    feols(log(price_real)  ~  log_sd_inter_250 + log_sd_inter_250_500  + age | year + sale_month +roll_num, ., vcov = ~roll_num)
)

modelsummary(models_fes, 
             coef_map = cm_limited, 
             #gof_map = gm,
             stars = T)

modelsummary(models_fes, 
             coef_map = cm_limited, 
             #gof_map = gm,
             stars = T,
             output = "./results/A_table_covid.docx")


##############################################################################
# VIF
#############################################################################

library(olsrr)


model_1 <- lm(log(price_real)  ~ log_sd_inter_500 + dist_500m +lotsize +
                log_dist_toronto+ log_dist_cma+prop_type+pool+ air_cond+ heat_elect+ heat_forcedair+ 
                heat_hw+floodp_d+ floodp_r+ view_pr+view_gd+ view_wf+wtr_l_r+ wtr_well+ wtr_mun+
                wfront_l+wfront_r+ab_ind+ab_comm+ab_inst+ab_educ+ab_golf+fireplcs+baths+
                bedrooms+ storeys+lbasement_area+age+bedrooms_dum+baths_dum+NEAR_DIST_HWY, df_main)


p <- ols_coll_diag(model_1)

p1 <- p$vif_t%>%
  as.data.frame()

mean(p1$VIF)

###############################################################################

# distance to hwy-400 as control variable


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
quantile(df_main$price_real,probs=c(0.01,0.99))

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

# load file with distance to hwy400 

library(sf)

hwydist <- st_read("./shapefile/house_n400hwy_meters.shp")

hwydist <- hwydist%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(roll_num,NEAR_DIST)%>%
  rename("NEAR_DIST_HWY"="NEAR_DIST")

hwydist$roll_num <- as.numeric(hwydist$roll_num)

df_main <- df_main%>%
  left_join(hwydist)


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

cm_limited <- c('log_sd_inter_500'    = 'Log(SD)X500m',
                'log_sd_inter_waterfront' = 'Log(SD)XWaterfront',
                'log_sd_inter_250'    = 'Log(SD)X250m',
                'log_sd_inter_250_500' = 'Log(SD)X250m-500m',
                'log_sd'    = 'Log(SD)',
                'log_sd_inter'    = 'Log(SD)',
                'log_sd_500'    = 'Log(SD)X500m',
                'log_sd_inter_250_gl' = 'GL log(SD)X250m',
                'log_sd_inter_250_gl_no' = 'Non-GL log(SD)X250m',        
                'log_sd_inter_250_500_gl'    = 'GL Log(SD)X250m-500m',
                'log_sd_inter_250_500_gl_no'    = 'Non-GL Log(SD)X250-500m',
                'log(sd_interp):dist:: (0-100)'= 'ln SD X Dist(0-100)',
                'log(sd_interp):dist:: (100-200)'= 'ln SD X Dist(100-200)',
                'log(sd_interp):dist:: (200-300)'= 'ln SD X Dist(200-300)',
                'log(sd_interp):dist:: (300-400)'= 'ln SD X Dist(300-400)',
                'log(sd_interp):dist:: (500+)'= 'ln SD X Dist(500+)',
                'log_sd_250_500' = 'Log(SD)X250m-500m',
                'log_sd_inter_0_250' = 'Log(SD)X0m-250m',
                'sd_inter_500' = 'SDX500m',
                'sd_inter_250' = 'SDX0m-250m',
                'sd_inter_250_500' = 'SDX250m-500m',
                'log_sd_inter_100' = 'SDX0m-100m',
                'log_sd_inter_100_200' = 'SDX100m-200m',
                'log_sd_inter_200_300' = 'SDX200m-300m',
                'log_sd_inter_300_400' = 'SDX300m-400m',
                'log_sd_inter_400_500' = 'SDX400m-500m',
                'log_sd_inter_250_wf_ex' = 'Log(SD)X250m',
                'rob_log_sd_inter_0_50' = 'Log(SD)X50m',
                'rob_log_sd_inter_50_200' = 'Log(SD)X50m-200m',
                'rob_log_sd_inter_0_100' = 'Log(SD)X100m',
                'rob_log_sd_inter_100_200' = 'Log(SD)X100m-200m',
                'rob_log_sd_inter_100_300' = 'Log(SD)X100m-300m',
                'rob_log_sd_inter_0_200' = 'Log(SD)X200m',
                'rob_log_sd_inter_200_500' = 'Log(SD)X200m-500m',
                'rob_log_sd_inter_0_300' = 'Log(SD)X300m',
                'rob_log_sd_inter_300_1000' = 'Log(SD)X300m-1000m',
                'log_sd_250_cottage' = 'Log(SD)X250mXCottage country',
                'log_sd_250_500_cottage' = 'Log(SD)X250m-500mXCottage country'
)







cm_struc<- c(
  'lotsize' = 'Lot size (m2)',  
  "log_area_tot" = "Total area (m2)",
  "prop_type" = 'Property type',
  "pool" = 'Presence of pool (Yes/No)', 
  "air_cond" = 'Presence of air condition', 
  "heat_elect" = "Pelectric heat (Yes/No)", 
  "heat_forcedair" = 'Heat type: Force Air' , 
  "heat_hw" = 'Heat type: Hot water',
  "floodp_d" = 'Floodplain – Developable (Yes/No)', 
  "floodp_r" = 'Floodplain – Restricted (Yes/No)', 
  "fireplcs" = "Presence of fire place (Yes/No)",
  "baths" = "Number of baths",
  "bedrooms" = "Number of bedrooms", 
  "storeys" = "Total Number of Storeys",
  "quality" = "Quality",
  "lbasement_area" = "log (Area of basement (m2))", 
  'age' = " Age",
  'bedrooms_dum'= "Bedroom dummy",
  'baths_dum' = "Bathroo dummy"
)

cm_neigh <- c("view_pr" = "Predominant View – Obstructed (Yes/N0)",
              "view_gd" = "Predominant View- Panoramic (Yes/No)" , 
              "view_wf" = "Presence of waterfront view (Yes/No)",
              "wtr_l_r" = "Lake waterfront (Yes/No)", 
              "wtr_well" = "Private Well (Yes/No)", 
              "wtr_mun" = "Municipal water supply (Yes/No)",
              "wfront_l" = ". Lake waterfront (Yes/No)",
              "wfront_r" = "River Waterfront (Yes/No)",
              "ab_ind" = "Abuts Industrial (Yes/No)",
              "ab_comm" = "Abuts Commercial (Yes/No)",
              "ab_inst" = "Abuts Institutional (Yes/No)",
              "ab_educ" = "AAbuts Educational Institution (Yes/No)",
              "ab_golf" = "Abuts Golf Course (Yes/No)")

cm_envi <- c(        
  "log_dist_toronto" = "log(Distance to Toronto (km))", 
  "log_dist_cma" = "log(Distance to nearest CMA (km))")


################################################################################
# Table 2 - Basic model results
################################################################################

fes_ward = "| ward^year + sale_month"


x_500_inter = paste("log(price_real)  ~ log_sd_inter_500  + dist_500m +",
                    paste0(x_struct, collapse=" + "))
x_distbuf_2 = paste("log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500+ 
                  dist_250m + dist_250m_500m +",
                    paste0(x_struct, collapse=" + "))

x_distbuf_2_wf_sig_check = paste("log(price_real)  ~  log_sd_inter_250 + log_sd_inter_500 + 
                  dist_250m + dist_250m_500m +",
                                 paste0(x_struct, collapse=" + "))



# Repeat sales
df_repeat = df_main %>%
  dplyr::select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
                log_sd_250,log_sd_250_500, log_sd_inter_250,log_sd_inter_250_500, dist_250m, dist_250m_500m,
                log_sd_inter_250_wf_ex,log_sd_inter_waterfront)

df_repeat = df_subset  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


models_fes = list(
  model_1 = feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward),
  model_2 = feols(as.formula(paste(x_distbuf_2, fes_ward)), df_main, vcov = ~ward),
  model_3 = feols(as.formula(paste(x_distbuf_2_wf_sig_check, fes_ward)), df_main, vcov = ~ward),
  model_4 = df_repeat  %>%
    feols(log(price_real)  ~  log_sd_inter_500  + age | year + sale_month +roll_num, ., vcov = ~roll_num),
  model_5 = df_repeat  %>%
    feols(log(price_real)  ~  log_sd_inter_250 + log_sd_inter_250_500  + age | year + sale_month +roll_num, ., vcov = ~roll_num)
)

modelsummary(models_fes, 
             coef_map = cm_limited, 
             #gof_map = gm,
             stars = T)


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

quantile(df_main$price_real,probs=c(0.035,0.95))
#quantile(df_main$price_nominal,probs=c(0.01,0.9))


price_lo <-  57365.29 #56596.6
price_high <- 915252.33  #1008363.7   


df_main <- filter(df_main, price_real >= price_lo )
df_main <- filter(df_main, price_real <= price_high )

# deal with missing lotsize
# replace missing lot size with average lot size

mean(df_main$lotsize) #1260.281

df_main$lotsize[df_main$lotsize == "0"] <- 1260.281



# deal with missing totalarea
# replace missing lot size with average lot size

mean(df_main$area_tot, na.rm = T) #1569.626

df_main$area_tot[df_main$area_tot == "0"] <- 1569.626


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


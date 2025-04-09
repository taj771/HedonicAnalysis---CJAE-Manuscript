
# log_sd_interp: log of sd with +/- 5 years of data (can change in CreateAnalysisSample)
# log_sd_###: log of sd with same year sd data and distance threshold by #'s

rm(list=ls(all=TRUE))

library(readr)
library(fixest)
library(dplyr)
library(modelsummary)


df_subset = readRDS("data/analysis_sample.rds")


sales_per_region = df_subset  %>%
  group_by(economic_region, region, county, township, ward) %>%
  tally(.)

df_subset %>%
  mutate(sd_average_miss = ifelse(is.na(sd_average), 1, 0),
         sd_interp_miss = ifelse(is.na(sd_interp), 1, 0),
         log_sd_500_miss = ifelse(is.na(log_sd_500), 1, 0),
         log_sd_inter_500_miss = ifelse(is.na(log_sd_inter_500), 1, 0)) %>%
  group_by(log_sd_500, log_sd_inter_500) %>%
  summarise(count = n())

#summary(tt)
#---------
x_struct = c("lotsize",  "log_area_tot",
             "log_dist_toronto", "log_dist_cma",
               "prop_type",
"pool", "air_cond", "heat_elect", "heat_forcedair", "heat_hw",
#             "floodp_d", "floodp_r", 
 #            "view_pr","view_gd", "view_wf",
 #            "wtr_l_r", "wtr_well", "wtr_mun",
#, "wfront_l",# "wfront_r",
"fireplcs", "baths", "bedrooms", "storeys",
             "quality","lbasement_area")#,#,
        #        "ab_ind","ab_comm","ab_inst","ab_educ","ab_golf")#,
        #     "lgar_area")

df_main = df_subset %>%
  mutate(log_sd_inter_500 = ifelse(abs(parse_number(interpolation)) > 5 & 
                                     log_sd_inter_500 != 0, 
                          NA, log_sd_inter_500))

tt = df_main %>%
  group_by(interpolation) %>%
  summarise(count =n(),
            sd = mean(log_sd_inter_500, na.rm = T)) %>%
  arrange(-count)


x_500 = paste("log(price_real)  ~ log_sd_500 + inverse_dist + dist_500m +",
              paste0(x_struct, collapse=" + "))

x_500_gl = paste("log(price_real)  ~ log_sd_500_gl + log_sd_500_gl_no + inverse_dist + dist_500m +",
              paste0(x_struct, collapse=" + "))

x_500_second = paste("log(price_real)  ~ log_sd_inter_500 + inverse_dist + dist_500m +",
              paste0(x_struct, collapse=" + "))

x_front = paste("log(price_real)  ~ log_sd_front + inverse_dist + waterfront +",
                   paste0(x_struct, collapse=" + "))

x_250 = paste("log(price_real)  ~ log_sd_250 + inverse_dist + dist_250m +",
              paste0(x_struct, collapse=" + "))

x_1000 = paste("log(price_real)  ~ log_sd_1000 + inverse_dist + dist_1000m +",
              paste0(x_struct, collapse=" + "))

x_repeat_sales = paste("log(price_real)  ~ log_sd_500 + age")

fes_ward = "| ward^year + sale_month"

models_fes = list(
  county = feols(as.formula(paste(x_500, "| county^year + sale_month")), df_main, vcov = ~county),
  township = feols(as.formula(paste(x_500, "| township^year + sale_month")), df_main, vcov = ~township),
  ward = feols(as.formula(paste(x_500, fes_ward)), df_main, vcov = ~township),
  ward_second = feols(as.formula(paste(x_500_second, fes_ward)), df_main, vcov = ~township)
)



models_sens = list(
  base = feols(as.formula(paste(x_500, fes_ward)), df_main, vcov = ~township),
  mod_house = feols(as.formula(paste(x_500, fes_ward)), 
                    subset(df_main, house == 1), vcov = ~township),
  mod_nonhouse = feols(as.formula(paste(x_500, fes_ward)), 
                    subset(df_main, house == 0), vcov = ~township),  
  mod_regionlakes = feols(as.formula(paste(x_500, fes_ward)), 
                    subset(df_main, region_lakes == 1), vcov = ~township),
  mod_nonregionlakes = feols(as.formula(paste(x_500, fes_ward)), 
                      subset(df_main, region_lakes == 0), vcov = ~township)
)


models_max_lake_distance = list(
  base = feols(as.formula(paste(x_500, fes_ward)), df_main, vcov = ~township),
  mod_1000m_var = feols(as.formula(paste(x_500, " + factor(dist_house_lake <= 1000)", fes_ward)), 
                    df_main, vcov = ~township),
  mod_1000m = feols(as.formula(paste(x_500, fes_ward)), 
               subset(df_main, dist_house_lake <= 1000), vcov = ~township),
  mod_2500m = feols(as.formula(paste(x_500, fes_ward)), 
                    subset(df_main, dist_house_lake <= 2500), vcov = ~township)
)

cm <- c('log_sd_500'    = 'Log(SD)',
        'log_sd_500_gl'    = 'GL Log(SD)',
        'log_sd_500_gl_no'    = 'Non-GL Log(SD)',
        'log_sd_inter_500'    = 'Log(SD)',
        'log_sd_front'    = 'Log(SD)',
        'log_sd_250'    = 'Log(SD)',
        'log_sd_1000'    = 'Log(SD)')#,
     #   'inverse_dist'    = 'Inverse Distance',
    #    'dist_500m' = 'Near lake (500m)')

f <- function(x) format(round(x, 3), big.mark=",")

gm <- list(
  list("raw" = "nobs", "clean" = "Homes", "fmt" = f),
  list("raw" = "R2", "clean" = "R2", "fmt" = 3))

modelsummary(models_fes, 
             coef_map = cm, 
             gof_map = gm,
             stars = T)


modelsummary(models_max_lake_distance, 
             coef_map = cm, 
             gof_map = gm)

modelsummary(models_sens, 
             coef_map = cm, 
             gof_map = gm)



models = list(
  modfront = feols(as.formula(paste(x_front, fes_ward)), subset(df_main), vcov = ~township),
  mod250m = feols(as.formula(paste(x_250, fes_ward)), subset(df_main), vcov = ~township),
  mod500m = feols(as.formula(paste(x_500, fes_ward)), subset(df_main), vcov = ~township),
  mod1000m = feols(as.formula(paste(x_1000, fes_ward)), subset(df_main), vcov = ~township)
)


modelsummary(models, 
             coef_map = cm, 
             gof_map = gm)

# repeat sales
df_repeat = df_subset %>%
  select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
         log_sd_front, log_sd_500, log_sd_inter_500, log_sd_1000, log_sd_500_gl, log_sd_500_gl_no, region)

df_repeat = df_repeat  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


models_repeat_sales = list(
  normal = df_repeat  %>%
    feols(log(price_real)  ~ log_sd_500+ age | year + sale_month +roll_num, ., vcov = ~roll_num),
  
  interp = df_repeat  %>%
    feols(log(price_real)  ~ log_sd_inter_500+ age | year + sale_month +roll_num, ., vcov = ~roll_num)
)

modelsummary(models_repeat_sales, 
             coef_map = cm, 
             gof_map = gm)



# Binned regressions
start_number = 0
end_number = 5000
by_number = 250
breaks1 = seq(start_number, end_number, by = by_number)  
breaks1 = c(breaks1[-length(breaks1)],Inf)
#breaks1 = c(-Inf,breaks1)

names1 = paste0(rep(" (",length(breaks1)-1),
                breaks1[-1]-by_number, "-", breaks1[-1], ")")

names1 = c(names1[-length(names1)], paste0(" (",end_number,"+)"))

x_500_dummy = paste("log(price_real)  ~log(sd_interp):i(dist) + dist +", #
                    paste0(x_struct, collapse=" + "))


model1 =  df_main %>%
  mutate(dist = cut(dist_house_lake,
                    breaks=breaks1,
                    labels=names1)) %>%
  feols(as.formula(paste(x_500_dummy, fes_ward)), ., vcov = ~township)
etable(model1)


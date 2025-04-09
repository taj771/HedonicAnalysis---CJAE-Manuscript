# clear memory
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  fixest,
  modelsummary
)
df_subset = readRDS("data/analysis_sample.rds")

interp_lo = -5
interp_hi = 5

df_main <- df_subset%>%
  mutate(sd_extrap_years = str_remove(interpolation, "year"),
         sd_extrap_years = replace_na(as.numeric(sd_extrap_years), 0))

df_main <- filter(df_main, sd_extrap_years >= interp_lo )
df_main <- filter(df_main, sd_extrap_years <= interp_hi )


# property characteristics
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
             #"floodp_d", 
             #"floodp_r", 
             #"view_pr",
             #"view_gd", 
             #"view_wf",
             #"wtr_l_r", 
             #"wtr_well", 
             #"wtr_mun",
             #"wfront_l",
             # "wfront_r",
             #"ab_ind",
             #"ab_comm",
             #"ab_inst",
             #"ab_educ",
             #"ab_golf",
             #"lgar_area",
             "fireplcs",
             "baths",
             "bedrooms", 
             "storeys",
             "quality",
             "lbasement_area",
             'age')

cm <- c('log_sd' = 'ln SD',
        'log_sd_inter:waterfront' = 'ln SD X 0-100m', 
        'log_sd_inter:dist_100m_500m' = 'ln SD X 100-500m',
        'log_sd:waterfront' = 'ln SD X 0-100m',
        'log_sd:dist_100m_500m' = 'ln SD X 100-500m',
        'log_sd_inter' = 'ln SD',
        'log_sd_front'='ln SD X 0-100m',
        'log_sd_inter_front_200' = 'ln SD X 100-200m',
        'log_sd_inter_200_300 ' = 'ln SD X 200-300m',
        'log_sd_inter_300_400' = 'ln SD X 200-400m',
        'log_sd_inter_400_500' = 'ln SD X 200-500m',
        'log_sd_inter_front' = 'ln SD X 0-100m',
        'log_sd_inter_250' = 'ln SD X dist_250m',
        'log_sd_inter_500'='ln SD X dist_500m',
        'log_sd_inter_10_500' = 'ln SD X dist_500m',
        'log_sd_inter_15_500' = 'ln SD X dist_500m',
        'log_sd_inter_20_500' = 'ln SD X dist_500m',
        'log_sd_inter_25_500' = 'ln SD X dist_500m',
        'log_sd_inter_1000' = 'ln SD X dist_1000m',
        'log_sd_500' = 'ln SD X dist_500m',
        'log_sd_250' = 'ln SD X dist_250m',
        'log_sd_1000' = 'ln SD X dist_1000m',
        'log_sd_front_500' = 'Ln SD X 100m-500m',
        'log_sd_front_200' = 'Ln SD X 100m-200m',
        'log_sd_inter_front_300' = 'Ln SD X 100m-300m',
        'log_sd_inter_front_400' = 'Ln SD X 100m-400m',
        'log_sd_inter_front_500' = 'Ln SD X 100m-500m',
        'log_sd_inter_front_1000' = 'Ln SD X 100m-1000m',
        'log_sd_inter:dist_100m_200m' = 'Ln SD X 100m-200m',
        'log_sd_inter_2000'='ln SD',
        'log_sd_250_gl'='ln SD GL',
        'log_sd_250_gl_no'='ln SD N-GL',
        'log_sd_500_gl'='ln SD GL',
        'log_sd_500_gl_no'='ln SD N-GL',
        'log_sd_1000_gl'='ln SD GL',
        'log_sd_1000_gl_no'='ln SD N-GL',
        'log_sd_2000_gl'='ln SD GL',
        'log_sd_2000_gl_no'='ln SD N-GL',
        'log_sd_inter_250_gl'='ln SD GL',
        'log_sd_inter_250_gl_no'='ln SD N-GL',
        'log_sd_inter_500_gl'='ln SD GL',
        'log_sd_inter_500_gl_no'='ln SD N-GL',
        'log_sd_inter_1000_gl'='ln SD GL',
        'log_sd_inter_1000_gl_no'='ln SD N-GL',
        'log_sd_inter_2000_gl'='ln SD GL',
        'log_sd_inter_2000_gl_no'='ln SD N-GL',
        'log_sd_250:log_lakearea'='ln SD X ln LA',
        'log_sd_500:log_lakearea' ='ln SD X ln LA',
        'log_sd_1000:log_lakearea' ='ln SD X ln LA',
        'log_sd_2000:log_lakearea' ='ln SD X ln LA',
        'log_sd_inter_250:log_lakearea'='ln SD X ln LA',
        'log_sd_inter_500:log_lakearea'='ln SD X ln LA',
        'log_sd_inter_1000:log_lakearea'='ln SD X ln LA',
        'log_sd_inter_2000:log_lakearea'='ln SD X ln LA',
        'log_sd_250:waterfront'='ln SD X WF',
        'log_sd_500:waterfront' ='ln SD X WF',
        'log_sd_1000:waterfront' ='ln SD X WF',
        'log_sd_2000:waterfront' ='ln SD X WF',
        'log_sd_inter_250:waterfront'='ln SD X WF',
        'log_sd_inter_500:waterfront'='ln SD X WF',
        'log_sd_inter_1000:waterfront'='ln SD X WF',
        'log_sd_inter_2000:waterfront'='ln SD X WF',
        'log_sd_inter_250_area'= 'ln SD X ln LA',
        'log_sd_inter_500_area'= 'ln SD X ln LA',
        'log_sd_inter_1000_area'= 'ln SD X ln LA',
        'log_sd_inter_2000_area'= 'ln SD X ln LA',
        'log_sd_250_area'='ln SD X ln LA',
        'log_sd_500_area'='ln SD X ln LA',
        'log_sd_1000_area'='ln SD X ln LA',
        'log_sd_2000_area'='ln SD X ln LA',
        ' waterfront:log(sd_interp)'= 'ln SD X Waterfront',
        'log(sd_interp):dist:: (0-100)'= 'ln SD X Dist(0-100)',
        'log(sd_interp):dist:: (100-200)'= 'ln SD X Dist(100-200)',
        'log(sd_interp):dist:: (200-300)'= 'ln SD X Dist(200-300)',
        'log(sd_interp):dist:: (300-400)'= 'ln SD X Dist(300-400)',
        'log(sd_interp):dist:: (400-500)'= 'ln SD X Dist(400-500)',
        'log(sd_interp):dist:: (500-600)'= 'ln SD X Dist(500-600)',
        'log(sd_interp):dist:: (600-700)'= 'ln SD X Dist(600-700)',
        'log(sd_interp):dist:: (700-800)'= 'ln SD X Dist(700-800)',
        'log(sd_interp):dist:: (800-900)'= 'ln SD X Dist(800-900)',
        'log(sd_interp):dist:: (900-1000)'= 'ln SD X Dist(900-1000)',
        'log(sd_interp):dist:: (2500-2750)'= 'ln SD X Dist(2250-2750)',
        'log(sd_interp):dist:: (2750-3000)'= 'ln SD X Dist(2750-3000)',
        'log(sd_interp):dist:: (3000-3250)'= 'ln SD X Dist(3000-3250)',
        'log(sd_interp):dist:: (3250-3500)'= 'ln SD X Dist(3250-3500)',
        'log(sd_interp):dist:: (3500-3750)'= 'ln SD X Dist(3500-3750)',
        'log(sd_interp):dist:: (3750-4000)'= 'ln SD X Dist(3750-4000)',
        'log(sd_interp):dist:: (4000-4250)'= 'ln SD X Dist(4000-4250)',
        'log(sd_interp):dist:: (4250-4500)'= 'ln SD X Dist(4250-4500)',
        'log(sd_interp):dist:: (4500-4750)'= 'ln SD X Dist(4500-4750)',
        'log(sd_interp):dist:: (500+)'= 'ln SD X Dist(500+)',
        'log(sd_average):dist:: (0-100)'= 'ln SD X Dist(0-100)',
        'log(sd_average):dist:: (100-200)'= 'ln SD X Dist(100-200)',
        'log(sd_average):dist:: (200-300)'= 'ln SD X Dist(200-300)',
        'log(sd_average):dist:: (300-400)'= 'ln SD X Dist(300-400)',
        'log(sd_average):dist:: (400-500)'= 'ln SD X Dist(400-500)',
        'log(sd_average):dist:: (1250-1500)'= 'ln SD X Dist(1250-1500)',
        'log(sd_average):dist:: (1500-1750)'= 'ln SD X Dist(1500-1750)',
        'log(sd_average):dist:: (1750-2000)'= 'ln SD X Dist(1750-2000)',
        'log(sd_average):dist:: (2000-2250)'= 'ln SD X Dist(2000-2250)',
        'log(sd_average):dist:: (2250-2500)'= 'ln SD X Dist(2250-2500)',
        'log(sd_average):dist:: (2500-2750)'= 'ln SD X Dist(2250-2750)',
        'log(sd_average):dist:: (2750-3000)'= 'ln SD X Dist(2750-3000)',
        'log(sd_average):dist:: (3000-3250)'= 'ln SD X Dist(3000-3250)',
        'log(sd_average):dist:: (3250-3500)'= 'ln SD X Dist(3250-3500)',
        'log(sd_average):dist:: (3500-3750)'= 'ln SD X Dist(3500-3750)',
        'log(sd_average):dist:: (3750-4000)'= 'ln SD X Dist(3750-4000)',
        'log(sd_average):dist:: (4000-4250)'= 'ln SD X Dist(4000-4250)',
        'log(sd_average):dist:: (4250-4500)'= 'ln SD X Dist(4250-4500)',
        'log(sd_average):dist:: (4500-4750)'= 'ln SD X Dist(4500-4750)',
        'log(sd_average):dist:: (500+)'= 'ln SD X Dist(500+)',
        'dist (0-100)'='Distance (0-100)',
        'dist (100-200)'='Distance (100-200)',
        'dist (200-300)'='Distance (200-300)',
        'dist (300-400)'='Distance (300-400)',
        'dist (400-500)'='Distance (400-500)',
        'dist (1500-1750)'='Distance (1500-1750)',
        'dist (1750-2000)'='Distance (1750-2000)',
        'dist (2000-2250)'='Distance (2000-2250)',
        'dist (2250-2500)'='Distance (2250-2500)',
        'dist (2500-2750)'='Distance (2500-2750)',
        'dist (2750-3000)'='Distance (2750-3000)',
        'dist (3000-3250)'='Distance (3000-3250)',
        'dist (3250-3500)'='Distance (3250-3500)',
        'dist (3500-3750)'='Distance (3500-3750)',
        'dist (3750-4000)'='Distance (3750-4000)',
        'dist (4000-4250)'='Distance (4000-4350)',
        'dist (4250-4500)'='Distance (4250-4500)',
        'dist (500+)'='Distance (500+)',
        'waterfront:log(sd_average)' = 'ln SD X Waterfront',
        'waterfront:log(sd_interp)' = 'ln SD X Waterfront',
        'log(sd_average):log_lakearea' = 'Ln SD X LA',
        'log(sd_interp):log_lakearea' = 'Ln SD X LA',
        'waterfront' = 'dist_100',
        'dist_100m_200m' = 'dist_100-200m',
        'dist_200m_300m' = 'dist_200-300m',
        'dist_300m_400m' = 'dist_300-400m',
        'dist_400m_500m' = 'dist_400-500m',
        'inverse_dist'='Inverse Distance',
        'inverse_dist_eq'= 'Inverse Distance',
        'dist_200m_plus' = 'dist_200m_plus',
        'log_sd_inter_200_plus' = 'ln SD X 200m+',
        'dist_300m_plus' = 'dist_300m_plus',
        'log_sd_inter_300_plus' = 'ln SD X 300m+',
        'dist_400m_plus' = 'dist_400m_plus',
        'log_sd_inter_400_plus' = 'ln SD X 400m+',
        'dist_500m_plus' = 'dist_500m_plus',
        'log_sd_inter_500_plus' = 'ln SD X 500m+'
        )


f <- function(x) format(round(x, 3), big.mark=",")

gm <- list(
  list("raw" = "nobs", "clean" = "Homes", "fmt" = f),
  list("raw" = "R2", "clean" = "R2", "fmt" = 3))

###############################################################################
# Model Selection

# Type 1

model_wf_inter = paste("log(price_real)  ~ log_sd_inter_front + log_lakearea + waterfront +",
                        paste0(x_struct, collapse=" + "))
model_wf_ave = paste("log(price_real)  ~ log_sd_front + log_lakearea + waterfront +",
                       paste0(x_struct, collapse=" + "))
model_250_inter = paste("log(price_real)  ~ log_sd_inter_250 + log_lakearea + dist_250m +",
                        paste0(x_struct, collapse=" + "))
model_250_ave = paste("log(price_real)  ~ log_sd_250 + log_lakearea + dist_250m +",
                      paste0(x_struct, collapse=" + "))
model_500_inter = paste("log(price_real)  ~ log_sd_inter_500 +log_lakearea + dist_500m +",
                        paste0(x_struct, collapse=" + "))
model_500_ave = paste("log(price_real)  ~ log_sd_500 + log_lakearea + dist_500m +",
                      paste0(x_struct, collapse=" + "))
model_1000_inter = paste("log(price_real)  ~ log_sd_inter_1000 +log_lakearea + dist_1000m +",
                         paste0(x_struct, collapse=" + "))
model_1000_ave = paste("log(price_real)  ~ log_sd_1000 +log_lakearea + dist_1000m +",
                       paste0(x_struct, collapse=" + "))



models_1 = list(
  model_1 = feols(as.formula(paste(model_wf_inter, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_2 = feols(as.formula(paste(model_wf_ave, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_3 = feols(as.formula(paste(model_250_inter, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_4 = feols(as.formula(paste(model_250_ave, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_5 = feols(as.formula(paste(model_500_inter, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_6 = feols(as.formula(paste(model_500_ave, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_7 = feols(as.formula(paste(model_1000_inter, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_8 = feols(as.formula(paste(model_1000_ave, "| ward^year + sale_month")), df_main, vcov = ~township)
)


modelsummary(models_1, stars = T,coef_map = cm,)

modelsummary(models_1, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table2.tex")



# Type 2

model_250_inter = paste("log(price_real)  ~ log_sd_inter_250 + inverse_dist + waterfront + dist_250m +",
                        paste0(x_struct, collapse=" + "))
model_250_ave = paste("log(price_real)  ~ log_sd_250 + inverse_dist + waterfront + dist_250m +",
                      paste0(x_struct, collapse=" + "))
model_500_inter = paste("log(price_real)  ~ log_sd_inter_500 + inverse_dist + waterfront + dist_500m +",
                  paste0(x_struct, collapse=" + "))
model_500_ave = paste("log(price_real)  ~ log_sd_500 + inverse_dist + waterfront + dist_500m +",
                      paste0(x_struct, collapse=" + "))
model_1000_inter = paste("log(price_real)  ~ log_sd_inter_1000 + inverse_dist + waterfront + dist_1000m +",
                        paste0(x_struct, collapse=" + "))
model_1000_ave = paste("log(price_real)  ~ log_sd_1000 + inverse_dist + waterfront + dist_1000m +",
                      paste0(x_struct, collapse=" + "))



models_2 = list(
  model_1 = feols(as.formula(paste(model_250_inter, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_2 = feols(as.formula(paste(model_250_ave, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_3 = feols(as.formula(paste(model_500_inter, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_4 = feols(as.formula(paste(model_500_ave, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_5 = feols(as.formula(paste(model_1000_inter, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_6 = feols(as.formula(paste(model_1000_ave, "| ward^year + sale_month")), df_main, vcov = ~township)
)

modelsummary(models_2, stars = T,coef_map = cm,)

modelsummary(models_2, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table3.tex")
##########################################################################
# Type 3
# use subset of distance 

models_3 = list(
  model_1 = feols(as.formula(paste(model_500_inter, "| ward^year + sale_month")), subset(df_main, dist_1000m == 1), vcov = ~township),
  model_2 = feols(as.formula(paste(model_500_ave, "| ward^year + sale_month")), subset(df_main, dist_1000m == 1), vcov = ~township),
  model_3 = feols(as.formula(paste(model_500_inter, "| ward^year + sale_month")), subset(df_main, dist_2000m == 1), vcov = ~township),
  model_4 = feols(as.formula(paste(model_500_ave, "| ward^year + sale_month")), subset(df_main, dist_2000m == 1), vcov = ~township)
)


modelsummary(models_3, stars = T,coef_map = cm,)

modelsummary(models_3, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table4.tex")


########################################################################
# Type 4


model_buffer_100_200 = paste("log(price_real)  ~ log_sd_inter  + log_sd_inter_front + log_sd_inter_front_200 + waterfront + dist_100m_200m +",
                           paste0(x_struct, collapse=" + "))
model_buffer_100_300 = paste("log(price_real)  ~ log_sd_inter  + log_sd_inter_front + log_sd_inter_front_300 + waterfront + dist_100m_300m +",
                           paste0(x_struct, collapse=" + "))
model_buffer_100_400 = paste("log(price_real)  ~ log_sd_inter  + log_sd_inter_front + log_sd_inter_front_400 + waterfront + dist_100m_400m +",
                             paste0(x_struct, collapse=" + "))
model_buffer_100_500 = paste("log(price_real)  ~ log_sd_inter  + log_sd_inter_front + log_sd_inter_front_500 + waterfront + dist_100m_500m +",
                             paste0(x_struct, collapse=" + "))
model_buffer_100_1000 = paste("log(price_real)  ~ log_sd_inter  + log_sd_inter_front + log_sd_inter_front_1000 + waterfront + dist_100m_1000m +",
                             paste0(x_struct, collapse=" + "))

models_4 = list(
  model_1 = feols(as.formula(paste(model_buffer_100_200, "| ward^year + sale_month")), subset(df_main, dist_house_lake < 4000), vcov = ~township),
  model_2 = feols(as.formula(paste(model_buffer_100_300, "| ward^year + sale_month")), subset(df_main, dist_house_lake < 4000), vcov = ~township),
  model_3 = feols(as.formula(paste(model_buffer_100_400, "| ward^year + sale_month")), subset(df_main, dist_house_lake < 4000), vcov = ~township),
  model_4 = feols(as.formula(paste(model_buffer_100_500, "| ward^year + sale_month")), subset(df_main, dist_house_lake < 4000), vcov = ~township),
  model_5 = feols(as.formula(paste(model_buffer_100_1000, "| ward^year + sale_month")), subset(df_main, dist_house_lake < 4000), vcov = ~township)
  
)

modelsummary(models_4, stars = T,coef_map = cm,)


options(modelsummary_format_numeric_latex = "mathmode")


modelsummary(models_4, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table5.tex")




# Type 5

model_buffer_100_200 = paste("log(price_real)  ~ log_sd_inter_front + log_sd_inter_front_200 + log_sd_inter_200_plus + waterfront + dist_100m_200m + dist_200m_plus +",
                             paste0(x_struct, collapse=" + "))
model_buffer_100_300 = paste("log(price_real)  ~ log_sd_inter_front + log_sd_inter_front_300 + log_sd_inter_300_plus + waterfront + dist_100m_300m + dist_300m_plus +",
                             paste0(x_struct, collapse=" + "))
model_buffer_100_400 = paste("log(price_real)  ~ log_sd_inter_front + log_sd_inter_front_400 + log_sd_inter_400_plus  + waterfront + dist_100m_400m + dist_400m_plus +",
                             paste0(x_struct, collapse=" + "))
model_buffer_100_500 = paste("log(price_real)  ~ log_sd_inter_front + log_sd_inter_front_500 + log_sd_inter_500_plus  + waterfront + dist_100m_500m + dist_500m_plus +",
                             paste0(x_struct, collapse=" + "))


models_4 = list(
  model_1 = feols(as.formula(paste(model_buffer_100_200, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_2 = feols(as.formula(paste(model_buffer_100_300, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_3 = feols(as.formula(paste(model_buffer_100_400, "| ward^year + sale_month")), df_main, vcov = ~township),
  model_4 = feols(as.formula(paste(model_buffer_100_500, "| ward^year + sale_month")), df_main, vcov = ~township)

)

modelsummary(models_4, stars = T,coef_map = cm,)

modelsummary(models_4, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table6.tex")

# Type 7
# Binned regression 1

start_number = 0
end_number = 500
by_number = 100
breaks1 = seq(start_number, end_number, by = by_number)  
breaks1 = c(breaks1[-length(breaks1)],Inf)
#breaks1 = c(-Inf,breaks1)

# Binned regression - repeated sales


names1 = paste0(rep(" (",length(breaks1)-1),
                breaks1[-1]-by_number, "-", breaks1[-1], ")")

names1 = c(names1[-length(names1)], paste0(" (",end_number,"+)"))

x_interp = paste("log(price_real)  ~ log(sd_interp):i(dist) + dist +", 
                 paste0(x_struct, collapse=" + "))
x_average = paste("log(price_real)  ~ log(sd_average):i(dist) + dist +", 
                  paste0(x_struct, collapse=" + "))


fes_ward = "| ward^year + sale_month"

bined_models = list(
  sd_interp =  df_main %>%
    mutate(dist = cut(dist_house_lake,
                      breaks=breaks1,
                      labels=names1)) %>%
    feols(as.formula(paste(x_interp, fes_ward)), ., vcov = ~township),
  sd_average =  df_main %>%
    mutate(dist = cut(dist_house_lake,
                      breaks=breaks1,
                      labels=names1)) %>%
    feols(as.formula(paste(x_average, fes_ward)), ., vcov = ~township)
  
)



modelsummary(bined_models, stars = T,coef_map = cm)

modelsummary(bined_models, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table7.tex")

# TYpe 8

# Repeat Sale
df_repeat = df_main %>%
  select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
         log_sd_front, log_sd_inter_front,log_sd_250,log_sd_inter_250,log_sd_500, log_sd_inter_500, 
         log_sd_1000,log_sd_inter_1000, log_sd_500_gl, log_sd_500_gl_no, region, inverse_dist,
         dist_250m,dist_500m,dist_1000m,dist_2000m, waterfront)

df_repeat = df_subset  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)




models_repeat_sales = list(
  interp_250 = feols(as.formula(paste(model_250_inter, "| ward^year + sale_month")),df_repeat, vcov = ~township),
  normal_250 = feols(as.formula(paste(model_250_ave, "| ward^year + sale_month")),df_repeat, vcov = ~township),
  interp_500 = feols(as.formula(paste(model_500_inter, "| ward^year + sale_month")), df_repeat, vcov = ~township),
  normal_500 = feols(as.formula(paste(model_500_ave, "| ward^year + sale_month")),df_repeat, vcov = ~township),
  interp_1000 = feols(as.formula(paste(model_1000_inter, "| ward^year + sale_month")),df_repeat, vcov = ~township),
  normal_1000 = feols(as.formula(paste(model_1000_ave, "| ward^year + sale_month")), df_repeat, vcov = ~township)
)

modelsummary(models_repeat_sales, 
             stars = T,
             coef_map = cm)
             #gof_map = gm)

modelsummary(models_repeat_sales, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table8.tex")


# TYpe 9
#Binned regression 
start_number = 0
end_number = 500
by_number = 100
breaks1 = seq(start_number, end_number, by = by_number)  
breaks1 = c(breaks1[-length(breaks1)],Inf)
#breaks1 = c(-Inf,breaks1)

# Binned regression - repeated sales


df_repeat = df_main%>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)

names1 = paste0(rep(" (",length(breaks1)-1),
                breaks1[-1]-by_number, "-", breaks1[-1], ")")

names1 = c(names1[-length(names1)], paste0(" (",end_number,"+)"))

x_repeat_interp = paste("log(price_real)  ~ waterfront + log(sd_interp):i(dist) + dist  + log_lakearea:log(sd_interp) + waterfront:log(sd_interp) +", 
                        paste0(x_struct, collapse=" + "))
x_repeat_average = paste("log(price_real)  ~ waterfront + log(sd_interp):i(dist) + dist  + log_lakearea:log(sd_interp) + waterfront:log(sd_interp) +", 
                         paste0(x_struct, collapse=" + "))


fes_ward = "| ward^year + sale_month"

bined_models = list(
  sd_repeat_interp =  df_repeat %>%
    mutate(dist = cut(dist_house_lake,
                      breaks=breaks1,
                      labels=names1)) %>%
    feols(as.formula(paste(x_interp, fes_ward)), ., vcov = ~township),
  sd_repeat_average =  df_repeat %>%
    mutate(dist = cut(dist_house_lake,
                      breaks=breaks1,
                      labels=names1)) %>%
    feols(as.formula(paste(x_average, fes_ward)), ., vcov = ~township)
  
  
)


modelsummary(bined_models, stars = T,coef_map = cm,)

modelsummary(bined_models, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table9.tex")







###############################################################################
# Sensitivity Analysis
###############################################################################
# sensitivity analysis with different fixed effects

model_250_inter = paste("log(price_real)  ~ log_sd_inter_250 + inverse_dist + waterfront + dist_250m +",
                        paste0(x_struct, collapse=" + "))
model_250_ave = paste("log(price_real)  ~ log_sd_250 + inverse_dist + waterfront + dist_250m +",
                      paste0(x_struct, collapse=" + "))
model_500_inter = paste("log(price_real)  ~ log_sd_inter_500 + inverse_dist + waterfront + dist_500m +",
                        paste0(x_struct, collapse=" + "))
model_500_ave = paste("log(price_real)  ~ log_sd_500 + inverse_dist + waterfront + dist_500m +",
                      paste0(x_struct, collapse=" + "))
model_1000_inter = paste("log(price_real)  ~ log_sd_inter_1000 + inverse_dist + waterfront + dist_1000m +",
                         paste0(x_struct, collapse=" + "))
model_1000_ave = paste("log(price_real)  ~ log_sd_1000 + inverse_dist + waterfront + dist_1000m +",
                       paste0(x_struct, collapse=" + "))



models_fes_snes = list(
  county_inter_250 = feols(as.formula(paste(model_250_inter, "| county^year + sale_month")), df_main, vcov = ~county),
  township_inter_250 = feols(as.formula(paste(model_250_inter, "| township^year + sale_month")),df_main, vcov = ~township),
  ward_inter_250 = feols(as.formula(paste(model_250_inter, "| ward^year + sale_month")),df_main, vcov = ~township),
  county_ave_250 = feols(as.formula(paste(model_250_ave, "| county^year + sale_month")),df_main, vcov = ~county),
  township_ave_250 = feols(as.formula(paste(model_250_ave , "| township^year + sale_month")),df_main, vcov = ~township),
  ward_ave_250 = feols(as.formula(paste(model_250_ave, "| ward^year + sale_month")),df_main, vcov = ~township),
  county_inter_500 = feols(as.formula(paste(model_500_inter, "| county^year + sale_month")),df_main, vcov = ~county),
  township_inter_500 = feols(as.formula(paste(model_500_inter, "| township^year + sale_month")),df_main, vcov = ~township),
  ward_inter_500 = feols(as.formula(paste(model_500_inter, "| ward^year + sale_month")),df_main, vcov = ~township),
  county_ave_500 = feols(as.formula(paste(model_500_ave, "| county^year + sale_month")),df_main, vcov = ~county),
  township_ave_500 = feols(as.formula(paste(model_500_ave, "| township^year + sale_month")),df_main, vcov = ~township),
  ward_ave_500 = feols(as.formula(paste(model_500_ave, "| ward^year + sale_month")),df_main, vcov = ~township),
  county_inter_1000 = feols(as.formula(paste(model_1000_inter, "| county^year + sale_month")),df_main, vcov = ~county),
  township_inter_1000 = feols(as.formula(paste(model_1000_inter, "| township^year + sale_month")),df_main, vcov = ~township),
  ward_inter_1000 = feols(as.formula(paste(model_1000_inter, "| ward^year + sale_month")),df_main, vcov = ~township),
  county_ave_1000 = feols(as.formula(paste(model_1000_ave, "| county^year + sale_month")),df_main, vcov = ~county),
  township_ave_1000 = feols(as.formula(paste(model_1000_ave, "| township^year + sale_month")),df_main, vcov = ~township),
  ward_ave_1000 = feols(as.formula(paste(model_1000_ave, "| ward^year + sale_month")),df_main, vcov = ~township)
)

df1 <- data.frame(coef=coef(models_fes_snes$county_inter_250)[["log_sd_inter_250"]], 
                  se=sqrt(diag(vcov(models_fes_snes$county_inter_250)))[["log_sd_inter_250"]], r2=r2(models_fes_snes$county_inter_250,"r2"))%>%
  mutate(county_year = "TRUE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "TRUE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df2 <- data.frame(coef=coef(models_fes_snes$township_inter_250)[["log_sd_inter_250"]], 
                  se=sqrt(diag(vcov(models_fes_snes$township_inter_250)))[["log_sd_inter_250"]], r2=r2(models_fes_snes$township_inter_250,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "TRUE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "TRUE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df3 <- data.frame(coef=coef(models_fes_snes$ward_inter_250)[["log_sd_inter_250"]], 
                  se=sqrt(diag(vcov(models_fes_snes$ward_inter_250)))[["log_sd_inter_250"]], r2=r2(models_fes_snes$ward_inter_250,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "TRUE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "TRUE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")


df4 <- data.frame(coef=coef(models_fes_snes$county_ave_250)[["log_sd_250"]], 
                  se=sqrt(diag(vcov(models_fes_snes$county_ave_250)))[["log_sd_250"]], r2=r2(models_fes_snes$county_ave_250,"r2"))%>%
  mutate(county_year = "TRUE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(Dist_250 = "TRUE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df5 <- data.frame(coef=coef(models_fes_snes$township_ave_250)[["log_sd_250"]], 
                  se=sqrt(diag(vcov(models_fes_snes$township_ave_250)))[["log_sd_250"]], r2=r2(models_fes_snes$township_ave_250,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "TRUE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(Dist_250 = "TRUE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df6 <- data.frame(coef=coef(models_fes_snes$ward_ave_250)[["log_sd_250"]], 
                  se=sqrt(diag(vcov(models_fes_snes$ward_ave_250)))[["log_sd_250"]], r2=r2(models_fes_snes$ward_ave_250,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "TRUE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(Dist_250 = "TRUE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df7 <- data.frame(coef=coef(models_fes_snes$county_inter_500)[["log_sd_inter_500"]], 
                  se=sqrt(diag(vcov(models_fes_snes$county_inter_500)))[["log_sd_inter_500"]], r2=r2(models_fes_snes$county_inter_500,"r2"))%>%
  mutate(county_year = "TRUE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "TRUE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")


df8 <- data.frame(coef=coef(models_fes_snes$township_inter_500)[["log_sd_inter_500"]], 
                  se=sqrt(diag(vcov(models_fes_snes$township_inter_500)))[["log_sd_inter_500"]], r2=r2(models_fes_snes$township_inter_500,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "TRUE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "TRUE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df9 <- data.frame(coef=coef(models_fes_snes$ward_inter_500)[["log_sd_inter_500"]], 
                  se=sqrt(diag(vcov(models_fes_snes$ward_inter_500)))[["log_sd_inter_500"]], r2=r2(models_fes_snes$ward_inter_500,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "TRUE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "TRUE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df10 <- data.frame(coef=coef(models_fes_snes$county_ave_500)[["log_sd_500"]], 
                  se=sqrt(diag(vcov(models_fes_snes$county_ave_500)))[["log_sd_500"]], r2=r2(models_fes_snes$county_ave_500,"r2"))%>%
  mutate(county_year = "TRUE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "TRUE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")


df11 <- data.frame(coef=coef(models_fes_snes$township_ave_500)[["log_sd_500"]], 
                  se=sqrt(diag(vcov(models_fes_snes$township_ave_500)))[["log_sd_500"]], r2=r2(models_fes_snes$township_ave_500,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "TRUE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "TRUE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df12 <- data.frame(coef=coef(models_fes_snes$ward_ave_500)[["log_sd_500"]], 
                  se=sqrt(diag(vcov(models_fes_snes$ward_ave_500)))[["log_sd_500"]], r2=r2(models_fes_snes$ward_ave_500,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "TRUE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "TRUE")%>%
  mutate(Dist_1000 = "FALSE")%>%
  mutate(Dist_2000 = "FALSE")

df13 <- data.frame(coef=coef(models_fes_snes$county_inter_1000)[["log_sd_inter_1000"]], 
                  se=sqrt(diag(vcov(models_fes_snes$county_inter_1000)))[["log_sd_inter_1000"]], r2=r2(models_fes_snes$county_inter_1000,"r2"))%>%
  mutate(county_year = "TRUE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "TRUE")%>%
  mutate(Dist_2000 = "FALSE")

df14 <- data.frame(coef=coef(models_fes_snes$township_inter_1000)[["log_sd_inter_1000"]], 
                  se=sqrt(diag(vcov(models_fes_snes$township_inter_1000)))[["log_sd_inter_1000"]], r2=r2(models_fes_snes$township_inter_1000,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "TRUE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "TRUE")%>%
  mutate(Dist_2000 = "FALSE")

df15 <- data.frame(coef=coef(models_fes_snes$ward_inter_1000)[["log_sd_inter_1000"]], 
                  se=sqrt(diag(vcov(models_fes_snes$ward_inter_1000)))[["log_sd_inter_1000"]], r2=r2(models_fes_snes$ward_inter_1000,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "TRUE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "FALSE")%>%
  mutate(sd_interp = "TRUE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "TRUE")%>%
  mutate(Dist_2000 = "FALSE")


df16 <- data.frame(coef=coef(models_fes_snes$county_ave_1000)[["log_sd_1000"]], 
                  se=sqrt(diag(vcov(models_fes_snes$county_ave_1000)))[["log_sd_1000"]], r2=r2(models_fes_snes$county_ave_1000,"r2"))%>%
  mutate(county_year = "TRUE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "TRUE")%>%
  mutate(Dist_2000 = "FALSE")

df17 <- data.frame(coef=coef(models_fes_snes$township_ave_1000)[["log_sd_1000"]], 
                  se=sqrt(diag(vcov(models_fes_snes$township_ave_1000)))[["log_sd_1000"]], r2=r2(models_fes_snes$township_ave_1000,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "TRUE")%>%
  mutate(ward_year = "FALSE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "TRUE")%>%
  mutate(Dist_2000 = "FALSE")

df18 <- data.frame(coef=coef(models_fes_snes$ward_ave_1000)[["log_sd_1000"]], 
                  se=sqrt(diag(vcov(models_fes_snes$ward_ave_1000)))[["log_sd_1000"]], r2=r2(models_fes_snes$ward_ave_1000,"r2"))%>%
  mutate(county_year = "FALSE")%>%
  mutate(township_year = "FALSE")%>%
  mutate(ward_year = "TRUE")%>%
  mutate(sale_month = "TRUE")%>%
  mutate(Physical_charac = "TRUE")%>%
  mutate(sd_average = "TRUE")%>%
  mutate(sd_interp = "FALSE")%>%
  mutate(Dist_250 = "FALSE")%>%
  mutate(Dist_500 = "FALSE")%>%
  mutate(Dist_1000 = "TRUE")%>%
  mutate(Dist_2000 = "FALSE")



df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18)

df <- df%>%
  rename("FE:County Year"="county_year",
         "FE:Township Year" = "township_year",
         "FE:Ward Year" = "ward_year",
         "FE:Sale Month" = "sale_month",
         "Physical Characteristics" = "Physical_charac",
         "Average SD"="sd_average",
         "Interpolated SD"="sd_interp")

source("./code/spec_chart_function.R")

# Looks better when there is an outer margins
par(oma=c(1,0,1,1))

# Most basic plot
schart(df, n = 6,lwd.symbol=2, lwd.est=2, length=.02)

###############################################################################

# Sensitivity analysis with different interpolation lengths

df_inter_sensiti = readRDS("data/analysis_sample_wo_interpolationlimit.rds")


df_inter_sensiti <- df_inter_sensiti%>%
  mutate(sd_extrap_years = str_remove(interpolation, "year"),
         sd_extrap_years = replace_na(as.numeric(sd_extrap_years), 0),
         interp_length_2 = ifelse(between(sd_extrap_years, -2,2),1,0),
         interp_length_10 = ifelse(between(sd_extrap_years, -10,10),1,0),
         interp_length_15 = ifelse(between(sd_extrap_years, -15,15),1,0),
         interp_length_20 = ifelse(between(sd_extrap_years, -20,20),1,0),
         interp_length_25 = ifelse(between(sd_extrap_years, -25,25),1,0),
         )

df_inter_sensiti_1 <- df_inter_sensiti%>%
  subset(interp_length_10 == 1)%>%
  mutate( log_sd_inter_500 = ifelse(dist_500m == 1, log(sd_interp), 0)
          )

df_inter_sensiti_2 <- df_inter_sensiti%>%
  subset(interp_length_15 == 1)%>%
  mutate( log_sd_inter_500 = ifelse(dist_500m == 1, log(sd_interp), 0)
  )

df_inter_sensiti_3 <- df_inter_sensiti%>%
  subset(interp_length_2 == 1)%>%
  mutate( log_sd_inter_500 = ifelse(dist_500m == 1, log(sd_interp), 0)
  )



model_sens_5_inter = paste("log(price_real)  ~ log_sd_inter_500  + waterfront + dist_500m + inverse_dist +",
                        paste0(x_struct, collapse=" + "))
model_sens_10_inter = paste("log(price_real)  ~ log_sd_inter_500  + waterfront + dist_500m + inverse_dist +",
                           paste0(x_struct, collapse=" + "))
model_sens_15_inter = paste("log(price_real)  ~ log_sd_inter_500  + waterfront + dist_500m + inverse_dist +",
                           paste0(x_struct, collapse=" + "))
model_sens_2_inter = paste("log(price_real)  ~ log_sd_inter_500  + waterfront + dist_500m + inverse_dist +",
                            paste0(x_struct, collapse=" + "))

models_inter_sensi = list(
  ward_ave_2000_5 = feols(as.formula(paste(model_sens_5_inter, "| ward^year + sale_month")), df_inter_sensiti, vcov = ~township),
  ward_inter_2000_10 = feols(as.formula(paste(model_sens_10_inter, "| ward^year + sale_month")),df_inter_sensiti_1, vcov = ~township),
  ward_inter_2000_15 = feols(as.formula(paste(model_sens_10_inter, "| ward^year + sale_month")),df_inter_sensiti_2, vcov = ~township),
  ward_inter_2000_2 = feols(as.formula(paste(model_sens_2_inter, "| ward^year + sale_month")),df_inter_sensiti_3, vcov = ~township)
  
  

)

modelsummary(models_inter_sensi, stars = T,coef_map = cm,)

options(modelsummary_format_numeric_latex = "mathmode")


modelsummary(models_inter_sensi, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table7.tex")

###############################################################################

# Great Lakes vs Non Great Lakes 


x_250_gl = paste("log(price_real)  ~ log_sd_250_gl + log_sd_250_gl_no  + waterfront + inverse_dist + dist_250m+",
                 paste0(x_struct, collapse=" + "))
x_500_gl = paste("log(price_real)  ~ log_sd_500_gl + log_sd_500_gl_no  + waterfront + inverse_dist + dist_500m +",
                 paste0(x_struct, collapse=" + "))
x_1000_gl = paste("log(price_real)  ~ log_sd_1000_gl + log_sd_1000_gl_no + waterfront + inverse_dist + dist_1000m+",
                 paste0(x_struct, collapse=" + "))
x_2000_gl = paste("log(price_real)  ~ log_sd_2000_gl + log_sd_2000_gl_no + waterfront + inverse_dist + dist_2000m +",
                 paste0(x_struct, collapse=" + "))
x_250_inter_gl = paste("log(price_real)  ~ log_sd_inter_250_gl + log_sd_inter_250_gl_no + waterfront + inverse_dist + dist_250m+",
                 paste0(x_struct, collapse=" + "))
x_500_inter_gl = paste("log(price_real)  ~ log_sd_inter_500_gl + log_sd_inter_500_gl_no + waterfront + inverse_dist + dist_500m +",
                       paste0(x_struct, collapse=" + "))
x_1000_inter_gl = paste("log(price_real)  ~ log_sd_inter_1000_gl + log_sd_inter_1000_gl_no + waterfront + inverse_dist + dist_1000m +",
                       paste0(x_struct, collapse=" + "))
x_2000_inter_gl = paste("log(price_real)  ~ log_sd_inter_2000_gl + log_sd_inter_2000_gl_no + waterfront + inverse_dist + dist_2000m+",
                       paste0(x_struct, collapse=" + "))

models_gl = list(
  gl_inter_250 = feols(as.formula(paste(x_250_inter_gl, "| ward^year + sale_month")), df_main, vcov = ~township),
  gl_250 = feols(as.formula(paste(x_250_gl, "| ward^year + sale_month")), df_main, vcov = ~township),
  gl_inter_500 = feols(as.formula(paste(x_500_inter_gl, "| ward^year + sale_month")), df_main, vcov = ~township),
  gl_500 = feols(as.formula(paste(x_500_gl, "| ward^year + sale_month")), df_main, vcov = ~township),
  gl_inter_1000 = feols(as.formula(paste(x_1000_inter_gl, "| ward^year + sale_month")), df_main, vcov = ~township),
  gl_1000 = feols(as.formula(paste(x_1000_gl, "| ward^year + sale_month")), df_main, vcov = ~township),
  gl_inter_2000 = feols(as.formula(paste(x_2000_inter_gl, "| ward^year + sale_month")), df_main, vcov = ~township),
  gl_2000 = feols(as.formula(paste(x_2000_gl, "| ward^year + sale_month")), df_main, vcov = ~township)
)


modelsummary(models_gl, stars = T,coef_map = cm,)

modelsummary(models_gl, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table10.tex")

#########################################################
#Binned regression 
start_number = 0
end_number = 5000
by_number = 250
breaks1 = seq(start_number, end_number, by = by_number)  
breaks1 = c(breaks1[-length(breaks1)],Inf)
#breaks1 = c(-Inf,breaks1)

# Binned regression - repeated sales


df_repeat = df_main%>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)

names1 = paste0(rep(" (",length(breaks1)-1),
                breaks1[-1]-by_number, "-", breaks1[-1], ")")

names1 = c(names1[-length(names1)], paste0(" (",end_number,"+)"))

x_interp = paste("log(price_real)  ~ waterfront + log(sd_interp):i(dist) + dist  + log_lakearea:log(sd_interp) + waterfront:log(sd_interp) +", 
                    paste0(x_struct, collapse=" + "))
x_average = paste("log(price_real)  ~ waterfront + log(sd_average):i(dist) + dist  + log_lakearea:log(sd_average) + waterfront:log(sd_average) +", 
                  paste0(x_struct, collapse=" + "))
x_repeat_interp = paste("log(price_real)  ~ waterfront + log(sd_interp):i(dist) + dist  + log_lakearea:log(sd_interp) + waterfront:log(sd_interp) +", 
                 paste0(x_struct, collapse=" + "))
x_repeat_average = paste("log(price_real)  ~ waterfront + log(sd_interp):i(dist) + dist  + log_lakearea:log(sd_interp) + waterfront:log(sd_interp) +", 
                        paste0(x_struct, collapse=" + "))


fes_ward = "| ward^year + sale_month"

bined_models = list(
  sd_interp =  df_main %>%
  mutate(dist = cut(dist_house_lake,
                    breaks=breaks1,
                    labels=names1)) %>%
  feols(as.formula(paste(x_interp, fes_ward)), ., vcov = ~township),
  sd_average =  df_main %>%
    mutate(dist = cut(dist_house_lake,
                      breaks=breaks1,
                      labels=names1)) %>%
    feols(as.formula(paste(x_average, fes_ward)), ., vcov = ~township),
  sd_repeat_interp =  df_repeat %>%
    mutate(dist = cut(dist_house_lake,
                      breaks=breaks1,
                      labels=names1)) %>%
    feols(as.formula(paste(x_interp, fes_ward)), ., vcov = ~township),
  sd_repeat_average =  df_repeat %>%
    mutate(dist = cut(dist_house_lake,
                      breaks=breaks1,
                      labels=names1)) %>%
    feols(as.formula(paste(x_average, fes_ward)), ., vcov = ~township)
  
  
)


modelsummary(bined_models, stars = T,coef_map = cm,)

modelsummary(bined_models, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table5.tex")


# Repeat Sale
df_repeat = df_main %>%
  select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
         log_sd_front, log_sd_inter_front,log_sd_250,log_sd_inter_250,log_sd_500, log_sd_inter_500, 
         log_sd_1000,log_sd_inter_1000, log_sd_500_gl, log_sd_500_gl_no, region, inverse_dist,
         dist_250m,dist_500m,dist_1000m,dist_2000m)

df_repeat = df_subset  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


models_repeat_sales = list(
  interp_250 = feols(as.formula(paste(x_wf_inter_250, "| ward^year + sale_month")),subset(df_repeat, dist_250m ==1), vcov = ~township),
  average_250 = feols(as.formula(paste(x_wf_ave_250, "| ward^year + sale_month")),subset(df_repeat, dist_250m ==1), vcov = ~township),
  interp_500 = feols(as.formula(paste(x_wf_inter_500, "| ward^year + sale_month")),subset(df_repeat, dist_500m ==1), vcov = ~township),
  normal_500 = feols(as.formula(paste(x_wf_ave_500, "| ward^year + sale_month")),subset(df_repeat, dist_500m ==1), vcov = ~township),
  interp_1000 = feols(as.formula(paste(x_wf_inter_1000, "| ward^year + sale_month")),subset(df_repeat, dist_1000m ==1), vcov = ~township),
  normal_1000 = feols(as.formula(paste(x_wf_ave_1000, "| ward^year + sale_month")),subset(df_repeat, dist_1000m ==1), vcov = ~township),
  interp_2000 = feols(as.formula(paste(x_wf_inter_2000, "| ward^year + sale_month")),subset(df_repeat, dist_2000m ==1), vcov = ~township),
  normal_2000 = feols(as.formula(paste(x_wf_ave_2000, "| ward^year + sale_month")),subset(df_repeat, dist_2000m ==1), vcov = ~township)
)

modelsummary(models_repeat_sales, 
             stars = T,
             coef_map = cm, 
             gof_map = gm)

modelsummary(models_repeat_sales, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table6.tex")


################################################################################
# Sensitivity analysis of property characteristics in the model

xs <- c("lotsize","log_area_tot","log_dist_toronto","log_dist_cma",
        "prop_type","pool","air_cond","heat_elect","heat_forcedair","fireplcs",
        "baths","bedrooms","storeys","quality","lbasement_area","sd_interp")
f <- paste("log(price_real) ~",paste(xs, collapse=" + "))
county = feols(as.formula(paste(f, "| county^year + sale_month")), df_main, vcov = ~county)
etable(county)


vars0 <- list(c("sd_interp"),xs)
index <- combn(1:length(xs), length(xs)-1)
index <- index[,index[nrow(index),] %in% match("sd_interp",xs)] # keep only models with nox
vars1 <- lapply(1:ncol(index), function(i) xs[index[,i]])
vars <- c(vars0, vars1)
rm(vars1,index)
flist <- lapply(vars, function(x) paste("log(price_real) ~",paste(x, collapse=" + ")))


# 2. Run models and export key info
regs1 <- lapply(flist, function(f) {
  print(f)
  # Run model
  reg <- feols(as.formula(paste(f, "| county^year + sale_month")), df_main, vcov = ~county)
  # Export
  data.frame(coef=coef(reg)[["sd_interp"]], se=sqrt(diag(vcov(reg)))[["sd_interp"]], r2=r2(reg,"r2"))
})
regs1 <- data.frame(do.call("rbind",regs1))

# 3. Prepare data for plotting
regs2 <- lapply(vars, function(x) c(xs) %in% x)
regs2 <- data.frame(do.call("rbind",regs2))
names(regs2) <- c(xs)
data <- cbind(regs1,regs2)
rm(regs1, regs2)
data$nox <- NULL # remove indicator for coefficient of interest present in all models
# remove R2 from table but keep it as a separate object
r2 <- data$r2
data$r2  <- NULL

# 4. Labels
# Enter labels in order they appear in table
labels <- list("Physical controls:" = c("lotsize","log_area_tot","log_dist_toronto","log_dist_cma",
                                        "prop_type","baths","bedrooms","storeys","quality","lbasement_area"),
               "Dummies:" = c("air_cond","pool","heat_elect","heat_forcedair","fireplcs"))


source("./code/spec_chart_function.R")

# Looks better when there is an outer margins
par(oma=c(1,0,1,1))


# Add labels without groups
schart(data, unlist(labels),lwd.symbol=2, lwd.est=2, length=.02)



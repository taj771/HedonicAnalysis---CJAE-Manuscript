# Test for differences in SD average vs SD average + SD interpolated

# clear memory
rm(list = ls())

# load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  fixest,
  modelsummary,
  flextable
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


cm <- c('log_sd'    = 'Log(SD)',
        'log_sd_inter'    = 'Log(SD)',
        'log_sd_front' = 'Log(SD)X100m',
        'log_sd_200'    = 'Log(SD)X200m',
        'log_sd_250'    = 'Log(SD)X250m',
        'log_sd_300'    = 'Log(SD)X300m',
        'log_sd_400'    = 'Log(SD)X400m',
        'log_sd_500'    = 'Log(SD)X500m',
        'log_sd_inter_front' = 'Log(SD)X100m',
        'log_sd_inter_200'    = 'Log(SD)X200m',
        'log_sd_inter_250'    = 'Log(SD)X250m',
        'log_sd_inter_300'    = 'Log(SD)X300m',
        'log_sd_inter_400'    = 'Log(SD)X400m',
        'log_sd_inter_500'    = 'Log(SD)X500m',
        'log_sd_1000'    = 'Log(SD)X1000m',
        'log_sd_inter_1000'    = 'Log(SD)X1000m',
        'log_sd_2000'    = 'Log(SD)X2000m',
        'log_sd_inter_2000'    = 'Log(SD)X2000m',
        'log_sd_front_200' = 'Log(SD)X100m-200m',
        'log_sd_200_300' = 'Log(SD)X200m-300m',
        'log_sd_300_400' = 'Log(SD)X300m-400m',
        'log_sd_400_500' = 'Log(SD)X400m-500m',
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
        'log_sd_inter_250_500' = 'Log(SD)X250m-500m',
        'sd_inter_500' = 'SDX500m',
        'sd_inter_250' = 'SDX0m-250m',
        'sd_inter_250_500' = 'SDX250m-500m',
        'log_sd_inter_100' = 'SDX0m-100m',
        'log_sd_inter_100_200' = 'SDX100m-200m',
        'log_sd_inter_200_300' = 'SDX200m-300m',
        'log_sd_inter_300_400' = 'SDX300m-400m',
        'log_sd_inter_400_500' = 'SDX400m-500m',
        'log_sd_500_ave_dum' = 'Log(SD)X500mX SD WO Interpolation',
        'log_sd_250_ave_dum' = 'Log(SD)X250mX SD WO Interpolation',
        'log_sd_250_500_ave_dum' = 'Log(SD)X250m-500mX SD WO Interpolation'
)


# Repeat sales
df_repeat = df_main %>%
  select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
         log_sd_250,log_sd_250_500, log_sd_inter_250,log_sd_inter_250_500, dist_250m, dist_250m_500m )

df_repeat = df_subset  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


fes_ward = "| ward^year + sale_month"

############################################################################


x_distbuf_1 = paste("log(price_real)  ~ log_sd_inter_500 + 
                  dist_500m +",
                    paste0(x_struct, collapse=" + "))
x_distbuf_1_ave = paste("log(price_real)  ~ log_sd_500  + 
                  dist_500m +",
                        paste0(x_struct, collapse=" + "))

x_distbuf_2 = paste("log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500  + 
                  dist_250m + dist_250m_500m +",
                    paste0(x_struct, collapse=" + "))
x_distbuf_2_ave = paste("log(price_real)  ~ log_sd_250 + log_sd_250_500  + 
                  dist_250m + dist_250m_500m +",
                        paste0(x_struct, collapse=" + "))


models_fes = list(
  model_1 = feols(as.formula(paste(x_distbuf_1, fes_ward)), df_main, vcov = ~ward),
  model_2 = feols(as.formula(paste(x_distbuf_1_ave, fes_ward)), df_main, vcov = ~ward),
  model_3 = feols(as.formula(paste(x_distbuf_2, fes_ward)), df_main, vcov = ~ward),
  model_4 = feols(as.formula(paste(x_distbuf_2_ave, fes_ward)), df_main, vcov = ~ward),
  model_5 = df_repeat  %>%
    feols(log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500 + age | year + sale_month +roll_num, ., vcov = ~roll_num),
  model_6 = df_repeat  %>%
    feols(log(price_real)  ~ log_sd_250 + log_sd_250_500 + age | year + sale_month +roll_num, ., vcov = ~roll_num)
)

modelsummary(models_fes, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T)



b1 <- models_fes$model_1$coefficients[1]
se1 <- models_fes$model_1$se[1]

b2 <- models_fes$model_2$coefficients[1]
se2 <- models_fes$model_2$se[1]



b = b1 - b2
s1 = se1^2
s2 = se2^2
sc = s1 + s2
v = b / sqrt(sc)

data.frame(diff=b, zdiff=v, `p-value`=format(2*pnorm(-abs(v)), scientific=FALSE))


b3_1 <- models_fes$model_3$coefficients[1]
b3_2 <- models_fes$model_3$coefficients[2]
se3_1 <- models_fes$model_3$se[1]
se3_2 <- models_fes$model_3$se[2]


b4_1 <- models_fes$model_4$coefficients[1]
b4_2 <- models_fes$model_4$coefficients[2]
se4_1 <- models_fes$model_4$se[1]
se4_2 <- models_fes$model_4$se[2]


b = b3_1 - b4_1
s3 = se3_1^2
s4 = se4_1^2
sc = s1 + s2
v = b / sqrt(sc)

data.frame(diff=b, zdiff=v, `p-value`=format(2*pnorm(-abs(v)), scientific=FALSE))

b = b3_2 - b4_2
s3 = se3_2^2
s4 = se4_2^2
sc = s1 + s2
v = b / sqrt(sc)

data.frame(diff=b, zdiff=v, `p-value`=format(2*pnorm(-abs(v)), scientific=FALSE))


b5_1 <- models_fes$model_5$coefficients[1]
se5_1 <- models_fes$model_5$se[1]

b5_2 <- models_fes$model_5$coefficients[2]
se5_2 <- models_fes$model_5$se[2]

b6_1 <- models_fes$model_6$coefficients[1]
se6_1 <- models_fes$model_6$se[1]

b6_2 <- models_fes$model_6$coefficients[1]
se6_2 <- models_fes$model_6$se[1]


b = b5_1 - b6_1
s5 = se5_1^2
s6 = se6_1^2
sc = s5 + s6
v = b / sqrt(sc)

data.frame(diff=b, zdiff=v, `p-value`=format(2*pnorm(-abs(v)), scientific=FALSE))


b = b5_2 - b6_2
s5 = se5_2^2
s6 = se6_2^2
sc = s5 + s6
v = b / sqrt(sc)

data.frame(diff=b, zdiff=v, `p-value`=format(2*pnorm(-abs(v)), scientific=FALSE))

##########################################################################

df_main <- df_main%>%
  mutate(sd_ave_dum = ifelse(interpolation == "none", 1, 0))%>%
  mutate(sd_ave_dum  = ifelse(is.na(sd_ave_dum ), 0, sd_ave_dum ))%>%
  mutate(log_sd_500_ave_dum = log_sd_inter_500*sd_ave_dum)%>%
  mutate(log_sd_250_ave_dum = log_sd_inter_250*sd_ave_dum)%>%
  mutate(log_sd_250_500_ave_dum = log_sd_inter_250_500*sd_ave_dum)


df_repeat = df_subset%>%
  mutate(sd_ave_dum = ifelse(interpolation == "none", 1, 0))%>%
  mutate(sd_ave_dum  = ifelse(is.na(sd_ave_dum ), 0, sd_ave_dum ))%>%
  mutate(log_sd_500_ave_dum = log_sd_inter_500*sd_ave_dum)%>%
  mutate(log_sd_250_ave_dum = log_sd_inter_250*sd_ave_dum)%>%
  mutate(log_sd_250_500_ave_dum = log_sd_inter_250_500*sd_ave_dum)%>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


x_distbuf_1 = paste("log(price_real)  ~ log_sd_inter_500 +
                  dist_500m +log_sd_500_ave_dum+",
                    paste0(x_struct, collapse=" + "))
x_distbuf_1_ave = paste("log(price_real)  ~ log_sd_500  + 
                  dist_500m +",
                        paste0(x_struct, collapse=" + "))
x_distbuf_2 = paste("log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500  + 
                  dist_250m + dist_250m_500m + log_sd_250_ave_dum + log_sd_250_500_ave_dum  +",
                    paste0(x_struct, collapse=" + "))
x_distbuf_2_ave = paste("log(price_real)  ~ log_sd_250 + log_sd_250_500  + 
                  dist_250m + dist_250m_500m +",
                        paste0(x_struct, collapse=" + "))


models_fes = list(
  model_8 = feols(as.formula(paste(x_distbuf_1_ave, fes_ward)), df_main, vcov = ~ward),
  model_9 = feols(as.formula(paste(x_distbuf_1, fes_ward)), df_main, vcov = ~ward),
  model_10 = feols(as.formula(paste(x_distbuf_2_ave, fes_ward)), df_main, vcov = ~ward),
  model_11 = feols(as.formula(paste(x_distbuf_2, fes_ward)), df_main, vcov = ~ward),
  model_12 = df_repeat  %>%
    feols(log(price_real)  ~ log_sd_250 + log_sd_250_500 + age | year + sale_month +roll_num, ., vcov = ~roll_num),
  model_13 = df_repeat  %>%
    feols(log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500 +log_sd_250_ave_dum + log_sd_250_500_ave_dum+ age | year + sale_month +roll_num, ., vcov = ~roll_num)
)

modelsummary(models_fes, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T)

modelsummary(models_fes, 
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/table3.docx")

##########################################################################


ind <- sample(c(TRUE, FALSE), 427011, replace=TRUE, prob=c(0.80, 0.20))

df_sd_1 <- df_main[ind,]
df_sd_2 <- df_main[!ind,]

model_1 = feols(as.formula(paste(x_distbuf_1, fes_ward)), df_sd_1, vcov = ~ward, combine.quick = FALSE)

pred_sd_1 = predict(model_1, newdata = df_sd_2)%>%
  as.data.frame()%>%
  rename(pred_lg_price = ".")

MSE <- df_sd_2%>%
  select(price_real)%>%
  mutate(log_price = log(price_real))

MSE_error <- cbind(pred_sd_1,MSE)%>%
  mutate(e = (log_price-pred_lg_price)^2)

MSE_1 <- mean(MSE_error$e, na.rm = T)
MSE_1 

#model 2
model_2 = feols(as.formula(paste(x_distbuf_1_ave, fes_ward)), df_sd_1, vcov = ~ward,combine.quick = FALSE)

pred_sd_2 = predict(model_2, newdata = df_sd_2)%>%
  as.data.frame()%>%
  rename(pred_lg_price = ".")

MSE <- df_sd_2%>%
  select(price_real)%>%
  mutate(log_price = log(price_real))

MSE_error <- cbind(pred_sd_2,MSE)%>%
  mutate(e = (log_price-pred_lg_price)^2)

MSE_2 <- mean(MSE_error$e, na.rm = T)
MSE_2 

# model3

model_3 = feols(as.formula(paste(x_distbuf_2, fes_ward)), df_sd_1, vcov = ~ward, combine.quick = FALSE)

pred_sd_3 = predict(model_3, newdata = df_sd_2)%>%
  as.data.frame()%>%
  rename(pred_lg_price = ".")

MSE <- df_sd_2%>%
  select(price_real)%>%
  mutate(log_price = log(price_real))

MSE_error <- cbind(pred_sd_3,MSE)%>%
  mutate(e = (pred_lg_price-log_price)^2)

MSE_3 <- mean(MSE_error$e, na.rm = T)
MSE_3 


#model4
model_4 = feols(as.formula(paste(x_distbuf_2_ave, fes_ward)), df_sd_1, vcov = ~ward,combine.quick = FALSE)

pred_sd_4 = predict(model_4, newdata = df_sd_2)%>%
  as.data.frame()%>%
  rename(pred_lg_price = ".")

MSE <- df_sd_2%>%
  select(price_real)%>%
  mutate(log_price = log(price_real))

MSE_error <- cbind(pred_sd_4,MSE)%>%
  mutate(e = (pred_lg_price-log_price)^2)

MSE_4 <- mean(MSE_error$e, na.rm = T)
MSE_4 


# repeated sales
#model 5

df_repeat = df_main  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)

ind <- sample(c(TRUE, FALSE), 196617, replace=TRUE, prob=c(0.80, 0.20))

df_sd_1 <- df_repeat[ind,]
df_sd_2 <- df_repeat[!ind,]

model_5 = df_sd_1  %>%
  feols(log(price_real)  ~ log_sd_inter_250 + log_sd_inter_250_500 + age | year + sale_month +roll_num, ., vcov = ~roll_num,combine.quick = FALSE)


pred_sd_5 = predict(model_5, newdata = df_sd_2)%>%
  as.data.frame()%>%
  rename(pred_lg_price = ".")

MSE <- df_sd_2%>%
  select(price_real)%>%
  mutate(log_price = log(price_real))

MSE_error <- cbind(pred_sd_5,MSE)%>%
  mutate(e = (pred_lg_price-log_price)^2)

MSE_5 <- mean(MSE_error$e, na.rm = T)
MSE_5 

#model 6
model_6 = df_sd_1%>%
  feols(log(price_real)  ~ log_sd_250 + log_sd_250_500 + age | year + sale_month +roll_num, ., vcov = ~roll_num,combine.quick = FALSE)

pred_sd_6 = predict(model_6, newdata = df_sd_2)%>%
  as.data.frame()%>%
  rename(pred_lg_price = ".")

MSE <- df_sd_2%>%
  select(price_real)%>%
  mutate(log_price = log(price_real))

MSE_error <- cbind(pred_sd_6,MSE)%>%
  mutate(e = (pred_lg_price-log_price)^2)

MSE_6 <- mean(MSE_error$e, na.rm = T)
MSE_6 




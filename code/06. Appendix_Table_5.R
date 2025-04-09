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
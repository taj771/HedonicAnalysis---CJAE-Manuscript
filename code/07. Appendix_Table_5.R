library(MASS)
library(car)
library(fixest)
library(dplyr)
library(modelsummary)

# Transform WQ variable for Box-Cox (not yet applying transformation here)
df_main <- df_main %>%
  mutate(lin_sd_500 = ifelse(dist_500m == 1, sd_interp, 0))

# -------------------------------
# Model 1: Linear model for Box-Cox estimation
# -------------------------------
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

# -------------------------------
# Model 5: Box-Cox transformed price
# -------------------------------
x_500_inter <- paste("bc.price  ~ lin_sd_500  + dist_500m +", paste0(x_struct, collapse = " + "))
model_5 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_5, stars = T)


# Jacobian adjustment for Box-Cox
jacobian_bc <- if (lambda != 0) {
  (lambda - 1) * sum(log(df_main$price_real))
} else {
  sum(log(df_main$price_real))
}
ll_bc <- as.numeric(logLik(model_5)) + jacobian_bc

# -------------------------------
# Model 6: Linear price
# -------------------------------
x_500_inter <- paste("price_real  ~ lin_sd_500  + dist_500m +", paste0(x_struct, collapse = " + "))
model_6 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_6, stars = T)


ll_lin <- as.numeric(logLik(model_6))

# -------------------------------
# Model 7: Log of price
# -------------------------------
x_500_inter <- paste("log(price_real)  ~ lin_sd_500  + dist_500m +", paste0(x_struct, collapse = " + "))
model_7 <- feols(as.formula(paste(x_500_inter, fes_ward)), df_main, vcov = ~ward)

modelsummary(model_7, stars = T)


# Jacobian for log transformation
jacobian_log <- sum(log(df_main$price_real))
ll_log <- as.numeric(logLik(model_7)) + jacobian_log

# -------------------------------
# Print adjusted log-likelihoods
# -------------------------------
cat("Adjusted log-likelihoods:\n")
cat("Box-Cox (model_5):", ll_bc, "\n")
cat("Linear (model_6):", ll_lin, "\n")
cat("Log (model_7):", ll_log, "\n")

# -------------------------------
# Manually compute AICs with adjusted LL
# AIC = -2 * LL + 2 * k
# -------------------------------
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



# Cleans environment
rm(list=ls(all=TRUE))

library(tidyverse)
library(fixest)
##Import data
##Import WQ data

##Import housing data
df <-read.csv("data/df_house_wq.csv", header = T)

df = as_tibble(df) %>%
  mutate(lbasement_area = ifelse(bsmtarea == 0, 0, log(bsmtarea)))


df = df %>%
  mutate(sd_distance_bin = sd_distance_m - sd_distance_m  %% 500,
         sd_distance_bin = ifelse(sd_distance_bin == 0, 500, sd_distance_bin),
         sd_distance_bin = ifelse(sd_distance_bin > 3000, 3000, sd_distance_bin),
         tp_distance_bin = tp_distance_m - tp_distance_m  %% 500,
         tp_distance_bin = ifelse(tp_distance_bin == 0, 500, tp_distance_bin),
         tp_distance_bin = ifelse(tp_distance_bin > 3000, 3000, tp_distance_bin))

df %>%
  filter(distance_bin < 3000) %>%
ggplot(aes(x = distance_bin)) +
  geom_bar()


df = df %>%
  mutate(sd_distance_1km = ifelse(sd_distance_m <= 1000, 1, 0),
         sd_band = sd_distance_1km * sd,
         tp_distance_1km = ifelse(tp_distance_m <= 1000, 1, 0),
         tp2 = tp^2)


#---------
x_struct = c("lsize","larea", "air","heat","fireplcs", 
             "baths", "bedrooms", "storeys",
             "quality","lbasement_area",
             "pool","renovation_effect","lcity",
             "ab_ind","ab_comm","ab_inst","ab_educ","ab_golf",
             "lgar_area")

f_sd <- paste("log(price_real)  ~",
        #   "sd +",
           paste("i(sd, distance_bin)"),"+",
           paste("i(distance_bin)"),"+",
           paste(x_struct, collapse=" + "), 
           "  | year + csw0(township)")

f_sd <- paste("log(price_real)  ~",
           #   "sd +",
           paste("i(sd_band, region)"),"+",
           paste("i(sd_distance_1km)"),"+",
           paste(x_struct, collapse=" + "), 
           "  | year + csw0(township)")


f_sd <- paste("log(price_real)  ~",
              #   "sd +",
              paste("i(sd_band, region)"),"+",
              paste("i(sd_distance_1km)"),"+",
              paste(x_struct, collapse=" + "), 
              "  | year^county + csw0(township)")


f_tp <- paste("log(price_real)  ~",
              #   "sd +",
              paste("i(tp, tp_distance_1km, ref = 0)"),"+",
              paste("i(tp2, tp_distance_1km, ref = 0)"),"+",
              paste("i(tp_distance_1km)"),"+",
              paste(x_struct, collapse=" + "), 
              "  | year + csw0(township)")

f2 <- paste("log(price_real)  ~",
           paste("i(log(tp_river), distance_1km, ref = 0)"),"+",
           paste("i(distance_1km)"),"+",
           paste(x_struct, collapse=" + "), 
           "  | year + csw0(township)")

#remove_counties = c("Oxford","Huron","Perth","Dufferin","Waterloo","Peel",
#                    "Northumberland","Niagara","Ottawa","Prescott and Russell")

inner_counties = c("Kawartha Lakes","Muskoka","Peterborough","Nipissing", "Sudbury")
outer_counties = c("Simcoe", "Grey", "Bruce", "Dufferin")

east_counties = c("Leeds and Grenville","Northumberland","Hastings",
                          "Lennox and Addington", "Frontenac", "Lanark",
                          "Ottawa", "Prescott and Russell", "Prince Edward",
                          "Stormont, Dundas and Glengarry","Renfrew")
#north_counties = c("Nipissing", "Sudbury")
sw_counties = c("Huron", "Oxford", "Perth","Wellington", "Waterloo", "Chatham-Kent", "Elgin",
                        "Niagara","Middlesex", "Lambton","Hamilton", "Haldimand-Norfolk","Essex")
gta_counties = c("York", "Peel", "Toronto", "Halton", "Durham")

include_counties = c(inner_counties, outer_counties, east_counties)

df_dist = df %>%
  filter(sd_distance_m <= 200000000) %>%
  mutate(region = case_when(county %in% inner_counties ~ "inner",
         county %in% outer_counties ~ "outer",
county %in% east_counties ~ "east",
county %in% north_counties ~ "north",
county %in% sw_counties ~ "sw",
county %in% gta_counties ~ "gta", 
TRUE ~ "other")) %>%
  filter(region != "other")



df_subset = df_dist %>%
#  filter((county %in% include_counties)) %>%
    filter(!(county %in% sw_counties)) %>%
  filter(between(price_real, quantile(price_real, .01), 
                 quantile(price_real, .99))) %>%
  filter(sd_distance_m <= 20000)


res_multi = feols(as.formula(f_sd), 
                  df_subset,
                  cluster = ~township + year)
etable(res_multi)


df_dist = df %>%
  filter(tp_distance_m <= 200000000)


df_subset = df_dist %>%
  filter((county %in% include_counties)) %>%
  filter(between(price_real, quantile(price_real, .01), 
                 quantile(price_real, .99))) %>%
  filter(tp_distance_m <= 20000)


res_multi = feols(as.formula(f_tp), 
                  df_subset,
                  cluster = ~township + year)
etable(res_multi)


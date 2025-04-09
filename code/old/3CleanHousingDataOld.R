
# Cleans environment
rm(list=ls(all=TRUE))

library(tidyverse)
library(haven)

library(lubridate)
library(janitor)


##Import data
##I use dta file (in STATA) to open the full data set and choose the variables based on our model demand.
##I extract the data set from the dta file and convert it into .csv format in STATA 

df_house = read.csv("data/HouseData/rural_sales_0216.csv", header = T)

## Import Housing price index (HPI) data (Base year 2016)
df_hpi = read_csv("data/HouseData/house_price_index_ontario.csv")

df_hpi = df_hpi %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
  select(date, hpi)

df_house_stata = read_dta("data/HouseData/Rural res sales - 2002-16.dta")

df_house_2021= read_sav("data/HouseData/Rural residential sales 2021 (reg 1 to 15).sav") %>%
  bind_rows(read_sav("data/HouseData/Rural residential sales 2021 (reg 16 to 28).sav"))

df_house_all_rr= read_delim("data/HouseData/S29_All_RR_Variables.txt",
                            delim = ",") %>%
  select(roll_num = ARN, 
         latitude = Latitude_NAD83, 
         longitude = Longitude_NAD83)

names(df_house_20212)

df_house_20212 = df_house_2021 %>%
  left_join(df_house_all_rr) 

df_house_coordinates = df_house_stata %>%
  select(roll_num, address, saledate, latitude, longitude)

df_house_coordinates = df_house_20212 %>%
  select(roll_num, address, saledate, latitude, longitude) %>%
  bind_rows(df_house_coordinates) 

summary(df_house_coordinates)

df_house_coordinates = df_house_coordinates %>%
  distinct(roll_num, address, latitude, longitude) %>%
  group_by(roll_num) %>%
  summarise(latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T))

write_csv(df_house_coordinates, "data/HouseData/df_house_coordinates_all.csv")
rm(df_house_coordinates)
gc()
df_all = df_house_20212 %>%
  mutate(bsmttype = as.character(bsmttype),
         r_hnbhd_res = as.numeric(r_hnbhd_res)) %>%
  bind_rows(df_house_stata)


summary(df_house_lat2)

var.labels <- lapply(df_house_stata, attr, "label") # Gives you list of the labeled variables
saveRDS(var.labels, "data/HouseData/house_variable_key.rds")

df_all = bind_rows(mutate_all(df_house_stata, as.character), 
                         mutate_all(df_house_20212, as.character))

df_house_export  = df_all %>%
  select(roll_num, address, saletype, propcode,
     saledate, sale_amt, 
     yr_blt, yrblteff, quality,
     storeys,bsmtarea,fireplcs,baths,bedrooms,
     area_tot, aircond,
     pooliara,pooloara,
     heattype,
     wfront_l,wfront_r,wfront_c,
     ab_ind,ab_comm,ab_inst,ab_educ,ab_golf,
     inst_num, roll_num, address, propcode, saletype,
     wtr_well, wtr_mun, wtr_shw, wtr_l_r, wtr_no, wtr_na, wtr_unspec, 
     hydro_no, san_mun, san_sept,
     view_wf, view_gd, view_pr,
     floodp_r, floodp_d,
     sh_sandy, sh_rocky, sh_gravl, sh_weedy, sh_shal, sh_deep) %>%
  mutate(year = as.integer(substr(saledate, 1, 4)),
     date = as_date(parse_date_time(saledate, "ym")),
     age = year - as.numeric(yr_blt),
     renovation_effect = year - as.numeric(yrblteff),
     lbasement_area = ifelse(bsmtarea == 0, 0, log(as.numeric(bsmtarea))),
     sale_id = 1:n(),
     county2 = substr(roll_num, 1, 2),
     city = str_sub(roll_num, 3, 4), 
     ward = str_sub(roll_num, 5, 7),
     area = str_sub(roll_num, 8, 10),
     street = str_sub(roll_num, 11, 15),
     street2= str_sub(roll_num, 16, 19),
     total_pool = as.numeric(pooliara) +as.numeric(pooloara),
     pool = ifelse(total_pool > 0 , 1 ,0)) %>%
  left_join(df_hpi, by = "date") %>%
  mutate(price_real = as.numeric(sale_amt) * hpi / 100) %>%
  select(-hpi)

saveRDS(df_house_export, "data/df_house_new.rds")
saveRDS(df_all, "data/df_house_all.rds")


# 19 – County or Municipal District
# 04 – City or Town
# 031 – Ward
# 200 – Area Subcenterision
# 01100 – Street Subcenterision
# 0000 – Plate number (formerly used in realty/business assessments)

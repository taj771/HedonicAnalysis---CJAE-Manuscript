

df_house_02_16 = read_dta("data/HouseData/Rural res sales - 2002-16.dta")

var.labels <- lapply(df_house_02_16, attr, "label") # Gives you list of the labeled variables
saveRDS(var.labels, "data/HouseData/house_02_16_variable_key.rds")

df_house_16_20= read_sav("data/HouseData/Rural residential sales 2021 (reg 1 to 15).sav") %>%
  bind_rows(read_sav("data/HouseData/Rural residential sales 2021 (reg 16 to 28).sav"))

df_house_all_rr= read_delim("data/HouseData/S29_All_RR_Variables.txt",
                            delim = ",") %>%
  select(roll_num = ARN, 
         latitude = Latitude_NAD83, 
         longitude = Longitude_NAD83)

names(df_house_16_20)

df_house_16_20 = df_house_16_20 %>%
  left_join(df_house_all_rr) 

df_house_coordinates = df_house_02_16 %>%
  select(roll_num, address, saledate, latitude, longitude)

df_house_coordinates = df_house_16_20 %>%
  select(roll_num, address, saledate, latitude, longitude) %>%
  bind_rows(df_house_coordinates) 


write.csv(df_house_coordinates, "./shapefile/housecoordinate.csv")

summary(df_house_coordinates)

df_house_coordinates = df_house_coordinates %>%
  distinct(roll_num, address, latitude, longitude) %>%
  group_by(roll_num) %>%
  summarise(latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T))

write_csv(df_house_coordinates, "data/HouseData/df_house_coordinates_missing.csv")
rm(df_house_coordinates)
gc()

df_all = bind_rows(mutate_all(df_house_02_16, as.character), 
                   mutate_all(df_house_16_20, as.character))

saveRDS(df_all, "data/df_house_all.rds")

df_all = readRDS("data/df_house_all.rds")

x_numeric = c("yr_blt",  "yrblteff", "bsmtarea", "fireplcs", 
              "baths", "bedrooms", "storeys",
              "quality","bsmtarea", "propcode",
              "area_tot",
              "pooliara", "pooloara",
              "lotsize",
              "sale_amt")



df_house_export  = df_all %>%
  mutate(year = as.integer(substr(saledate, 1, 4)),
         date = as_date(parse_date_time(saledate, "ym"))) %>%
  select(roll_num, address, saledate, date, year, sale_amt, saletype, propcode, saletype,
     yr_blt, yrblteff, quality,
     storeys,bsmtarea,fireplcs,baths,bedrooms,
     lotsize, 
     area_tot, aircond, pooliara,pooloara, heattype,
     wfront_l,wfront_r,wfront_c,
     ab_ind,ab_comm,ab_inst,ab_educ,ab_golf,
     wtr_well, wtr_mun, wtr_shw, wtr_l_r, wtr_no, wtr_na, wtr_unspec, 
     hydro_no, san_mun, san_sept,
     view_wf, view_gd, view_pr,
     floodp_r, floodp_d,
     sh_sandy, sh_rocky, sh_gravl, sh_weedy, sh_shal, sh_deep) %>%
  mutate(across(all_of(x_numeric), ~as.numeric(.))) %>%
  mutate(across(wfront_l:sh_deep, ~as.numeric(.))) 
  
saveRDS(df_house_export, "data/df_house.rds")

# 19 – County or Municipal District
# 04 – City or Town
# 031 – Ward
# 200 – Area Subcenterision
# 01100 – Street Subcenterision
# 0000 – Plate number (formerly used in realty/business assessments)

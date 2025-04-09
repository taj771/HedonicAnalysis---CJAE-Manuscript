# clear memory
rm(list = ls())


# load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
sf,
dplyr
)

bfp_0_250m <- st_read("./shapefile/bfp_count_cen_da_004_250m_98_4454sqm.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)


bfp_250_500m <- st_read("./shapefile/bfp_count_cen_da_004_500m_98_4454sqm.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)

st_write(bfp_250_500m, "./shapefile/bfp_count_cen_da_004_250m_500m_98_4454sqm.shp", delete_layer = T)

# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("./shapefile/bfp_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)


bfp_0_250m <- st_read("./shapefile/bfp_count_cen_da_004_250m_98_4454sqm.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = 0.6/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m, "./shapefile/bfp_count_cen_da_250m_value.shp",delete_layer = T)


sum(bfp_0_250m$bfp_count_250, na.rm = T)
sum(bfp_0_250m$value_250m, na.rm = T)




bfp_250_500m <- st_read("shapefile/bfp_count_cen_da_004_250m_500m_98_4454sqm.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = 0.53/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)

st_write(bfp_250_500m, "./shapefile/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


sum(bfp_250_500m$bfp_count_500, na.rm = T)
sum(bfp_250_500m$value_500m, na.rm = T)

sf_use_s2(FALSE)

bfp_0_250m_csd <- bfp_0_250m%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))


bfp_250_500m_csd <- bfp_250_500m%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))



df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

sum(df_value_map$Total_value)


st_write(df_value_map, "./shapefile/map_values.shp", delete_layer = T )

tax_gain <- df_value_map%>%
  mutate(prop_tax = Total_value*0.015)

sum(tax_gain$prop_tax)
max(tax_gain$prop_tax)


#df2<-df1[!(df1$Name=="George" | df1$Name=="Andrea"),]


#########################################################


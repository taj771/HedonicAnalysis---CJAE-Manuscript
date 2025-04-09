
## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf
)


# Parameters
# elasticity 
e_250m <- 0.59/100
e_500m <- 0.53/100

# water qualiy change
wq_change <- 1

# bilding foof print area definitions
ub <- 2000
lb <- 40


# load files
# building foot print
df_bf <- st_read("./shapefile/on.shp")%>%
  st_transform(df_bf, crs = 3348)

# census
df_cen <- st_read("./shapefile/census_on_da_SA.shp")%>%
  st_transform(df_cen, crs = 3348)

# filter polygonns only that predefined area size
df_bf_filter <- df_bf%>%
  subset(areasqm > lb & areasqm < ub)

# spatial join filterbfp and census DA
bf_cen_da <- st_join(df_bf_filter,df_cen, all = TRUE )

# count bfp at each DA
bf_cen_da_count <- bf_cen_da%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

write.csv(bf_cen_da_count, "./shapefile/bf_count_da.csv")


# 250 - bfp within 250m boundary
df_bf_250 <- st_read("./shapefile/on_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(df_bf_250, crs = 3348)

bf_cen_da_250 <- st_join(df_bf_250,df_cen, all = TRUE)

bf_cen_da_count_250 <- bf_cen_da_250%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_250" = "n")


write.csv(bf_cen_da_count_250,"./shapefile/can_250_bf_count.csv", row.names=FALSE)


# 500 - bfp within 500m boundary
df_bf_500 <- st_read("./shapefile/on_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(df_bf_500, crs = 3348)

bf_cen_da_500 <- st_join(df_bf_500,df_cen, all = TRUE )

bf_cen_da_count_500 <- bf_cen_da_500%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_500" = "n")

write.csv(bf_cen_da_count_500,"./shapefile/can_500_bf_count.csv",row.names=FALSE)


############################################################
# to save time load files processed above to derive economic value

df_cen <- st_read("./shapefile/census_on_da_SA.shp")%>%
  st_transform(df_cen, crs = 3348)
bf_cen_da_count <- read_csv("./shapefile/bf_count_da.csv")
bf_cen_da_count_250 <- read_csv("./shapefile/can_250_bf_count.csv")
bf_cen_da_count_500 <- read_csv("./shapefile/can_500_bf_count.csv")

e_250m <- 0.59/100
e_500m <- 0.53/100

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)

df_cen_value <- df_cen%>%
  left_join(bf_cen_da_count_250)%>%
  left_join(bf_cen_da_count_500)%>%
  filter(!if_all(c(bfp_250, bfp_500), is.na))%>%
  drop_na(AvgDwlv)%>%
  filter(AvgDwlv!=0)%>%
  select(GeoUID,Type,CD_UID,Dwllngs,Popultn,CSD_UID,AvgDwlv,bfp_250,bfp_500)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(bfp_250_500 = bfp_500-bfp_250)%>%
  relocate(bfp_250_500, .before=geometry)%>%
  left_join(bf_cen_da_count)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  relocate(ratio, .before=geometry)%>%
  mutate(adj_bfp_250 = bfp_250*ratio)%>%
  mutate(adj_bfp_250_500 = bfp_250_500*ratio)%>%
  mutate_at(vars(adj_bfp_250, adj_bfp_250_500), list(~ round(., 0)))%>%
  mutate(e_250 = e_250m)%>%
  mutate(e_500 = e_500m)%>%
  #mutate(d_wq = wq_change )%>%
  mutate(val_wf = e_250*adj_bfp_250*AvgDwlv)%>%
  mutate(val_nwf = e_500*adj_bfp_250_500*AvgDwlv)%>%
  mutate(val_tot = val_wf + val_nwf)%>%
  mutate(val_wf_pp = val_wf/Popultn)%>%
  mutate(val_nwf_pp = val_nwf/Popultn)%>%
  mutate(val_tot_pp = val_wf_pp + val_nwf_pp)%>%
  select(GeoUID,val_wf,val_nwf,val_tot,val_wf_pp,val_nwf_pp,val_tot_pp,ratio,adj_bfp_250,adj_bfp_250_500,AvgDwlv,CSD_UID,Popultn)%>%
  as.data.frame()%>%
  select(-geometry)


df_value <- df_cen%>%
  select(GeoUID)%>%
  left_join(df_cen_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


st_write(df_value,"./shapefile/value_map.shp", delete_layer = T )
  
sum(df_value$adj_bfp_250)
sum(df_value$val_wf)/1000000
sum(df_value$adj_bfp_250_500)
sum(df_value$val_nwf)/1000000
sum(df_value$val_tot)/1000000
mean(df_value$val_wf_pp)
mean(df_value$val_nwf_pp)
mean(df_value$val_tot_pp)
min(df_value$AvgDwlv)
max(df_value$AvgDwlv)
mean(df_value$AvgDwlv)

# tax
tax_gain <- df_value%>%
  mutate(prop_tax = val_tot*0.015)

sum(tax_gain$prop_tax)
max(tax_gain$prop_tax)


df_value_csd <- df_value%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot)/1000000,
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(adj_bfp_250),
            modbfp_500 = sum(adj_bfp_250_500))%>%
  as.data.frame()%>%
  select(-geometry)

cen_can <- st_read("./shapefile/lcsd000b21a_e_SA.shp")%>%
  rename("CSD_UID" = "CSDUID")


value_csd <- cen_can%>%
  subset(PRUID == 35)%>%
  select(CSD_UID)%>%
  left_join(df_value_csd)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


st_write(value_csd ,"./shapefile/value_map_csd.shp", delete_layer = T )




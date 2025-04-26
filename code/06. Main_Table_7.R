#################################################################
# Input files are create in 59. Value maps
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
  mutate(tot_bfp = adj_bfp_250+adj_bfp_250_500)%>%
  mutate(tot_prop_value = tot_bfp*AvgDwlv)%>%
  mutate(wf_prop_value =adj_bfp_250*AvgDwlv)%>%
  mutate(nwf_prop_value =adj_bfp_250_500*AvgDwlv)%>%
  select(GeoUID,val_wf,val_nwf,val_tot,val_wf_pp,val_nwf_pp,val_tot_pp,ratio,adj_bfp_250,adj_bfp_250_500,AvgDwlv,wf_prop_value,nwf_prop_value,tot_prop_value,CSD_UID,Popultn)%>%
  as.data.frame()%>%
  select(-geometry)


df_value <- df_cen%>%
  select(GeoUID)%>%
  left_join(df_cen_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


st_write(df_value,"./shapefile/value_map.shp", delete_layer = T )


df_final <- df_value%>%
  select(adj_bfp_250,val_wf,adj_bfp_250_500,val_nwf,val_tot,val_wf_pp,
         val_nwf_pp,val_tot_pp,AvgDwlv,wf_prop_value,nwf_prop_value,tot_prop_value)%>%
  mutate(totnum_bfp_250 = sum(adj_bfp_250),
         totnum_bfp_250_500 = sum(adj_bfp_250_500),
         tot_bfp = totnum_bfp_250+totnum_bfp_250_500,
         tot_val_wf = sum(val_wf/1000000),
         tot_val_nwf = sum(val_nwf/1000000),
         tot_val = tot_val_wf+tot_val_nwf,
         tot_value_prop = sum(tot_prop_value),
         tot_val_pp = mean(val_tot_pp),
         tot_wf_prop_val = sum(wf_prop_value),
         tot_nwf_prop_val = sum(nwf_prop_value),
         
         )%>%
  select(totnum_bfp_250,totnum_bfp_250_500,tot_bfp,tot_val_wf,tot_val_nwf,tot_val,tot_val_pp,tot_wf_prop_val,tot_nwf_prop_val,tot_value_prop)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  distinct(,.keep_all = T)


# tax
tax_gain <- df_value%>%
  mutate(prop_tax = val_tot*0.015)

sum(tax_gain$prop_tax)
max(tax_gain$prop_tax)




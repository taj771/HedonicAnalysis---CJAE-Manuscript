rm(list=ls(all=TRUE))
library(sf)

df_subset = readRDS( "data/analysis_sample.rds")

df_subset <- df_subset%>%
  select(lake_index_sd,station_lat_lon)%>%
  drop_na()

df = df_subset %>%
  distinct(lake_index_sd, station_lat_lon) %>%
  mutate(lat = round(as.numeric(gsub( " .*$", "", station_lat_lon )), 4),
         lon = round(as.numeric(gsub( ".* ", "", station_lat_lon )), 4)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(lake_index_sd, station_lat_lon, geometry)

#st_write(df, "shapefile/stations.shp")

#library(leaflet)

#leaflet() %>% 
  #addTiles() %>% 
  #addPoints(data = df, # borders of all counties
              #color = "black", 
              #fill = NA, 
              #weight = 1.5)

#library(leaflet)
#leaflet(df) %>% addTiles() %>% addMarkers(popup = ~ lake_index_sd)


# Discriptive statistics

df_subset = readRDS("data/analysis_sample.rds")


df_subset %>%
  mutate(sd_average_miss = ifelse(is.na(sd_average), 1, 0),
         sd_interp_miss = ifelse(is.na(sd_interp), 1, 0),
         log_sd_500_miss = ifelse(is.na(log_sd_500), 1, 0),
         log_sd_inter_500_miss = ifelse(is.na(log_sd_inter_500), 1, 0)) %>%
  group_by(log_sd_500, log_sd_inter_500) %>%
  summarise(count = n())


df_main = df_subset %>%
  mutate(log_sd_inter_500 = ifelse(abs(parse_number(interpolation)) > 5 & 
                                     log_sd_inter_500 != 0, 
                                   NA, log_sd_inter_500))


df_repeat = df_subset %>%
  select(roll_num, county, ward, year, sale_month, price_real, prop_type, age, 
         log_sd_front, log_sd_500, log_sd_inter_500, log_sd_1000, log_sd_500_gl, log_sd_500_gl_no, region,
         dist_house_lake, sd_average,sd_interp,lake_area_sqkm,distance_cma_metres,age,area,baths,bedrooms,great_lake, 
         dist_250m,dist_250m_500m,lotsize,pool,air_cond,storeys)

df_repeat = df_repeat  %>%
  distinct(roll_num, year, .keep_all = T) %>%
  group_by(roll_num) %>%
  filter(length(unique(prop_type)) == 1) %>%
  mutate(n_sales = n()) %>%
  filter(n_sales > 1) %>%
  ungroup(.)


df_tab1 <- df_main%>%
  select(price_real, dist_house_lake, sd_average,sd_interp,lake_area_sqkm,distance_cma_metres,age,area,baths,bedrooms,lotsize,great_lake, 
         dist_250m,dist_250m_500m,pool,air_cond,storeys)

df_tab1$great_lake[df_tab1$great_lake=="0"] <- "nongl"
df_tab1$great_lake[df_tab1$great_lake=="1"] <- "gl"


df_tab2 <- df_main%>%
  select(price_real, dist_house_lake, sd_average,sd_interp,lake_area_sqkm,distance_cma_metres,age,area,baths,bedrooms,lotsize,
         dist_250m,dist_250m_500m,pool,air_cond,storeys)%>%
  mutate(great_lake = "full")

df_tab3 <- df_repeat%>%
  select(price_real, dist_house_lake, sd_average,sd_interp,lake_area_sqkm,distance_cma_metres,age,area,baths,bedrooms,lotsize,
         dist_250m,dist_250m_500m,pool,air_cond,storeys)%>%
  mutate(great_lake = "repeat")
  

df <- rbind(df_tab1,df_tab2,df_tab3)

library(vtable)
st(df, group = 'great_lake', group.long = F)

st(df, group = 'great_lake', group.long =  F, out = "latex", file = "./results/table1.tex")


#################################################################################

summary <- df_main %>%
select(price_real, sd_average, sd_interp, lotsize, dist_250m, dist_250m_500m, 
       distance_cma_km, distance_toronto_km, great_lake) %>% # select variables to summarise
  rename("pricereal" = "price_real",
         "sdaverage" = "sd_average",
         "sdinterp" = "sd_interp",
         "dist250m" = "dist_250m",
         "dist250m500m" = "dist_250m_500m",
         "distancecmakm" = "distance_cma_km",
         "distancetorontokm" = "distance_toronto_km",
         "greatlake" = "great_lake")%>%
  summarise_each(funs(mean = mean(., na.rm = T),
                      sd = sd(., na.rm = T),
                      median = median(., na.rm = T),
                      min = min(., na.rm = T), 
                      max = max(., na.rm = T)
                      ))

df.stats.tidy <- summary %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd, median, min, max) # reorder columns

#xtable(df.stats.tidy)


#datasummary_df(df.stats.tidy, output = "./results/summary_table.docx")


#print.xtable(xtable(df.stats.tidy), file = "./results/summary.tex")

sum(df_main$dist_250m==1)
sum(df_main$dist_250m_500m==1)

c <- df_main%>%
  subset(dist_250m==1)%>%
  select(dist_250m,sd_average,sd_interp)


t <- df_main%>%
  subset(dist_250m==1 & sd_average> 0)%>%
  select(dist_250m,sd_average,sd_interp)

tt <- df_main%>%
  subset(dist_250m==1 & sd_interp>0)%>%
  select(dist_250m,sd_average,sd_interp)

nrow(tt)-nrow(t)

t500 <- df_main%>%
  subset(dist_250m_500m==1 & sd_average>0)%>%
  select(dist_250m_500m,sd_average,sd_interp)

tt500 <- df_main%>%
  subset(dist_250m_500m==1 & sd_interp>0)%>%
  select(dist_250m_500m,sd_average,sd_interp)


nrow(tt500)-nrow(t500)

cc <- df_main%>%
  select(sd_average,sd_interp)

secchi_ave <- df_main%>%
  select(sd_average)%>%
  drop_na()

secchi_interp <- df_main%>%
  select(sd_interp)%>%
  drop_na()

nrow(secchi_interp)-nrow(secchi_ave)

secchi_interp <- df_main%>%
  select(sd_interp)%>%
  drop_na()


t <- df_main%>%
  subset(dist_500m==1)%>%
  select(sd_interp)%>%
  drop_na()

119420 - nrow(t)

119420 - 82650


df_main <- df_main%>%
  mutate(distance_cma_km = distance_cma_metres/1000,
         distance_toronto_km = distance_toronto_metres/1000 )


df <-df_main%>%
  select(price_real,sd_average,sd_interp, lotsize,log_area_tot,
         log_dist_toronto,log_dist_cma,prop_type,pool,air_cond , 
          heat_elect,heat_forcedair,heat_hw,floodp_d,floodp_r,view_pr ,
          view_gd,view_wf,wtr_l_r,wtr_well,wtr_mun,wfront_l,wfront_r,ab_ind ,
          ab_comm,ab_inst,ab_educ,ab_golf,fireplcs,baths,bedrooms,storeys,
          quality,lbasement_area,age, dist_500m, dist_250m, dist_250m_500m,
         distance_cma_km, distance_toronto_km, great_lake,distance_cma_km,station_lat_lon,year,dist_house_lake,
         baths_dum, bedrooms_dum, roll_num, region_lakes, interpolation,log_sd_inter_500,region_lakes,roll_num, saletype,ward,area_tot,NEAR_DIST_HWY,
         township,county)

df1 <- subset(df, df$dist_500m==1)

df2 <- subset(df, df$dist_500m==0)

  
df1 <- df1%>%
  filter(!is.na(price_real))%>%
  filter(!is.na(sd_interp))%>%
  filter(!is.na(lotsize))%>%
  filter(!is.na(log_area_tot))%>%
  filter(!is.na(log_dist_toronto))%>%
  filter(!is.na(log_dist_cma))%>%
  filter(!is.na(prop_type))%>%
  filter(!is.na(pool))%>%
  filter(!is.na(air_cond))%>%
  filter(!is.na(heat_elect))%>%
  filter(!is.na(heat_forcedair))%>%
  filter(!is.na(heat_hw))%>%
  filter(!is.na(floodp_d))%>%
  filter(!is.na(floodp_r))%>%
  filter(!is.na(view_pr))%>%
  filter(!is.na(view_gd))%>%
  filter(!is.na(view_wf))%>%
  filter(!is.na(wtr_l_r))%>%
  filter(!is.na(wtr_well))%>%
  filter(!is.na(wtr_mun))%>%
  filter(!is.na(wfront_l))%>%
  filter(!is.na(wfront_r))%>%
  filter(!is.na(ab_ind))%>%
  filter(!is.na(ab_comm))%>%
  filter(!is.na(ab_inst))%>%
  filter(!is.na(ab_educ))%>%
  filter(!is.na(ab_golf))%>%
  filter(!is.na(fireplcs))%>%
  filter(!is.na(baths))%>%
  filter(!is.na(bedrooms))%>%
  filter(!is.na(storeys))%>%
  filter(!is.na(quality))%>%
  filter(!is.na(lbasement_area))%>%
  filter(!is.na(age))%>%
  filter(!is.na(bedrooms_dum))%>%
  filter(!is.na(baths_dum))
  
  
df2 <- df2%>%
  filter(!is.na(price_real))%>%
  filter(!is.na(lotsize))%>%
  filter(!is.na(log_area_tot))%>%
  filter(!is.na(log_dist_toronto))%>%
  filter(!is.na(log_dist_cma))%>%
  filter(!is.na(prop_type))%>%
  filter(!is.na(pool))%>%
  filter(!is.na(air_cond))%>%
  filter(!is.na(heat_elect))%>%
  filter(!is.na(heat_forcedair))%>%
  filter(!is.na(heat_hw))%>%
  filter(!is.na(floodp_d))%>%
  filter(!is.na(floodp_r))%>%
  filter(!is.na(view_pr))%>%
  filter(!is.na(view_gd))%>%
  filter(!is.na(view_wf))%>%
  filter(!is.na(wtr_l_r))%>%
  filter(!is.na(wtr_well))%>%
  filter(!is.na(wtr_mun))%>%
  filter(!is.na(wfront_l))%>%
  filter(!is.na(wfront_r))%>%
  filter(!is.na(ab_ind))%>%
  filter(!is.na(ab_comm))%>%
  filter(!is.na(ab_inst))%>%
  filter(!is.na(ab_educ))%>%
  filter(!is.na(ab_golf))%>%
  filter(!is.na(fireplcs))%>%
  filter(!is.na(baths))%>%
  filter(!is.na(bedrooms))%>%
  filter(!is.na(storeys))%>%
  filter(!is.na(quality))%>%
  filter(!is.na(lbasement_area))%>%
  filter(!is.na(age))%>%
  filter(!is.na(bedrooms_dum))%>%
  filter(!is.na(baths_dum))
  

nrow(df1)+nrow(df2)

df12 <- rbind(df1,df2)

summary <- df12 %>%
  select(price_real, sd_average, sd_interp, lotsize, dist_250m, dist_250m_500m, 
         distance_cma_km, distance_toronto_km, great_lake) %>% # select variables to summarise
  rename("pricereal" = "price_real",
         "sdaverage" = "sd_average",
         "sdinterp" = "sd_interp",
         "dist250m" = "dist_250m",
         "dist250m500m" = "dist_250m_500m",
         "distancecmakm" = "distance_cma_km",
         "distancetorontokm" = "distance_toronto_km",
         "greatlake" = "great_lake")%>%
  summarise_each(funs(mean = mean(., na.rm = T),
                      sd = sd(., na.rm = T),
                      median = median(., na.rm = T),
                      min = min(., na.rm = T), 
                      max = max(., na.rm = T)
  ))

df.stats.tidy <- summary %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd, median, min, max) # reorder columns

  
  
  
datasummary_df(df.stats.tidy, output = "./results/summary_table.docx")



summary <- df12 %>%
  select(air_cond,area_tot,bedrooms,baths,dist_250m,dist_250m_500m,distance_cma_km,distance_toronto_km,
         heat_elect,lotsize,pool,price_real,sd_average,sd_interp,storeys,NEAR_DIST_HWY) %>% # select variables to summarise
  rename("Air Condition (1/0)" = "air_cond",
         "Total Area (m2)" = "area_tot",
         "Number of Bathrooms" = "baths",
         "Number of Bedrooms" = "bedrooms",
         "Real Price ($2021)" = "price_real",
         "sdaverage" = "sd_average",
         "sdinterp" = "sd_interp",
         "Lakeshore within 250m (1/0)" = "dist_250m",
         "Lakeshore within 250m 500m (1/0)" = "dist_250m_500m",
         "Distance to nearest CMA (km)" = "distance_cma_km",
         "Distance to Toronto (km)" = "distance_toronto_km",
         "Electric Heat (1/0)"="heat_elect",
         "Lot size (m2)"="lotsize",
         "Pool (1/0)" = "pool",
         "Measured SD (m)"="sd_average",
         "Measured SD+ Interpolated SD (m)" ="sd_interp",
         "Number of stories" = "storeys",
         "Distance to nearest highway (Km)" = "NEAR_DIST_HWY" )%>%
  summarise_each(funs(mean = mean(., na.rm = T),
                      sd = sd(., na.rm = T),
                      median = median(., na.rm = T),
                      min = min(., na.rm = T), 
                      max = max(., na.rm = T)
  ))

df.stats.tidy <- summary %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd, median, min, max) # reorder columns


datasummary_df(df.stats.tidy, output = "./results/summary_tableappendix.docx")



#############################################################################

#sd_average vs sd_interpolation data comparision

mean(df12$sd_interp, na.rm = T)

t <- df12%>%
  subset(dist_500m == 1)%>%
  select(sd_average, sd_interp, interpolation)

mean(t$sd_average, na.rm = T)
mean(t$sd_interp, na.rm = T)

t1 <- df12%>%
  subset(dist_500m == 1)%>%
  select(sd_average, sd_interp, interpolation)%>%
  subset(interpolation !="none")



t2 <- df_repeat%>%
  subset(dist_500m == 1)%>%
  select(sd_average, sd_interp, interpolation)

mean(t2$sd_average, na.rm = T)
mean(t2$sd_interp, na.rm = T)

t3 <- df_repeat%>%
  subset(dist_500m == 1)%>%
  select(sd_average, sd_interp, interpolation)%>%
  subset(interpolation !="none")

###############################################################################
# property transaction map


df_house <- read.csv("./shapefile/housecoordinate.csv")%>%
  drop_na(longitude)

df_list <- subset(df_house, roll_num %in% df12$roll_num)


df_house <- st_as_sf(df_list, coords = c("longitude", "latitude"), crs = 4326)


st_write(df_house, "./shapefile/sf_house.shp")

df_transaction_csd <- st_read("./shapefile/transactionnumbersmap.shp")

#############################################################################

# SD collection point map

SD_measured <- df12%>%
  drop_na(sd_average)%>%
  distinct(station_lat_lon, .keep_all = T)%>%
  select(roll_num)


df_house <- read.csv("./shapefile/housecoordinate.csv")%>%
  drop_na(longitude)

df_list <- subset(df_house, roll_num %in% SD_measured$roll_num)%>%
  distinct(roll_num, .keep_all = T)


SD_collection_p <- st_as_sf(df_list, coords = c("longitude", "latitude"), crs = 4326)

st_write(SD_collection_p, "./shapefile/SD_collection_P.shp", delete_layer = T)


##########################################################################
# regional vs nonregional lakes map

df_house <- read.csv("./shapefile/housecoordinate.csv")%>%
  drop_na(longitude)%>%
  select(roll_num,longitude,latitude)


df_list <- subset(df_house, roll_num %in% df12$roll_num)

df_reglakes <- df12%>%
  select(roll_num, region_lakes)


df_list <- df_list%>%
  left_join(df_reglakes)%>%
  distinct(roll_num, .keep_all = T)


lakesregion <- st_as_sf(df_list, coords = c("longitude", "latitude"), crs = 4326)

st_write(lakesregion, "./shapefile/lakesregion.shp", delete_layer = T)





#############################################################################
# SD graph
t <- select(df12, station_lat_lon, year, sd_average, sd_interp )%>%
  filter(!is.na(sd_interp))

t <- t%>%
  group_by(station_lat_lon, year)%>%
  mutate(mu_ave=mean(sd_average))

tt <- t%>%
  distinct(station_lat_lon, year, .keep_all = TRUE)


################# interpolation

t5 <- select(df12, station_lat_lon, year, sd_interp, interpolation )

t5 <- t5 %>%
  filter(!is.na(sd_interp))

#t5<-t5[!(t5$interpolation=="none"),]


t5 <- t5%>%
  group_by(station_lat_lon, year)%>%
  mutate(mu_inte = mean(sd_interp))

t5 <- t5%>%
  distinct(station_lat_lon, year, .keep_all = TRUE)%>%
  select(station_lat_lon, mu_inte, interpolation)



df <- select(df12, year, station_lat_lon)%>%
  distinct(station_lat_lon, year, .keep_all = TRUE)
  

df <- df%>%
  left_join(t5, by = c("year" = "year", "station_lat_lon" = "station_lat_lon"))%>%
  #left_join(tt, by = c("year" = "year", "station_lat_lon" = "station_lat_lon"))%>%
  select(year, mu_inte)



p1 <- ggplot(aes(x=year),data=df)+
  coord_cartesian(xlim=c(2002, 2020),ylim=c(0,17))+
  #geom_point(aes(y=mu_ave, colour = "sd_ave"), alpha=1/10)+
  #geom_line(aes(y=mu_ave, colour = "sd_ave"), stat='summary',fun.y=mean)+
  geom_point(aes(y=mu_inte,colour = "SD"), alpha=1/10)+
  geom_line(aes(y=mu_inte, colour = "SD"), stat='summary',fun.y=mean)+
  scale_colour_manual("", 
                      breaks = c('sd_ave','sd_ave','SD','SD'),
                      values = c("red", "red","dodgerblue", "dodgerblue")) +
  labs(y = "Secchi depth in m", x = "year")+
  theme_bw()
p1

##############################################################################
# Property price graph 

t <- select(df_main, price_real, year, dist_250m, dist_250m_500m, dist_500m_1000m, dist_1000m_2000m,
            dist_2000m_plus)%>%
  drop_na()

tt<- t%>%
  group_by( year)%>%
  mutate(mu_tot=mean(price_real))%>%
  distinct(year,.keep_all = TRUE)

ttt <- t%>%
  subset(dist_250m == '1')

t3<- ttt%>%
  group_by( year)%>%
  mutate(mu_250=mean(price_real))%>%
  distinct(year,.keep_all = TRUE)%>%
  select(mu_250,year,dist_250m)%>%
  mutate(group=1)

tttt <- t%>%
  subset(dist_250m_500m == '1')

t4<- tttt%>%
  group_by( year)%>%
  mutate(mu_250_500=mean(price_real))%>%
  distinct(year,.keep_all = TRUE)%>%
  select(mu_250_500,year,dist_250m_500m)%>%
  mutate(group=2)


t5 <- t%>%
  subset(dist_500m_1000m == '1')

t5<- t5%>%
  group_by( year)%>%
  mutate(mu_500_1000=mean(price_real))%>%
  distinct(year,.keep_all = TRUE)%>%
  select(mu_500_1000,year,dist_500m_1000m)


t6 <- t%>%
  subset(dist_1000m_2000m == '1')

t6<- t6%>%
  group_by( year)%>%
  mutate(mu_1000_2000=mean(price_real))%>%
  distinct(year,.keep_all = TRUE)%>%
  select(mu_1000_2000,year,dist_1000m_2000m)


t7 <- t%>%
  subset(dist_2000m_plus == '1')

t7<- t7%>%
  group_by( year)%>%
  mutate(mu_2000_plus=mean(price_real))%>%
  distinct(year,.keep_all = TRUE)%>%
  select(mu_2000_plus,year,dist_2000m_plus)


df  <- t3%>%
  left_join(t4, by = "year")%>%
  left_join(t5, by = "year")%>%
  left_join(t6, by = "year")%>%
  left_join(t7, by = "year")
  
  

  


ggplot(data = df, aes(x = year)) +
  geom_line(aes(y = mu_250, colour = "Dist_0-250m")) +
  geom_point(aes(y = mu_250, colour = "Dist_0-250m")) +
  geom_line(aes(y = mu_250_500, colour = "Dist_250m-500m")) +
  geom_point(aes(y = mu_250_500, colour = "Dist_250m-500m")) +
  geom_line(aes(y = mu_500_1000, colour = "Dist_500m-1000m")) +
  geom_point(aes(y = mu_500_1000, colour = "Dist_500m-1000m")) +
  geom_line(aes(y = mu_1000_2000, colour = "Dist_1000m-2000m")) +
  geom_point(aes(y = mu_1000_2000, colour = "Dist_1000m-2000m")) +
  geom_line(aes(y = mu_2000_plus, colour = "Dist_2000m_plus")) +
  geom_point(aes(y = mu_2000_plus, colour = "Dist_2000m_plus")) +
  scale_colour_manual("", 
                      breaks = c("Dist_0-250m", "Dist_0-250m", "Dist_250m-500m", 'Dist_250m-500m',
                                 "Dist_500m-1000m","Dist_500m-1000m", "Dist_1000m-2000m", "Dist_1000m-2000m",
                                 "Dist_2000m_plus", "Dist_2000m_plus"),
                      values = c("blue", "blue","maroon", "maroon", "lightgreen", "lightgreen", "orange", "darkgreen")) +
  xlab(" ") +
  scale_y_continuous("Sale Price ($ in 2021)") +
  scale_x_continuous("Year", n.breaks = 19) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

  
#################################################################################
# SUmmary of structural attributes 

summary <- df_main %>%
  select(price_nominal, lotsize, log_area_tot, prop_type, pool, air_cond, heat_elect, 
  heat_forcedair, heat_hw, floodp_d, floodp_r, fireplcs, baths, bedrooms, 
  storeys, quality, lbasement_area, age, view_pr, view_gd, view_wf, wtr_l_r, 
  wtr_well, wtr_mun, wfront_l, wfront_r, ab_ind, ab_comm, ab_inst, ab_educ, 
  ab_golf, log_dist_toronto, log_dist_cma) %>% # select variables to summarise
  rename("pricenominal" = "price_nominal",
         "logareatot" = "log_area_tot",
         "proptype" = "prop_type",
         "aircond" = "air_cond",
         "heatelect" = "heat_elect",
         "heatforcedair" = "heat_forcedair",
         "heathw" = "heat_hw",
         "floodpd" = "floodp_d",
         "floodpr" = "floodp_r",
         "lbasementarea" = "lbasement_area",
         "viewpr" = "view_pr",
         "viewgd" = "view_gd",
         "viewwf" = "view_wf",
         "wtrlr" = "wtr_l_r",
         "wtrwell" = "wtr_well",
         "wtrmun" = "wtr_mun",
         "wfrontl" = "wfront_l",
         "wfrontr" = "wfront_r",
         "abind" = "ab_ind",
         "abcomm" = "ab_comm",
         "abinst" = "ab_inst",
         "abeduc" = "ab_educ",
         "abgolf" = "ab_golf",
         "logdisttoronto" = "log_dist_toronto",
         "logdistcma" = "log_dist_cma")%>%
  summarise_each(funs(mean = mean(., na.rm = T),
                      sd = sd(., na.rm = T),
                      median = median(., na.rm = T),
                      min = min(., na.rm = T), 
                      max = max(., na.rm = T)
  ))

df.stats.tidy <- summary %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd, median, min, max) # reorder columns

xtable(df.stats.tidy, digits = 4 )


datasummary_df(df.stats.tidy, output = "./results/summary_table_appendix.docx")

print.xtable(xtable(df.stats.tidy), digits = 2, file = "./results/summary_structural.tex")


#################################################################

# boxplot

df1 <- df_main%>%
  select(price_real)%>%
  rename("Price" = "price_real")%>%
  mutate(group = "Transaction Data")

df2 <- census_on_da_sa_bfp_count_filter_250m%>%
  select(v_Avod_)%>%
  rename("Price" = "v_Avod_")%>%
  mutate(group = "Census Data")%>%
  as.data.frame()%>%
  select(-geometry)

df <- rbind(df1,df2)

boxplot(df_main$price_real, census_on_da_sa_bfp_count_filter_250m$v_Avod_)

library(viridis)
library(hrbrthemes)


df %>%
  ggplot( aes(x=group, y=Price, fill=group)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("")

df %>%
  ggplot( aes(x=group, y=Price, fill=group)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

min(census_on_da_sa_bfp_count_filter_250m$v_Avod_)

max(census_on_da_sa_bfp_count_filter_250m$v_Avod_)

summarise(census_on_da_sa_bfp_count_filter_250m$v_Avod_)

summary(census_on_da_sa_bfp_count_filter_250m$v_Avod_)

##################################################################################
# Lake area

df <- df_main%>%
  select(lake_index_canvec,lake_area_sqkm)

df <- df%>%
  distinct(lake_index_canvec, .keep_all = T)

median(df$lake_area_sqkm)

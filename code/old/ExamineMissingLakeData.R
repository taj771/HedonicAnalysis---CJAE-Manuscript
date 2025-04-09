# re-estimate distance to gta variable
# check out palgrave sd_dist, Innis lake. Need correct 

rm(list=ls(all=TRUE))

library(tidyverse)
library(janitor)

df = readRDS("data/analysis_sample.rds")

df_house_latlon =  read_csv("data/HouseData/df_house_coordinates_final.csv")

df = df  %>%
  left_join(df_house_latlon)

df_canvec_lake_index = readRDS("data/df_canvec_lake_index.rds")

tt = df %>%
  filter(subset == "lake-lake") %>%
  mutate(close = ifelse(dist_house_lake <= 1000, 1, 0),
         sd_missing = ifelse(is.na(sd_average), "missing", "notmissing")) %>%
  group_by(lake_index_canvec,lake_name_canvec, lake_latlon_canvec, #year, #subset, 
           close, sd_missing) %>%
  summarise(homes= n()) %>%
  pivot_wider(names_from = "sd_missing", values_from = homes) 







# Cleans environment
rm(list=ls(all=TRUE))

# Install packages

#library(foreign)
#library(rio)
#library(tidyverse)
#library(lubridate)
#library(geosphere)
#library(data.table)
#library(spatialrisk)
#library(stargazer)
#library(car)
#library(jtools)
#library(ggstance)
#library(broom.mixed)
#library(margins)
#library(furrr)

library(tidyverse)
library(lubridate)
library(spatialrisk)
library(janitor)
library(parzer)
library(furrr)
library(haven)

##Import data
##I use dta file (in STATA) to open the full data set and choose the variables based on our model demand.
##I extract the data set from the dta file and convert it into .csv format in STATA 

df_house_stata = read_dta("data/Rural res sales - 2002-16.dta")

#df_house = as_tibble(df_house)

#save(df_house, "data/RuralResSales2002_2016.Rdata")
df_house <-read.csv("data for codes/add.csv", header = T)

## Import Housing price index (HPI) data (Base year 2016)
data_hpi <-read_csv("data/HPI_data.CSv")

df_sd = read_csv("data/df_wq_sd.csv")
df_tp = read_csv("data/df_wq_tp.csv")
df_rivers = read_csv("data/df_wq_rivers.csv")

##Subset
##I took the main variables for the model
df_house<-subset(df_house, select = c("latitude","longitude", "date", "sold_year", "sale_amt", "yr_blt","yrblteff","quality",
                                      "storeys","bsmtarea","fireplcs","baths","bedrooms","lsize","larea","air","heat","lgar_area",
                                      "lpool_area","age","leffage","wfront_l","pooliara","pooloara",
                                      "wfront_r","wfront_c","waterfront","waterfront3","ab_ind","ab_comm",
                                      "ab_inst","ab_educ","ab_golf","city","lcity","county","township"))


df_house = df_house %>%
  rename(year = sold_year) %>%
  filter(!is.na(latitude), 
         !is.na(longitude)) %>%
#  filter(lsize != 0,
#         larea != 0)
  mutate(id = 1:n()) 


##In HPI_data, I converted Total(total HPI) into HPI by dividing Total with 100
## Convert date: "%m/%d/%Y" to "%Y/%m"
data_hpi = data_hpi %>%
  mutate(date = as.Date(Date, format="%m/%d/%Y")) %>%
  select(date, HPI)



df_house2 = df_house %>%
  #  select(date, sold_year) %>%
  mutate(date = as_date(parse_date_time(date, "ym"))) %>%
  left_join(data_hpi, by = "date") %>%
  mutate(price_real = sale_amt * HPI / 100)


## Calculates average WQ within certain threshold as well as closest WQ station by year
#--------------------------------------------------------------------------------------
##Import WQ data

# Function to calculate WQ variables

#df_house_data = df_house_subset[[1]][1,]
#df_wq_data = df_sd


CalcWQVariables <- function(df_house_data, # Single row of housing data
                            df_wq_data, # Wq data
                            dist_metres) # distance thresholds for radius
{
  df_house_data = split(df_house_data, df_house_data$id)

  out = map_dfr(df_house_data, CalcWQVariables2, 
                 df_wq_data = df_wq_data, 
                 dist_metres = dist_metres*10)
  return(out)
}


CalcWQVariables2 <- function(df_house_data, # Single row of housing data
                            df_wq_data, # Wq data
                            dist_metres) # distance thresholds for radius
{
  # Filter out WQ data for particular year
  df_wq_data <- df_wq_data[df_wq_data$year == df_house_data$year, ]
  # Calculate distance between house and all stations within year
  points <- points_in_circle(df_wq_data, 
                             lon_center = df_house_data$longitude, 
                             lat_center = df_house_data$latitude, 
                             radius = dist_metres) # metres (needs to be large to ensure at least one station is captured)
  
  # Return closest station
  closest <- points[which.min(points$distance_m),]
  
  # Return all stations within threshold distance
  points <- points[points$distance_m < dist_metres,]
  
  if(nrow(points) == 0){
    out <- df_house_data %>%
      mutate(
        # take average of WQ within threshold
        #  average_wq = mean(points$wq_mean),
        # How many stations average based on
        #  n_stations = nrow(points),
        # Return closest WQ station and data
        distance_m = NA,
        close_stn = NA,
        #  close_site_id = closest$site_id,
        close_wq = NA)#,
    #      closeSecchiMax = closest$sd_max,
    #      closeSecchiMin = closest$sd_min
    #)
    
  } else{
  out <- df_house_data %>%
    mutate(
      # take average of WQ within threshold
    #  average_wq = mean(points$wq_mean),
      # How many stations average based on
    #  n_stations = nrow(points),
      # Return closest WQ station and data
      distance_m = closest$distance_m,
      close_stn = closest$stn,
    #  close_site_id = closest$site_id,
      close_wq = closest$wq_mean)#,
  #      closeSecchiMax = closest$sd_max,
  #      closeSecchiMin = closest$sd_min
  #)
  }
  return(out)
}


# Distance threshold to use (in metres)
dist_metres <- 5000

df_house_subset = df_house2 %>%
  select(id, year, latitude, longitude)

# load data

# Split housing data into lists for use in purrr:map
df_house_subset <- split(df_house_subset, (seq(nrow(df_house_subset))-1) %/% 5000) #split(df_house_subset, df_house_subset$year)

plan(multiprocess, workers = 6)

#df_house_subset = df_house_subset[1:31]

df_sd_link <- future_map_dfr(df_house_subset, CalcWQVariables, 
                             df_wq_data = df_sd, 
                             dist_metres = dist_metres,
                             .options = furrr_options(seed=TRUE))

df_tp_link <- future_map_dfr(df_house_subset, CalcWQVariables, 
                             df_wq_data = df_tp, 
                             dist_metres = dist_metres,
                             .options = furrr_options(seed=TRUE))


df_tp_river = df_rivers %>%
  filter(parm == "PPUT")
#df_tp_link_river <- future_map_dfr(df_house_subset, CalcWQVariables, 
#                             df_wq_data = df_tp_river, 
#                             dist_metres = dist_metres,
#                             .options = furrr_options(seed=TRUE))

closeAllConnections()
df_sd_link = df_sd_link %>%
  rename(sd = close_wq,
         sd_stn = close_stn,
         sd_distance_m = distance_m)

df_tp_link = df_tp_link %>%
  rename(tp = close_wq,
         tp_stn = close_stn,
         tp_distance_m = distance_m)

#df_tp_link_river = df_tp_link_river %>%
#  rename(tp_river = close_wq,
#         tp_river_stn = close_stn,
#         tp_river_distance_m = distance_m)

## join house characteristics and closest wq reading
df_house_wq = left_join(df_house2, df_sd_link) %>%
  left_join(df_tp_link) #%>%
 # left_join(df_tp_link_river)

df_house_wq = df_house_wq %>%
  mutate(renovation_effect = year - yrblteff,
         total_pool = pooliara +pooloara,
         pool = ifelse(total_pool > 0 , 1 ,0))

write_csv(df_house_wq, "data/df_house_wq.csv")




# Cleans environment
rm(list=ls(all=TRUE))

# Install packages
install.packages("foreign")
install.packages("tidyverse")
install.packages("rio")
install.packages("lubridate")
install.packages("geosphere")
install.packages("plyr")
install.packages("data.table")
install.packages("dplyr")
install.packages("spatialrisk")
install.packages("car")
install.packages("jtools")
install.packages("ggstance")
install.packages("broom.mixed")
install.packages("margins")
install.packages("stargazer")
install.packages("tidyr")

library(foreign)
library(rio)
library(tidyverse)
library(lubridate)
library(geosphere)
library(plyr)
library(data.table)
library(dplyr)
library(spatialrisk)
library(stargazer)
library(car)
library(jtools)
library(ggstance)
library(broom.mixed)
library(margins)
library(tidyr)

# Import house sale data (.sav file)

house_1 <-read.spss("data/Rural res sales for Rashed.sav",
                    to.data.frame = TRUE)

## Import Housing price index (HPI) data (Base year 2016)

data_HPI <-read.csv("data/HPI_data.CSv", header = T)

##In HPI_data, I converted Total(total HPI) into HPI by dividing Total with 100

## Convert date: "%m/%d/%Y" to "%Y/%m"
data_HPI$date <- format(as.Date(data_HPI$Date, format="%m/%d/%Y"),"%Y/%m")

#Import sale data with specifice date(sale year and month) & year
house_data_date <-read.csv("data/new sale data with date.CSv", header = T)

## Adjust house price with HPI
##----------------------------

## subset of house data
h<-subset(house_data_date, select = c("date","Year","latitude","longitude","sale_amt","lotsize","quality","condtion",
                                      "yr_blt","storeys","area_tot","areau","bsmtarea","fireplcs","baths","bedrooms",
                                      "heattype","aircond","pooloara","pooliara","gardeta","garatta","ab_ind","ab_comm",
                                      "ab_inst","ab_educ","ab_golf","wfront_l","wfront_r","wfront_s"))

## create ID for house data
ID <- rownames(h)
h <- cbind(ID=ID, h)

i<-subset(data_HPI, select = c("date", "HPI"))
export(i, "i.csv")
i <-read.csv("data/i_new.CSv", header = T)
marged <- left_join(h, i)
## check missing data
sum(is.na(marged$HPI))

##Multiply HPI and house price and divide by 100 to get real house price
marged$Real_price <- (marged$HPI * marged$sale_amt)/ 100

## check missing data for latitude & longitude
sum(is.na(marged$latitude))
sum(is.na(marged$longitude))

## Omit N/A data= only select rows with data in all columns
clean_marged <- na.omit(marged)
sum(is.na(clean_marged))

colnames(clean_marged)[4] <- "Latitude"
colnames(clean_marged)[5] <- "Longitude"

## Water quality data
## Import TP data
data1 <-read.csv("data/Lake_Partners1.csv", header = T)

# Check is there any missing data (data1)
sum(is.na(data1))
sum(is.na(data1$Lake.Name))
sum(is.na(data1$STN))
sum(is.na(data1$Site.ID))
sum(is.na(data1$Latitude))
sum(is.na(data1$Longitude))
sum(is.na(data1$Date))
sum(is.na(data1$TP1))
sum(is.na(data1$TP2))

# Find missing data
which(is.na(data1))
which(is.na(data1$Lake.Name))
which(is.na(data1$STN))
which(is.na(data1$Site.ID))
which(is.na(data1$Latitude))
which(is.na(data1$Longitude))
which(is.na(data1$Date))
which(is.na(data1$TP1))
which(is.na(data1$TP2))

## Omit N/A data= only select rows with data in all columns
clean_data_set_1 <- na.omit(data1)
sum(is.na(clean_data_set_1))

## Subset from data1("Lake.Name", "STN","Site.ID", "Date","TP1","TP2")
data11<-subset(clean_data_set_1, select = c("Lake.Name", "STN","Site.ID", "Latitude", "Longitude", "Date", "TP1", "TP2"))

##Convert date into year("23 aug 2002" to "2002-05-23")
data11$yr<-as.Date(data11$Date, format="%d-%b-%y")

##Convert date into only year("2002-05-23" to "2002")
data11$Year<-year(data11$yr)

##Finding the mean value of TP1 & TP2 for a single year
#First, find the mean value from TP1 & TP2>> (TP1+TP2)/2
data11$meanTP <- rowMeans(data11[c('TP1', 'TP2')], na.rm=TRUE)

#Find out the yearly mean value
#add all mean TP from different months of a same year and divided it by the number of months
data11_yearly_mean_TP <- ddply(data11,c("Lake.Name", "STN","Site.ID","Latitude", "Longitude", "Year"),
                               summarise, meanTP_yearly=mean(meanTP)) 

## Change the DMS lat and long data into decimal for Data1
#Latitude
dms_data11_yearly_mean_TP<-transform(data11_yearly_mean_TP, d = substr(Latitude,1,2),m = substr(Latitude,3,4),s = substr(Latitude,5,6))

#Longitude
dms_data_set_1_TP_<-transform(dms_data11_yearly_mean_TP, d_long = substr(Longitude,1,2),m_long = substr(Longitude,3,4),s_long = substr(Longitude,5,6))

## Export r file as .csv

export(dms_data_set_1_TP_, "dms_data_set_1_TP_.csv")

##Final conversion DMS to decimal for data1
#after exporting the  "dms_data_set_1" in csv format, I use -> d+m/60+s/3600 to get the latitude data and d_long+m_long/60+s_long/3600 >>
#take -(d_long+m_long/60+s_long/3600) as longitude data and saved the file as "Final_clean_data1". Then import the data

Final_data_set_1_TP <-read.csv("data/dms_data_set_1_TP_.csv", header = T)

## Import lake data_Secchi_depth (Lake_2)
data2 <-read.csv("data/Lake_Partners2.csv", header = T)

# Check is there any missing data for data2
sum(is.na(data2))
sum(is.na(data2$Lake.Name))
sum(is.na(data2$STN))
sum(is.na(data2$Site.ID))
sum(is.na(data2$Latitude))
sum(is.na(data2$Longitude))
sum(is.na(data2$Year))
sum(is.na(data2$Secchi.depth_avg))

# Find missing data
which(is.na(data2))
which(is.na(data2$Lake.Name))
which(is.na(data2$STN))
which(is.na(data2$Site.ID))
which(is.na(data2$Latitude))
which(is.na(data2$Longitude))
which(is.na(data2$Year))
which(is.na(data2$Secchi.depth_avg))

## Omit N/A data= only select rows with data in all columns
clean_data_2 <- na.omit(data2)
sum(is.na(clean_data_2))

## Subset from data2("Lake.Name", "STN","Site.ID", "Year","Secchi.depth_avg")
data22<-subset(clean_data_2,select = c("Lake.Name", "STN","Site.ID","Latitude", "Longitude", "Year","Secchi.depth_avg"))

## Change the DMS lat and long data into decimal for Data2
#Latitude
dms_data22<-transform(data22, d = substr(Latitude,1,2),m = substr(Latitude,3,4),s = substr(Latitude,5,6))

#Longitude
dms_data_set_2_SD_<-transform(dms_data22, d_long = substr(Longitude,1,2),m_long = substr(Longitude,3,4),s_long = substr(Longitude,5,6))

## Export r file as .csv
export(dms_data_set_2_SD_, "dms_data_set_2_SD_.csv")

##Final conversion DMS to decimal for data2
#after exporting the  "dms_data22_data2" in csv format, I use -> d+m/60+s/3600 to get the latitude data and d_long+m_long/60+s_long/3600 >>
#take -(d_long+m_long/60+s_long/3600) as longitude data and saved the file as "Final_clean_data2". Then import the data
Final_data_set_2_SD <-read.csv("data/dms_data_set_2_SD_.csv", header = T)

##Final managed data from data1 (Final_data_set_1_TP) and data2 (Final_data_set_2_SD)
## merges data > Final_data_set_marged (Final_clean_data_set_1_TP & Final_clean_data_set_2_SD based on "Lake.Name", "STN", "Site.ID", "Latitude", "Longitude", "Year")
Final_wq_data <- left_join(Final_data_set_1_TP, Final_data_set_2_SD)

## Omit N/A data= only select rows with complete data in all columns
sum(is.na(Final_wq_data))
Final_wq_data_omit <- na.omit(Final_wq_data)

# Unique lakes
unique_lake_new <- distinct(Final_wq_data_omit, Lake.Name, .keep_all= TRUE)

## Calculates average WQ within certain threshold as well as closest WQ station by year
#--------------------------------------------------------------------------------------
# Function to calculate WQ variables

CalcWQVariables <- function(df_wq, # Wq data
                            df_house, # Single row of housing data
                            dist_metres) # distance thresholds for radius
{
  
  # Filter out WQ data for particular year
  df_wq <- df_wq[df_wq$Year == df_house$Year, ]
  
  # Calculate distance between house and all stations within year
  points <- points_in_circle(df_wq, df_house$Longitude, df_house$Latitude, radius = 2000000000) # metres
  
  # Return closest station
  closest <- points[which.min(points$distance_m),]
  
  # Return all stations within threshold distance
  points <- points[points$distance_m < dist_metres,]
  
  out <- df_house %>%
    mutate(
      # take average of WQ within threshold
      meanTP = mean(points$meanTP_yearly),
      meanSecchi = mean(points$Secchi.depth_avg),
      # How many stations average based on
      n_stations = nrow(points),
      # Return closest WQ station and data
      distance_m = closest$distance_m,
      closeTP = closest$meanTP_yearly,
      closeSecchi = closest$Secchi.depth_avg)
  
  return(out)
}

# Distance threshold to use (in metres)
dist_metres <- 3000

# load data
v_lake <- Final_wq_data_omit
w_house <- subset(clean_marged, select = c("ID", "date","Year","Latitude","Longitude","sale_amt", "Real_price"))

df_wq <- v_lake %>%
  select(Year, Latitude, Longitude, meanTP_yearly, Secchi.depth_avg) 

colnames(df_wq)[2] <- "lat"
colnames(df_wq)[3] <- "lon"
summary(df_wq)

# Split housing data into lists for use in purrr:map
df_house <- split(w_house, w_house$ID)

df_wq_link <- map_dfr(df_house, CalcWQVariables, 
                      df_wq = df_wq, 
                      dist_metres = dist_metres)

## join house characteristics and closest wq reading
HP_WQ <- left_join(clean_marged, df_wq_link)

##Change the column nameS
colnames(HP_WQ)[10] <- "Built_year"
colnames(HP_WQ)[12] <- "area"
colnames(HP_WQ)[13] <- "uppar_floor_area"
colnames(HP_WQ)[3] <- "sold_year"
colnames(HP_WQ)[38] <- "TP"
colnames(HP_WQ)[39] <- "SD"

df <- HP_WQ

# Take log for Real_price, distance_m, lotsize, area
df1<- subset(df,select = c(Real_price, distance_m, lotsize, area))
l_Real_price <- log(df1[,c(1)])
l_distance_m <- log(df1[,c(2)])
l_lotsize <- log(df1[,c(3)])
l_area <- log(df1[,c(4)])

##combine all the log variables
df2 <- data.frame(l_Real_price)
df3 <- data.frame(l_distance_m)
df4 <- data.frame(l_lotsize)
df5 <- data.frame(l_area)
df <- cbind(df, df2, df3, df4, df5)

## Find zero values for different variables
sum(df$area==0)
sum(df$Built_year==0)
sum(df$lotsize==0)
sum(df$quality==0)
sum(df$storeys==0)
sum(df$aircond==0)

which(df$area==0)
which(df$Built_year==0)
which(df$storeys==0)

## Remove rows which contain zero for area, storeys and built year
aa<-df[!(df$Built_year=="0"),]
ab<-aa[!(aa$lotsize=="0"),]
ac<-ab[!(ab$storeys=="0"),]

sum(ac$area==0)
sum(ac$Built_year==0)
sum(ac$lotsize==0)
sum(ac$quality==0)
sum(ac$storeys==0)

##Convert Aircondition (aircond) Y/N to 1/0
ac$aircond <-ifelse(ac$aircond=="Y",1,0)

##remove bedroom above 16
sum(ac$bedrooms >16)
which(ac$bedrooms >16)
##delete the row where bedrooms=85
ac = filter(ac, bedrooms != "85")

##combined waterfront (lake, river, swamp)
ac <- ac %>% mutate(wf = rowSums(.[29:31]))

##add detached and attached total garage area
ac <- ac %>% mutate(garage = rowSums(.[22:23]))
##log value of garage
l_garage <- log(ac$garage+1)
df6 <- data.frame(l_garage)
ac <- cbind(ac, df6)

##add indoor and outdoor pool area
ac <- ac %>% mutate(pool_area = rowSums(.[20:21]))
##log value of pool
l_pool <- log(ac$pool_area+1)
df7 <- data.frame(l_pool)
ac <- cbind(ac, df7)

##log of SD
l_SD <- log(ac$SD)
df8 <- data.frame(l_SD)
ac <- cbind(ac, df8)

df<-ac

#create dummy variable for different years (14 dummies for 15 years)
df$year_dummy_2002<- ifelse(df$sold_year==2002,1,0)
df$year_dummy_2003<- ifelse(df$sold_year==2003,1,0)
df$year_dummy_2004<- ifelse(df$sold_year==2004,1,0)
df$year_dummy_2005<- ifelse(df$sold_year==2005,1,0)
df$year_dummy_2006<- ifelse(df$sold_year==2006,1,0)
df$year_dummy_2007<- ifelse(df$sold_year==2007,1,0)
df$year_dummy_2008<- ifelse(df$sold_year==2008,1,0)
df$year_dummy_2009<- ifelse(df$sold_year==2009,1,0)
df$year_dummy_2010<- ifelse(df$sold_year==2010,1,0)
df$year_dummy_2011<- ifelse(df$sold_year==2011,1,0)
df$year_dummy_2012<- ifelse(df$sold_year==2012,1,0)
df$year_dummy_2013<- ifelse(df$sold_year==2013,1,0)
df$year_dummy_2014<- ifelse(df$sold_year==2014,1,0)
df$year_dummy_2015<- ifelse(df$sold_year==2015,1,0)

#create dummy variable for lake distance
df$lake_dist_dummy_1km<- ifelse(df$distance_m <=1000,1,0)
df$lake_dist_dummy_2km<- ifelse(df$distance_m > 1000 & df$distance_m <=2000,1,0)
df$lake_dist_dummy_3km<- ifelse(df$distance_m > 2000 & df$distance_m <=3000,1,0)
df$lake_dist_dummy_4km<- ifelse(df$distance_m > 3000 & df$distance_m <=4000,1,0)
df$lake_dist_dummy_5km<- ifelse(df$distance_m > 4000 & df$distance_m <=5000,1,0)

# create dummy for TP
df$tp_dummy_1 <- ifelse(df$TP >= 10 & df$TP <=20,1,0)

df$tp_dummy_10 <- ifelse(df$TP <= 10,1,0)
df$tp_dummy_20 <- ifelse(df$TP > 10 & df$TP <=20,1,0)
df$tp_dummy_30 <- ifelse(df$TP > 20 & df$TP <=30,1,0)
df$tp_dummy_40 <- ifelse(df$TP > 30 & df$TP <=40,1,0)
df$tp_dummy_50 <- ifelse(df$TP > 40 & df$TP <=50,1,0)
df$tp_dummy_above_50 <- ifelse(df$TP > 50,1,0)

##---------------------------
##experiment tp dummy

df$tp_dummy_1 <- ifelse(df$TP >= 1 & df$TP <=20,1,0)

df$tp_dummy_20_1 <- ifelse(df$TP > 1 & df$TP <=20,1,0)
df$tp_dummy_40 <- ifelse(df$TP > 20 & df$TP <=40,1,0)
df$tp_dummy_60 <- ifelse(df$TP > 40 & df$TP <=60,1,0)
df$tp_dummy_80 <- ifelse(df$TP > 60 & df$TP <=80,1,0)
df$tp_dummy_100 <- ifelse(df$TP > 80 & df$TP <=100,1,0)
df$tp_dummy_above_100 <- ifelse(df$TP > 100,1,0)

df$tp_dummy_10 <- ifelse(df$TP <= 10,1,0)
df$tp_dummy_20 <- ifelse(df$TP > 10 & df$TP <=20,1,0)
df$tp_dummy_40 <- ifelse(df$TP > 20 & df$TP <=40,1,0)
df$tp_dummy_60 <- ifelse(df$TP > 40 & df$TP <=60,1,0)
df$tp_dummy_80 <- ifelse(df$TP > 60 & df$TP <=80,1,0)
df$tp_dummy_100 <- ifelse(df$TP > 80 & df$TP <=100,1,0)
df$tp_dummy_above_100 <- ifelse(df$TP > 100,1,0)
##--------------------------------

## Interacted variable lake distance*SD
df$Distance_SD<- df$l_distance_m * df$SD

## Interacted variable lake distance*TP
df$Distance_TP<- df$l_distance_m * df$TP

## Interacted variable lake distance dummy*SD
df$lake_dist_dummy_1km_SD<- df$lake_dist_dummy_1km * df$SD
df$lake_dist_dummy_2km_SD<- df$lake_dist_dummy_2km * df$SD
df$lake_dist_dummy_3km_SD<- df$lake_dist_dummy_3km * df$SD
df$lake_dist_dummy_4km_SD<- df$lake_dist_dummy_4km * df$SD
df$lake_dist_dummy_5km_SD<- df$lake_dist_dummy_5km * df$SD

## find the data set within specific lake distance
## within 1km
df_1km<-df[df$distance_m <=1000,]
## within 2km
df_2km<-df[df$distance_m > 1000 & df$distance_m <=2000,]
## within 3km
df_3km<-df[df$distance_m > 2000 & df$distance_m <=3000,]
## within 4km
df_4km<-df[df$distance_m > 3000 & df$distance_m <=4000,]
## within 5km
df_5km<-df[df$distance_m > 4000 & df$distance_m <=5000,]
## above 5km
df_above_5km<-df[df$distance_m > 5000,]

## Number of houses within different distances
##within 5km
df_within_5km<-df[df$distance_m <= 5000,]
## within 8km
df_within_8km<-df[df$distance_m <= 8000,]
## within 10km
df_within_10km<-df[df$distance_m <= 10000,]
## within 15km
df_within_15km<-df[df$distance_m <= 15000,]

#create dummy variable for lake distance within 5km (df_within_5km)
df_within_5km$dummy_1km<- ifelse(df_within_5km$distance_m <=1000,1,0)
df_within_5km$dummy_2km<- ifelse(df_within_5km$distance_m > 1000 & df_within_5km$distance_m <=2000,1,0)
df_within_5km$dummy_3km<- ifelse(df_within_5km$distance_m > 2000 & df_within_5km$distance_m <=3000,1,0)
df_within_5km$dummy_4km<- ifelse(df_within_5km$distance_m > 3000 & df_within_5km$distance_m <=4000,1,0)

#create SD*lake distance dummy within 5km (df_within_5km)
## Interacted variable lake distance dummy*SD
df_within_5km$dummy_1km_SD<- df_within_5km$dummy_1km * df_within_5km$SD
df_within_5km$dummy_2km_SD<- df_within_5km$dummy_2km * df_within_5km$SD
df_within_5km$dummy_3km_SD<- df_within_5km$dummy_3km * df_within_5km$SD
df_within_5km$dummy_4km_SD<- df_within_5km$dummy_4km * df_within_5km$SD

export(df_within_5km, "df_within_5km.csv")


##Model estimation
## Model 1
#---------
xs <- c("SD", "TP")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_base_1 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_base_1, type = "text")


## Model 2
#---------
xs <- c("l_lotsize", "l_distance_m","Built_year", "SD", "TP", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "TP","storeys","quality", "ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf", "l_garage", "l_pool")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_base_2 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_base_2, type = "text")

## Model 3
#---------
xs <- c("l_lotsize", "l_distance_m","Built_year", "SD", "TP", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality", "ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_pool","l_garage", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_base_3 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_base_3, type = "text")
stargazer(reg_base_1,reg_base_2,reg_base_3, type = "text")


##Model 4: (l_Real_price) with TP dummy (tp_dummy) 
#------------------------------------------------------------
## Model 4
#---------------
xs <- c("l_lotsize", "l_distance_m","Built_year", "SD", "tp_dummy_1", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_4 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_4, type = "text")

## Model 5, Model 6, Model 7 (only single WQ variable)
#--------------------------------------------------------------------
## Model 5
#---------------
## Model 5:	Only SD as WQ variable

xs <- c("l_lotsize", "l_distance_m","Built_year", "SD", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms","storeys","quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_5 <- lm(as.formula(f), data=df)
# Regression results
stargazer(reg_5, type = "text")

## Model 6
#---------------
## Model 6:	Only TP as WQ variable
xs <- c("l_lotsize", "l_distance_m","Built_year", "TP", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms","storeys","quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_6 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_6, type = "text")

## Model 7
#---------------
## Model 7:	Only TP (dummy: 10-20 microgram per liter = 1, rest of the values = 0) as WQ variable
xs <- c("l_lotsize", "l_distance_m","Built_year", "tp_dummy_1", "bsmtarea", "fireplcs", 
        "baths", "bedrooms","storeys","area", "quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_7 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_5, reg_6, reg_7, type = "text")


## Alternative specific models
#-----------------------------

##model 8(SD*l_distance)
#-----------------------
xs <- c("l_lotsize", "Built_year", "SD", "l_distance_m", "Distance_SD", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool","year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_8 <- lm(as.formula(f), data=df)
stargazer(reg_8, type = "text")


##model 9 (SD*lake distance dummy)
#---------------------------------
xs <- c("l_lotsize", "lake_dist_dummy_1km","lake_dist_dummy_2km","lake_dist_dummy_3km","lake_dist_dummy_4km",
        "lake_dist_dummy_5km","lake_dist_dummy_1km_SD","lake_dist_dummy_2km_SD","lake_dist_dummy_3km_SD",
        "lake_dist_dummy_4km_SD","lake_dist_dummy_5km_SD","Built_year", "SD", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality", "ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_9 <- lm(as.formula(f), data=df)
stargazer(reg_9, type = "text")

stargazer(reg_8,reg_9, type = "text")

##model 10(SD*l_distance)(without year dummy)
#-----------------------
xs <- c("l_lotsize", "Built_year", "SD", "l_distance_m", "Distance_SD", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "TP","storeys","quality", "ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf", "l_garage","l_pool")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_10 <- lm(as.formula(f), data=df)
stargazer(reg_10, type = "text")

##model 11 (SD*lake distance dummy) (without year dummy)
#-----------------------------------------------
xs <- c("l_lotsize", "lake_dist_dummy_1km","lake_dist_dummy_2km","lake_dist_dummy_3km","lake_dist_dummy_4km",
        "lake_dist_dummy_5km","lake_dist_dummy_1km_SD","lake_dist_dummy_2km_SD","lake_dist_dummy_3km_SD",
        "lake_dist_dummy_4km_SD","lake_dist_dummy_5km_SD","Built_year", "SD", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality", "ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_11 <- lm(as.formula(f), data=df)
stargazer(reg_11, type = "text")

stargazer(reg_8,reg_9,reg_10,reg_11, type = "text")




## MWTP For lake distance=avg_l_dist
deltaMethod(reg_8, "(SD + (Distance_SD*average_l_distance_m))*average_price")

## Marginal implicit price of secchi depth
#-----------------------------------------
## Model 9: SD, SD*l_distance dummy

## MWTP for SD
#-------------

## Marginal implicit price of SD for different lake distance
#----------------------------------------------------------------------
average_price <- mean(df$Real_price)
deltaMethod(reg_9, "SD + lake_dist_dummy_1km_SD")
deltaMethod(reg_9, "SD + lake_dist_dummy_2km_SD")
deltaMethod(reg_9, "SD + lake_dist_dummy_3km_SD")
deltaMethod(reg_9, "SD + lake_dist_dummy_4km_SD")
deltaMethod(reg_9, "SD + lake_dist_dummy_5km_SD")

## MWTP for SD for different lake distance

##MWTP for SD
#----------

## For lake distance=1km
deltaMethod(reg_9, "(SD + lake_dist_dummy_1km_SD)*average_price")
## For lake distance=2km 
deltaMethod(reg_9, "(SD + lake_dist_dummy_2km_SD)*average_price")
## For lake distance=3km
deltaMethod(reg_9, "(SD + lake_dist_dummy_3km_SD)*average_price")
## For lake distance=4km 
deltaMethod(reg_9, "(SD + lake_dist_dummy_4km_SD)*average_price")
## For lake distance=5km 
deltaMethod(reg_9, "(SD + lake_dist_dummy_5km_SD)*average_price")
## For lake distance= Above 5km 
deltaMethod(reg_9, "(SD)*average_price")

## Plot of MWTP
#--------------
plot.data22 <- deltaMethod(reg_9, "(SD + lake_dist_dummy_1km_SD)*average_price") %>%
  mutate(MWTP="within 1km")
plot.data23 <- deltaMethod(reg_9, "(SD + lake_dist_dummy_2km_SD)*average_price") %>%
  mutate(MWTP="1km to 2km") 
plot.data24 <- deltaMethod(reg_9, "(SD + lake_dist_dummy_3km_SD)*average_price") %>%
  mutate(MWTP="2km to 3km") 
plot.data25 <- deltaMethod(reg_9, "(SD + lake_dist_dummy_4km_SD)*average_price") %>%
  mutate(MWTP="3km to 4km") 
plot.data26 <- deltaMethod(reg_9, "(SD + lake_dist_dummy_5km_SD)*average_price") %>%
  mutate(MWTP="4km to 5km")
plot.data27 <- deltaMethod(reg_9, "(SD)*average_price") %>%
  mutate(MWTP="Above 5km") 

z<- data.frame(rbind(plot.data22, plot.data23,plot.data24,plot.data25,plot.data26, plot.data27))

colnames(z)[3] <- "2.5 %"
colnames(z)[4] <- "97.5 %"

# Make a plot

level_order_1 <- c('within 1km','1km to 2km','2km to 3km','3km to 4km','4km to 5km','Above 5km')
ggplot(data = z, aes(x = factor(MWTP,level = level_order_1), y = `Estimate`, ymin = `2.5 %`, ymax = `97.5 %`)) + 
  geom_pointrange() +
  geom_errorbar()+
  geom_hline(yintercept = 1, lty = 2) +
  xlab("Distance from house to WQ station") + ylab("MWTP for SD with 95% Confidence Interval")   + # Labels
  theme_bw()  # Nicer theme


##--------------------------------------------------------
## Model-10 with observations within 5km lake distances
#---------------------------------------------------------
## Model-10: SD,l_Distance dummy, SD*l_distance dummy (only 3000 meter)

xs <- c("l_lotsize", "dummy_1km","dummy_2km","dummy_3km","dummy_4km",
        "dummy_1km_SD","dummy_2km_SD","dummy_3km_SD",
        "dummy_4km_SD","Built_year", "SD", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality", "ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_10 <- lm(as.formula(f), data=df_within_5km)
stargazer(reg_model_10, type = "text")


##------------------------
##------------------------
##model with tp & tp dummy

## Model 20
#---------
xs <- c("l_lotsize", "l_distance_m","Built_year", "TP", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality", "ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_pool","l_garage", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_20 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_20, type = "text")

##Model 21: (l_Real_price) with TP dummy (tp_dummy) 
#------------------------------------------------------------
xs <- c("l_lotsize", "l_distance_m","Built_year", "tp_dummy", "bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_21 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_21, type = "text")

##Model 22: (l_Real_price) with TP dummy (multiple) 
#------------------------------------------------------------
xs <- c("l_lotsize", "l_distance_m","Built_year", "tp_dummy_10","tp_dummy_20","tp_dummy_30","tp_dummy_40",
        "tp_dummy_50","tp_dummy_above_50","bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_22 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_22, type = "text")
stargazer(reg_20,reg_21,reg_22, type = "text")

##Model 23: (l_Real_price) with TP dummy (multiple) 
#------------------------------------------------------------
xs <- c("l_lotsize", "l_distance_m","Built_year", "tp_dummy_10", "tp_dummy_20", "tp_dummy_30","tp_dummy_40","tp_dummy_50",
        "tp_dummy_above_50","tp_dummy_above_50","bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_23 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_23, type = "text")

##Model 24: (l_Real_price) with TP dummy (multiple) 
#------------------------------------------------------------
xs <- c("l_lotsize", "l_distance_m","Built_year", "tp_dummy_20", "tp_dummy_40","tp_dummy_60","tp_dummy_80",
        "tp_dummy_100","tp_dummy_above_100","tp_dummy_above_50","bsmtarea", "area", "fireplcs", 
        "baths", "bedrooms", "storeys","quality","ab_ind",
        "ab_comm","ab_inst","ab_educ","ab_golf","wf","l_garage","l_pool", "year_dummy_2002",
        "year_dummy_2003", "year_dummy_2004", "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", 
        "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", 
        "year_dummy_2013", "year_dummy_2014", "year_dummy_2015")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_24 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_24, type = "text")
stargazer(reg_22,reg_23,reg_24, type = "text")



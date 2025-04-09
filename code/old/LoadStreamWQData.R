


# Cleans environment
rm(list=ls(all=TRUE))


library(tidyverse)
library(lubridate)
library(spatialrisk)
library(janitor)
library(parzer)
library(readxl)
## Water quality data
## Import lake data_Secchi_depth (Lake_2)

xl_data <- "data/wq_data/pwqmn_rawdata_2000_2014/pwqmn_rawdata_2003.xlsx"

file.list <- list.files(path = "data/wq_data/pwqmn_rawdata_2000_2014/",
                        pattern='*.xlsx')
file.list = paste0("data/wq_data/pwqmn_rawdata_2000_2014/",file.list)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


df <- lapply(file.list, read_excel_allsheets)

df <- map(df, bind_rows)
df <- map(df, function(x){
  x = as_tibble(x)
  x = clean_names(x) %>%
    mutate(time = as.character(time),
        field_no = as.character(field_no))
  
  found <- match(colnames(x), "station_no", nomatch = 0)
  colnames(x)[colnames(x) %in% "station_no"] <- "station"
  

  return(x)
})

df <- map_dfr(df, rbind)

summary(df$station)
df = df %>%
  select(station, parm, parm_description, date, result, units)

df_coordinates = read_excel("data/wq_data/PWQMN1.xlsx")

df_coordinates = clean_names(df_coordinates)

df_merge = df %>%
  left_join(df_coordinates) %>%
  filter(!is.na(latitude))


#biological oxygen demand (BOD), 
#dissolved oxygen (DO), 
#fecal coliform bacteria (FCB), 
#total suspended solids (TSS), 
#nitrogen (NO3), and
#phosphorous (PO4).

df2 = df_merge %>%
  filter(str_detect(parm_description, "FECAL"))

keep_parms = c("PPUT", # Phospohorus
               "BOD5",
               "DO",
               "NNOTUR", # nitrates
               "NNTKUR", #total nitrogen
               "TURB",
               "FSMF")

df2 = df_merge %>%
  filter(parm %in% keep_parms) %>%
  mutate(year = year(date)) %>%
  group_by(station, year, parm, latitude, longitude) %>%
  summarise(wq_mean = mean(result),
            num_readings = n()) %>%
  rename(lat = latitude,
         lon = longitude,
         stn =station) 

write_csv(df2, "data/df_wq_rivers.csv")

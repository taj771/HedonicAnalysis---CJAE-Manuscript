

df_muskoka_pdfs = read_csv("data/wq_data/wq_sd_muskoka_files.csv")

df_muskoka_pdfs = df_muskoka_pdfs %>%
  mutate(sd_may = as.numeric(sd_may),
         sd_aug = as.numeric(sd_aug),
         sd_average = as.numeric(sd_average),
         n_readings = 2 - is.na(sd_may) - is.na(sd_aug)) %>%
  distinct(lake_name, year, sd_average, n_readings) %>%
  arrange(lake_name, year) %>%
  mutate(lake_name_full = lake_name,
         lake_name = tolower(lake_name))


df_muskoka_pdfs_distinct = df_muskoka_pdfs %>%
  distinct(lake_name_full) %>%
  rename(lake_name = lake_name_full) %>%
#  filter(row_number() == 42)
  mutate(lake_name_old = lake_name,
         lake_name = ifelse(lake_name == "Pine", "Pine (BR)", lake_name),
         lake_name = ifelse(lake_name == "Go-Home" | lake_name == "Go Home", "Go Home Lake", lake_name),
         lake_name = ifelse(lake_name == "Clear", "Clear (Torrance)", lake_name),
 #        lake_name = str_replace(lake_name, "Clear (ML)", "Clear (Torrance)"),
         lake_name = str_replace(lake_name, "(HTE)|HTE|(HT)", "Huntsville"),
         lake_name = str_replace(lake_name, "(GR)", "Gravenhurst"),
         lake_name = str_replace(lake_name, "(ML)", "Muskoka Lakes"),
         lake_name = str_replace(lake_name, "(BB)|(BR)", "Bracebridge"),
         lake_name = str_replace(lake_name, "LOB", "Lake of Bays"),
         lake_name = str_replace(lake_name, "NMRB", "North Muskoka River Bay"),
         lake_name = str_replace(lake_name, "Three Mile (Muskoka Lakes) - Main", "Three Mile - Main")) %>%
  mutate(lake_name = ifelse(lake_name_old == "Clear (ML)", "Clear (Torrance)", lake_name),
         lake_name = ifelse(lake_name_old == "buck (ryde) lake", "ryde (buck) lake", lake_name),
         lake_name = ifelse(lake_name_old == "long (cardwell) lake", "cardwell (long) lake", lake_name),
         lake_name = ifelse(lake_name == "young lake (trout)", "young lake", lake_name)) %>%
  distinct()


df_muskoka_latlon = read_csv("data/wq_data/muskokalakecoordinates.csv")%>%
  filter(!is.na(lat_lon)) %>%
  filter(lake_name != "Buck (Ryde) Lake", 
         lake_name != "Long (Cardwell) Lake", 
         lake_name != "Rutter (Little Long) Lake")

df_muskoka_latlon_duplicates = df_muskoka_latlon %>% 
  group_by(lake_name) %>%
  filter(n()>1) %>%
  mutate(lake_name = paste0(lake_name," ", municipality))
  
df_muskoka_latlon = df_muskoka_latlon %>% 
  group_by(lake_name) %>%
  filter(n()==1) %>%
  bind_rows(df_muskoka_latlon_duplicates) 

df_join_muskoka_pdf = stringdist_join(df_muskoka_pdfs_distinct, df_muskoka_latlon, 
                by = "lake_name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(lake_name.x) %>%
  slice_min(order_by = dist, n = 1) %>%
  rename(lake_name = lake_name_old ) %>%
  distinct() %>%
  distinct(lake_name.x, lake_name, lat_lon, .keep_all = T)

df_muskoka_pdf_latlon = df_muskoka_pdfs %>%
  dplyr::select(lake_name_full, sd_average, year, n_readings) %>%
  rename(lake_name = lake_name_full) %>%
#  left_join(df_lake_distinct, by = "lake_name_old") %>%
  left_join(df_join_muskoka_pdf, by = "lake_name") %>%
  distinct() %>%
  dplyr::select(lake_name = lake_name.y.y, lake_name.x, lat_lon, year, sd_average, n_readings) %>%
  mutate(lake_name = ifelse(lake_name == "thinn (reay) lake", "reay (thinn) lake", lake_name),
         lake_name = ifelse(lake_name == "rutter (little long) lake", "little long (rutter) lake", lake_name),
         lake_name = ifelse(lake_name == "little lake joseph", "joseph - little lake joseph", lake_name),
         source = "muskoka_pdfs")


df_lpp = read_csv("data/df_wq_lpp_sd.csv")  %>%
  distinct(lake_name, district_county, stn, year, wq_mean, lon, lat, n_readings, source) %>%
  rename(sd_average = wq_mean) %>%
  arrange(lake_name, year) %>%
  mutate(lake_name = str_trim(lake_name)) %>%
  mutate(lake_name = tolower(lake_name),
         lat_lon = paste0(lat, " ", lon)) %>%
  dplyr::select(lake_name, district_county, stn, year, lat_lon, sd_average, n_readings, source)

# %>%
#   mutate(lat_lon = ifelse(lake_name == "st. george lake", "43.956520 -79.425164", lat_lon),
#          lat_lon = ifelse(lake_name == "green lake" & stn == 7331, "44.689027 -76.521154", lat_lon),
#          lat_lon = ifelse(lake_name == "south nelson lake", "45.470972 -78.956106", lat_lon),
#          lat_lon = ifelse(lake_name == "eyre lake (black)", "45.259775 -78.500832", lat_lon),
#          lat_lon = ifelse(lake_name == "little john lake", "44.514966 -76.631085", lat_lon),
#          lat_lon = ifelse(lake_name == "mccullough lake", "44.358409 -80.920326", lat_lon),
#          lat_lon = ifelse(lake_name == "perry lake" & stn == 4324, "45.546438 -79.227218", lat_lon),
#          lat_lon = ifelse(lake_name == "whitestone lake", "45.658039 -79.8727036", lat_lon),
#          lat_lon = ifelse(lake_name == "penyck lake (pennick)", "44.849165 -76.716043", lat_lon),
#          lat_lon = ifelse(lake_name == "clam lake (big clam)", "45.528820 -79.158032", lat_lon),
#          lat_lon = ifelse(lake_name == "little clam lake", "45.530894 -79.165446", lat_lon),
#          lat_lon = ifelse(lake_name == "horseshoe lake (elsie)", "44.988112 -78.678795", lat_lon),
#          lat_lon = ifelse(lake_name == "lipsy lake", "45.174261 -78.634954", lat_lon),
#          lat_lon = ifelse(lake_name == "fourteen island lake" & lat_lon == "44.4991683959961 -76.6594467163086", "44.494655 -76.655333", lat_lon),
#          lat_lon = ifelse(lake_name == "fourteen island lake" & lat_lon == "44.5055541992188 -76.6327743530273", "44.505074 -76.626735", lat_lon),
#          lat_lon = ifelse(lake_name == "spectacle lake", "45.528218 -77.860737", lat_lon),
#          lat_lon = ifelse(lake_name == "lower perieau lake", "45.261600 -78.137466", lat_lon),
#          lat_lon = ifelse(lake_name == "bells lake (silver)", "45.602678 -79.683055", lat_lon),
#          lat_lon = ifelse(lake_name == "big bald lake", "44.573353 -78.384640", lat_lon),
#          lat_lon = ifelse(lake_name == "big barnum lake", "45.028071 -78.531960", lat_lon),
#          lat_lon = ifelse(lake_name == "boulter lake", "45.307051 -78.048973", lat_lon),
#          lat_lon = ifelse(lake_name == "bowley lake", "45.173466 -76.252401", lat_lon),
#          lat_lon = ifelse(lake_name == "buckskin lake", "44.959659 -78.193543", lat_lon),
#          lat_lon = ifelse(lake_name == "cheer lake", "45.774690 -79.519516", lat_lon),
#          lat_lon = ifelse(lake_name == "little black lake", "45.253943 -78.512986", lat_lon),
#          lat_lon = ifelse(lake_name == "lohi lake", "46.387716 -81.043818", lat_lon),
#          lat_lon = ifelse(lake_name == "eaton lake (seneca)", "43.959746 -79.520336", lat_lon),
#          lat_lon = ifelse(lake_name == "elbow lake" & lat_lon == "44.4261093139648 -76.4286117553711", "44.475059 -76.429473", lat_lon),
#          lat_lon = ifelse(lake_name == "little cameron lake (third)", "45.122624 -78.653821", lat_lon),
#          lat_lon = ifelse(lake_name == "lovesick lake" & lat_lon == "44.5499992370605 -78.216667175293", "44.562869 -78.238231", lat_lon),
#          lat_lon = ifelse(lake_name == "little cameron lake (third)", "45.122624 -78.653821", lat_lon),
#          lat_lon = ifelse(lake_name == "moon river", "45.109837 -79.949341", lat_lon),
#          lat_lon = ifelse(lake_name == "nelson lake", "46.728404 -81.092885", lat_lon),
#          lat_lon = ifelse(lake_name == "nine mile lake" & lat_lon == "45.466667175293 -80.0666656494141", "45.444863 -80.045175", lat_lon),
#          lat_lon = ifelse(lake_name == "perbeth lake (fisher)", "45.543720 -79.206165", lat_lon),
#          lat_lon = ifelse(lake_name == "aird lake", "45.129947 -77.074838", lat_lon),
#          lat_lon = ifelse(lake_name == "peters lake", "45.542346079942284 -79.14642236524378", lat_lon),
#          lat_lon = ifelse(lake_name == "alfred lake", "45.676378 -79.008566", lat_lon),
#          lat_lon = ifelse(lake_name == "bear lake" & stn == 273, "48.123745 -79.639667", lat_lon),
#          lat_lon = ifelse(lake_name == "billings lake (wolf)", "44.932203 -78.366943", lat_lon),
#          lat_lon = ifelse(lake_name == "whitefish lake" & 
#                             lat_lon == "44.5338897705078 -76.2261123657227", "44.532972 -76.230115", lat_lon),
#          lat_lon = ifelse(lake_name == "wet lake", "45.8997230529785 -79.1350021362305", lat_lon),
#          lat_lon = ifelse(lake_name == "west lake" & stn == 7598, "45.310362 -75.997963", lat_lon),
#          lat_lon = ifelse(lake_name == "sunrise lake (lower butte)", "45.426812 -78.753637", lat_lon),
#          lat_lon = ifelse(lake_name == "skeleton lake" & stn == 7350, "47.864397 -79.651765", lat_lon),
#          lat_lon = ifelse(lake_name == "shawandasee lake", "45.435823 -78.784517", lat_lon),
#          lat_lon = ifelse(lake_name == "robinson lake" & stn == 4649, "46.455787 -81.031170", lat_lon),
#          lat_lon = ifelse(lake_name == "pike lake" & stn == 7106, "47.186237 -79.728205", lat_lon),
#          lat_lon = ifelse(lake_name == "pigeon river", "44.336263 -78.539941", lat_lon),
#          lat_lon = ifelse(lake_name == "pewee lake", "45.571435 -78.524938", lat_lon),
#          lat_lon = ifelse(lake_name == "ouse lake", "45.546885 -78.673617", lat_lon),
#          lat_lon = ifelse(lake_name == "murr lake", "48.144814 -81.787361", lat_lon),
#          lat_lon = ifelse(lake_name == "murdock lake", "45.297028 -79.972695", lat_lon),
#          lat_lon = ifelse(lake_name == "long turtle lake", "44.925246 -79.450544", lat_lon),
#          lat_lon = ifelse(lake_name == "farlain lake", "44.815391 -79.971281", lat_lon),
#          lat_lon = ifelse(lake_name == "long turtle lake", "44.925246 -79.450544", lat_lon),
#          lake_name = ifelse(lake_name == "young lake (trout)", "young lake", lake_name))



df_lpp_muskoka  = df_lpp %>%
  filter(str_detect(district_county,"MUSKOKA")) 

# Manually added from
# http://thamesriver.on.ca/wp-content/uploads/SurfaceWater/TRWaterQualityAssessment-NutrientSedimentSources-Report.pdf
# Using W1 on page 38
df_wildwood = tibble(year = 2005:2013,
                     sd_average = c(1.05, 1, 1.45,1.4, 1.2, 1.4, 1.05, 
                                    2, 1.75)) %>%
  mutate(lake_name = "wildwood lake",
         lat_lon = "43.2333335876465 -81.033332824707",
         source = "thames_river",
         n_readings = 1)


df_lpp_not_muskoka  = df_lpp %>%
  filter(!(stn %in% df_lpp_muskoka$stn)) %>%
  distinct(lake_name, year, lat_lon, sd_average, n_readings, source) %>%
  bind_rows(df_wildwood)

df_lpp_muskoka = df_lpp_muskoka %>%
  dplyr::select(lake_name, year, lat_lon, sd_average, n_readings, source) %>%
  mutate(lake_name = ifelse(lake_name == "buck lake", "buck lake huntsville &", lake_name),
         lake_name = ifelse(lake_name == "clearwater lake", "clearwater lake gravenhurst", lake_name),
         lake_name = ifelse(lake_name == "chub lake", "chub lake huntsville", lake_name),
         lake_name = ifelse(lake_name == "doe lake (doeskin)", "doe (doeskin) lake", lake_name),
         lake_name = ifelse(lake_name == "fairy lake", "fairy - main", lake_name),
         lake_name = ifelse(lake_name == "garter snake lake (long)", "gartersnake (long) lake", lake_name),
         lake_name = ifelse(lake_name == "gibson lake", "gibson - south", lake_name),
         lake_name = ifelse(lake_name == "golden city lake (waterhouse)", "golden city lake", lake_name),
         lake_name = ifelse(lake_name == "jessop lake (jingo)", "jessop (jingo) lake", lake_name),
         lake_name = ifelse(lake_name == "kahshe lake", "kahshe - main", lake_name),
      lake_name = ifelse(lake_name == "lake of bays", "lake of bays - south portage bay", lake_name),
lake_name = ifelse(lake_name == "little long lake" | lake_name == "rutter (little long) lake", "little long (rutter) lake", lake_name),
lake_name = ifelse(lake_name == "pells lake", "pell lake", lake_name),
lake_name = ifelse(lake_name == "peninsula lake", "peninsula - east", lake_name),
lake_name = ifelse(lake_name == "pine lake" & (year == 2009 | year > 2011), "pine lake bracebridge", lake_name),
lake_name = ifelse(lake_name == "pine lake" & (year == 2010 | year == 2011), "pine lake gravenhurst", lake_name),
lake_name = ifelse(lake_name == "bass lake" & (year == 2004), "bass lake muskoka lakes", lake_name),
lake_name = ifelse(lake_name == "bass lake" & (year == 2003 | year == 2005), "bass lake gravenhurst", lake_name),
lake_name = ifelse(lake_name == "ripple lake (deep)", "ripple lake", lake_name),
lake_name = ifelse(lake_name == "silver lake", "silver lake gravenhurst", lake_name),
lake_name = ifelse(lake_name == "six mile lake", "six mile - main", lake_name),
lake_name = ifelse(lake_name == "spence lake", "spence - north", lake_name),
lake_name = ifelse(lake_name == "st. mary lake (paint)", "paint (st. mary) lake", lake_name),
lake_name = ifelse(lake_name == "three mile lake", "three mile - main", lake_name),
lake_name = ifelse(lake_name == "torrance lake (clear)", "clear (torrance) lake", lake_name),
lake_name = ifelse(lake_name == "waseosa lake", "waseosa", lake_name),
lake_name = ifelse(lake_name == "cardwell lake (long)", "cardwell (long) lake", lake_name),
lake_name = ifelse(lake_name == "ryde lake(buck)" | lake_name == "buck (ryde) lake", "ryde (buck) lake", lake_name),
lake_name = ifelse(lake_name == "little lake joseph", "joseph - little lake joseph", lake_name))




df_muskoka_all = df_muskoka_pdf_latlon %>%
  dplyr::select(lake_name, year, lat_lon, sd_average, n_readings, source) %>%
#  mutate(type = "new") %>%
  bind_rows(df_lpp_muskoka) %>%
  distinct(lake_name, year, .keep_all = T) %>%
  arrange(lake_name, year)

df_muskoka_all_pdfs = df_muskoka_all %>%
  filter(source == "muskoka_pdfs") %>%
  distinct(lake_name, lat_lon)

df_muskoka_all_lat_lon = df_muskoka_all %>%
  filter(source == "lpp") %>%
  distinct(lake_name, lat_lon) %>%
  filter(!(lake_name %in% df_muskoka_all_pdfs$lake_name)) %>%
  bind_rows(df_muskoka_all_pdfs) %>%
  arrange(lake_name)



df_lake_year = df_muskoka_all %>%
  dplyr::select(lake_name, year, sd_average, n_readings, source) %>%
  left_join(df_muskoka_all_lat_lon, by = "lake_name") %>%
  bind_rows(df_lpp_not_muskoka) 


# LAKE SIMCOE DATA
df_simcoe_sf <- st_read(
  "data/wq_data/Lake_Simcoe_Water_Quality_Stations/LakeSimcoeWaterQualityStations.shp")

df_simcoe = read_csv("data/wq_data/SimcoeSecchiDept1980-2019.csv") %>%
  mutate(date = as.Date(date, format = "%m-%d-%Y")) %>%
  rename(STN_FKM = stn) %>%
  group_by(STN_FKM, year) %>%
  summarise(sd_average = mean(secchi, na.rm = T),
            n_readings = n())

df_simcoe_join = df_simcoe %>%
  left_join(df_simcoe_sf, by = "STN_FKM") %>%
  mutate(lat_lon = paste0(LATITUDE, " ", LONGITUDE),
         lake_name = "lake simcoe",
         source = "lake simcoe wq") %>%
  ungroup(.) %>%
  filter(!is.na(sd_average), !is.na(STATION)) %>%
  dplyr::select(lake_name, year, lat_lon, sd_average, n_readings, source)


# GEORGIAN BAY
df_georgian = readxl::read_excel(
  "data/wq_data/Georgian_Bay_Water_Quality_2003_2005.xlsx") %>%
  clean_names(.) %>%
  mutate(year = year(date),
         secchi = parse_number(secchi),
         lat_lon = paste0(latitude, " ", longitude)) %>%
  group_by(description, year ,lat_lon) %>%
  summarise(sd_average = mean(secchi),
            n_readings = n(),
            source = "georgian bay wq 03-05") %>%
  rename(lake_name = description) %>%
  dplyr::select(lake_name, year, lat_lon, sd_average, n_readings, source)

# Lake couching
df_couching_lat_lon = read_delim("data/wq_data/lake_couchiching_2003_stn_latlon.txt", delim = " ", col_names = F) %>%
  rename(stn = X1) %>%
  mutate(longitude_dms = paste0(X5,":",X6,":",X7),
         latitude_dms = paste0(X2,":",X3,":",X4),
         lon = -parse_lon(longitude_dms),
         lat = parse_lat(latitude_dms)) %>%
  mutate(lat_lon = paste0(lat, " ", lon)) %>%
  select(stn, lat_lon)

df_couching = read_delim("data/wq_data/lake_couchiching_2003.txt", delim = " ", col_names = F) %>%
  rename(stn = X1,
         date = X2,
         depth = X3,
         secchi = X4) %>%
  dplyr::select(stn, date, secchi) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"),
         year = year(date)) %>%
  group_by(stn, year) %>%
  summarise(sd_average = mean(secchi),
            n_readings = n()) %>%
  left_join(df_couching_lat_lon) %>%
  mutate(lake_name = "couchiching lake",
         source = "lake couchichin 2003") %>%
  ungroup(.) %>%
  dplyr::select(lake_name, year, lat_lon, sd_average, n_readings, source)


df_gl = read_csv("data/wq_data/df_gl.csv")

df_ontario_moe = read_csv("data/wq_data/df_ontario_moe.csv")

df_lake_year = df_lake_year %>%
  filter(!(str_detect(lake_name, "simcoe lake"))) %>%
  bind_rows(df_simcoe_join) %>%
  bind_rows(df_georgian) %>%
  bind_rows(df_couching) %>%
  bind_rows(df_gl) %>%
  bind_rows(df_ontario_moe) %>%
  ungroup(.) %>%
  group_by(lake_name, lat_lon) %>%
  mutate(lake_index_sd = cur_group_id())

df_lake = df_lake_year %>%
  distinct(lake_index_sd, lake_name, lat_lon)

###########################################################
# NEED TO FIND DUPLICATES
###########################################################

df_lake %>%
#  filter(year > 2001) %>%
  group_by(lat_lon) %>%
  mutate(count = n()) %>%
  filter(count > 1)

remove_lakes_mnr_sheets = c("adams lake", "barrons lake", "hesners lake", "longs lake", "buckhorn lake",
"cardwell lake","clam lake","chub lake","clearwater lake","crawford lake","doe lake","fairy lake","fish lake",
"gartersnake lake","gilbank lake","glouster pool","heeney lake","hesners lake","himbury lake",
"jessop lake","kahshe lake","perbeth lake","kernick lake","lake joseph","lake muskoka",
"lake of bays","lake rosseau","lake vernon","lake waseosa","longs lake","muldrew lakes","north dotty lake",
"north healey lake","paint lake","peninsula lake","perch lake","reay lake","ryde lake","rutter lake",
"six mile lake","south tasso lake","spence lake","torrance lake","upper oxbow lake","upper raft lake",
"upper raven lake","waseosa lake", "lower raft lake","lower raven lake")

df_muskoka_mnr = read_csv("data/wq_data/sd_lake_extra_mnr_sheets.csv") %>%
  dplyr::select(lake_name, lat, lon, sd_average) %>%
  filter(!is.na(sd_average)) %>%
  mutate(lake_name = tolower(lake_name)) %>%
  filter(!(lake_name %in% df_lake$lake_name),
         !(lake_name %in% remove_lakes_mnr_sheets)) %>%
  mutate(lake_id = row_number())

tt = stringdist_join(df_muskoka_mnr, df_lake, 
                     by = "lake_name",
                     mode = "left",
                     ignore_case = FALSE, 
                     method = "jw", 
                     max_dist = 99, 
                     distance_col = "dist") %>%
  group_by(lake_name.x) %>%
  slice_min(order_by = dist, n = 1) %>%
  distinct(lake_id, .keep_all = T)

df_muskoka_mnr = df_muskoka_mnr  %>%
  mutate(lon2 = lon,
         lat2 = lat) %>%
  st_as_sf(coords = c("lon2", "lat2"), crs = 4326) 

df_lake_sf = df_lake %>%
  ungroup(.) %>%
  mutate(lat = round(as.numeric(gsub( " .*$", "", lat_lon )), 4),
         lon = round(as.numeric(gsub( ".* ", "", lat_lon )), 4),
         id_index_link = row_number()) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 


index = st_nn(df_muskoka_mnr, df_lake_sf,
              k = 1,
              returnDist = T)

df_muskoka_mnr = df_muskoka_mnr %>%
  mutate(id_index_link = unlist(index$nn),
         dist = unlist(index$dist) ) %>%
  as_tibble(.) %>%
  left_join(as_tibble(df_lake_sf), by = "id_index_link") %>%
  filter(as.numeric(dist) > 600 | lake_name.x == "bloody lake") %>%
  rename(lake_name = lake_name.x) %>%
  mutate(lat_lon = paste0(lat, " ", lon)) %>%
  select(lake_name, sd_average, lat_lon) %>%
  mutate(lake_index_sd = row_number() + max(df_lake_year$lake_index_sd),
         source = "muskoka mnr sheets",
         n_readings = 1)

df_muskoka_mnr_id = df_muskoka_mnr %>%
  select(lake_index_sd, lake_name, lat_lon)



year = sort(unique(df_lake_year$year))

df_lake = df_lake %>%
  bind_rows(df_muskoka_mnr_id) 



df_lake_year_subset = df_lake_year %>%
  ungroup(.) %>%
  dplyr::select(lake_index_sd, year, sd_average, n_readings, source)


df_all = tibble(lake_index_sd = df_lake$lake_index_sd) %>%
  crossing(year) %>%
  left_join(df_lake, by = c("lake_index_sd")) %>%
  left_join(df_lake_year_subset, by = c("lake_index_sd", "year")) %>%
  # only join mnr data for missing years
  left_join(df_muskoka_mnr, by = c("lake_index_sd", "lat_lon", "lake_name")) %>%
  mutate(sd_average = coalesce(sd_average.x, sd_average.y),
        n_readings = coalesce(n_readings.x, n_readings.y),
        source = coalesce(source.x, source.y),
        interpolation = ifelse(!is.na(sd_average), "none", NA)) %>%
  dplyr::select(-sd_average.x, -sd_average.y, -n_readings.x, -n_readings.y, -source.x, -source.y) %>%
  group_by(lake_index_sd) %>%
  mutate(sd_interp = na.approx(sd_average, na.rm=FALSE, maxgap = 10)) %>% 
  mutate(interpolation = ifelse(is.na(sd_average) & !is.na(sd_interp), "interp", interpolation)) %>%
  # fill next 5 years with sd_interp if missing
  mutate(new_year = max(year[!is.na(sd_interp)]), 
         diff1 = year - new_year) %>% 
  fill(sd_interp, .direction = "down") %>% 
  mutate(interpolation = ifelse(is.na(interpolation) & !is.na(sd_interp), 
                                paste0("year",diff1), interpolation)) %>%
  mutate(new_year = min(year[!is.na(sd_interp)]), 
         diff1 = year - new_year) %>% 
  fill(sd_interp, .direction = "up") %>% 
  mutate(interpolation = ifelse(is.na(interpolation) & !is.na(sd_interp), 
                                paste0("year",diff1), interpolation)) %>%
 # mutate(sd_interp = replace(sd_interp, which(diff1 > 5), NA)) %>%
#  dplyr::select(-new_year, -diff1) %>%
  filter(year > 2001)


df_all = df_all %>% 
  group_by(lake_index_sd) %>%
  mutate(allNA = all(is.na(sd_average)) & (new_year < 1997) ) %>%
  filter(allNA == "FALSE") %>%
  dplyr::select(-allNA, -new_year, -diff1) %>%
  ungroup(.) %>%
  group_by(lake_name, lat_lon) %>%
  mutate(lake_index_sd = cur_group_id()) %>%
  arrange(lake_index_sd) %>%
  rename(lake_name_sd = lake_name)
#  filter(!is.na(sd_interp))

write_csv(df_all, "data/sd_merged_data.csv")

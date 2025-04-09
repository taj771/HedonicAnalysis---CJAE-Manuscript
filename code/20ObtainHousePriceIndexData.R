
# Download Statistics Canada data from Cansim by table name
df <- get_cansim(1810020501) %>% 
  normalize_cansim_values() %>%
  clean_names(.)

# Get recreation data and clean-up province names / abbreviations
df_on <- df %>%
  filter(geo == "Ontario") %>%
  select(date, hpi = value, type = new_housing_price_indexes) %>%
  filter(type == "Total (house and land)")

write_csv(df_on, "data/HouseData/house_price_index_ontario.csv")


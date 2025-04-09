
#set wd to folder where .pdf file is located


#read in .pdf format news article
article_path <- list.files("data/wq_data/muskoka_wq_data/", pattern = "pdf$")

#read in article using lapply
#article <- lapply(paste0("data/wq_data/muskoka_wq_data/",article_path), pdf_data)

# All textboxes on page 1
force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
}

# 2003
pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[1])
out <- extract_tables(pdf_file, pages = 5)


out_temp = out[[1]][,3:8] %>%
  as_tibble() %>%
  dplyr::select(-V2, -V3) %>%
  filter(V1 != "" | V4 != "") %>%
  mutate(V4 = ifelse(V4 == "", lead(V4), V4),
         V5 = ifelse(V5 == "", lead(V5), V5),
         V6 = ifelse(V6 == "", lead(V6), V6)) %>%
  filter(V4 != lag(V4)) %>%
  mutate(V1 = ifelse(V1 == "", "Wildcat", V1)) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  mutate(lake_name = case_when(lake_name == "Brackenrig Bay" ~ "Rosseau - Brackenrig Bay",
                               lake_name == "Portage Bay" ~ "Rosseau - East Portage Bay",
                               lake_name == "Bay" ~ "Rosseau - Skeleton Bay",
                               lake_name == "Park Bay" ~ "Six Mile - Provincial Park Bay",
                               lake_name == "Nook" ~ "Six Mile - Cedar Nook",
                               TRUE ~ lake_name))

df_extra = tibble(lake_name = "Six Mile - Main",
                  sd_may = "4.00",
                  sd_aug = "5.20",
                  sd_average = "4.60")

out2003 = out[[1]][,1:2] %>%
  as_tibble() %>%
  filter(V1 != "") %>%
  mutate(V12 = case_when(V2 == "" & lead(V2) == "" ~ paste0(V1,lead(V1),lead(V1,2)), 
                         V2 == "" ~ paste0(V1,lead(V1)),
                         TRUE ~ V1)) %>%
  slice(-1) %>%
  mutate(num = parse_number(V12),
         V122 = V12) %>%
  extract(V12, c("sd_may", "sd_aug"),
          "(?s)(\\d+\\.\\d+).(?s)(\\d+\\.\\d+)", 
          convert = TRUE) %>%
  mutate(lake_name = str_replace_all(V122, "(?s)(\\d+\\.\\d+)", "")) %>%
  mutate(lake_name = str_replace_all(lake_name, "Wildcat", "")) %>%
  dplyr::select(lake_name, sd_may, sd_aug) %>%
  distinct(sd_may, sd_aug, .keep_all = T) %>%
  mutate(sd_average = (sd_may + sd_aug)/2) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(lake_name = gsub("\\Rosseau.*","", lake_name))  %>%
  mutate(lake_name = gsub("\\Sparrow.*","", lake_name)) %>%
  mutate(lake_name = gsub("\\Six Mile.*","", lake_name)) %>%
  bind_rows(out_temp, df_extra) %>%
  mutate(year = 2003)



# 2004
pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[2])
out <- extract_tables(pdf_file, pages = 5)

out_temp = out[[1]] %>%
  as_tibble() %>%
  filter(V1 != "") %>%
dplyr::select(-V2, -V7) 

out2004 = out_temp[,1:4] %>%
  force_bind(out_temp[,5:8]) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  mutate(year = 2004)


# 2005
pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[3])

out <- extract_tables(pdf_file, pages = 6)

out_temp = out[[1]] %>%
  as_tibble() %>%
  filter(V1 != "") %>%
  dplyr::select(-V2, -V5, -V7) %>%
  separate(V4, into = c("V4", "V5"), sep = " ") %>%
  separate(V9, into = c("V9", "V10"), sep = " ")

out2005 = out_temp[,1:4] %>%
  force_bind(out_temp[,5:8]) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2005)


# 2006
pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[4])

out <- extract_tables(pdf_file, pages = 5)

out_temp = out[[1]] %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V4) %>%
  separate(V3, into = c("V3", "V4"), sep = " ") 

out2006 = out_temp[,1:4] %>%
  force_bind(out_temp[,5:8]) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "")

out_error = out2006 %>%
  filter(lake_name == "Shoe Siding") %>%
  pivot_longer(everything()) %>%
  separate(value, into = c("V1", "V2"), sep = " ") %>%
  pivot_longer(-name, names_to = "lakes", values_to = "value")  %>%
  pivot_wider(names_from = "name") %>%
  dplyr::select(-lakes)

out2006 = out2006 %>%
  filter(lake_name != "Shoe Siding") %>%
  bind_rows(out_error) %>%
  mutate(year = 2006)


  

# 2007
pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[5])

out <- extract_tables(pdf_file, pages = 7)

out_temp = out[[1]] %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2, -V5) %>%
  separate(V3, into = c("V3", "V3.5"), sep = " ")  %>%
  separate(V7, into = c("V7", "V7.5"), sep = " ") 

out2007 = out_temp[,1:4] %>%
  force_bind(out_temp[,5:8]) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2007)


# 2008
pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[6])

out <- extract_tables(pdf_file, pages = 5)

out_temp = out[[1]] %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2, -V7)

out2008 = out_temp[,1:4] %>%
  force_bind(out_temp[,5:8]) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2008)

# 2008
pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[7])

out <- extract_tables(pdf_file, pages = 5)

out_temp = out[[1]] %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out2009 = out_temp[,1:4] %>%
  force_bind(out_temp[,5:8]) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2009)

# 2010
# error here
#pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[8])

#out <- extract_tables(pdf_file, pages = 5)

#out_temp = out[[1]] %>%
#  as_tibble() %>%
#  filter(V1 != "", V1 != "Lake Name")


df_temp = read_csv("data/wq_data/muskoka_wq_data/muskoka2010data_half.csv", col_names = F) %>%
  mutate(index = rep(c("lake_name", "sd_may", "sd_aug", "sd_average"), nrow(.)/4),
         lake_index = rep(1:(nrow(.)/4), each = 4)) %>%
  pivot_wider(names_from = "index", values_from = "X1") %>%
  dplyr::select(-lake_index)

out2010 = #out_temp[,1:4] %>%
#  force_bind(out_temp[,5:8]) %>%
#  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
 # filter(lake_name != "") %>%
#  bind_rows(df_temp) %>%
  df_temp %>%
  mutate(year = 2010)


pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[9])

out <- extract_tables(pdf_file, pages = 6)

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out <- extract_tables(pdf_file, pages = 7)

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out2011 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2011)


pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[10])

out <- extract_tables(pdf_file, pages = 6)

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out <- extract_tables(pdf_file, pages = 7)

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out2012 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2012)


pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[11])

out <- extract_tables(pdf_file, pages = 6)

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out <- extract_tables(pdf_file, pages = 7)

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out2013 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2013)


pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[12])

out <- extract_tables(pdf_file, pages = 6)

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out <- extract_tables(pdf_file, pages = 7)

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out2014 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2014)

pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[13])

out <- extract_tables(pdf_file, pages = 6)

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out <- extract_tables(pdf_file, pages = 7)

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out2015 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2015)


pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[14])

out <- extract_tables(pdf_file, pages = 6)

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out <- extract_tables(pdf_file, pages = 7)

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out2016 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2016)


pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[15])

out <- extract_tables(pdf_file, pages = 6)

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out <- extract_tables(pdf_file, pages = 7)

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out2017 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2017)


pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[16])

out <- extract_tables(pdf_file, pages = 34)

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)


# Use to manually extract information
#f <- locate_areas(pdf_file,pages = 35)

out <- extract_tables(pdf_file, 
                      pages = c(35),
                      area = list(c(74.58641,  68.65061, 697.42136, 522.20896)))

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  mutate(V1 = ifelse(V1 == "", V2, V1),
         V2 = ifelse(V1 == V2, V3, V2),
         V3 = ifelse(V2 == V3, V4, V3),
         V4 = ifelse(V3 == V4, V5, V4),
         V4 = ifelse(V4 == "", V3, V4)) %>%
  filter(V1 != "", V1 != "Lake Name", V1 != "Spring Secchi") %>%
  dplyr::select(-V5) %>%
  rename(V3 = V2, V4 = V3, V5 = V4)

out2018 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2018)


pdf_file = paste0("data/wq_data/muskoka_wq_data/",article_path[17])

out <- extract_tables(pdf_file, pages = c(31))

out_temp = as.matrix(out[[1]]) %>%
  as_tibble() %>%
  filter(V1 != "", V1 != "Lake Name") %>%
  dplyr::select(-V2)

out <- extract_tables(pdf_file, pages = 32)

out_temp2 = as.matrix(out[[1]]) %>%
  as_tibble()  %>%
  mutate(V1 = ifelse(V1 == "", V2, V1),
         V2 = ifelse(V1 == V2, V3, V2),
         V3 = ifelse(V2 == V3, V4, V3),
         V4 = ifelse(V3 == V4, V5, V4),
         V4 = ifelse(V4 == "", V3, V4)) %>%
  filter(V1 != "", V1 != "Lake Name", V1 != "Spring Secchi") %>%
  dplyr::select(-V5) %>%
  rename(V3 = V2, V4 = V3, V5 = V4)

out2019 = bind_rows(out_temp, out_temp2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average")) %>%
  filter(lake_name != "") %>%
  mutate(year = 2019)


wq_all = bind_rows(out2003, out2004, out2005, out2006, out2007, out2008, out2009,
                   out2010, out2011, out2012, out2013, out2014, out2015, out2016, 
                   out2017, out2018, out2019) %>%
  mutate(lake_name = str_trim(lake_name))

write_csv(wq_all, "data/wq_sd_muskoka_files.csv")


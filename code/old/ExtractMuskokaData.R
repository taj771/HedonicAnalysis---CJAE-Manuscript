
# Cleans environment
rm(list=ls(all=TRUE))


library(pdftools)
library(tabulizer)
library(janitor)
#set wd to folder where .pdf file is located

#read in .pdf format news article
article_path <- list.files("data/", pattern = "pdf$")

#read in article using lapply
article <- lapply(paste0("data/",article_path), pdf_data)

pdf_file = paste0("data/",article_path[1])
# All textboxes on page 1
tt = pdf_data(pdf_file)[[4]]

article1 = article[[4]]

out <- extract_tables(pdf_file, pages = 4)

out2 = out[[1]][,3:8] %>%
  as_tibble() %>%
  select(-V2, -V3) %>%
  filter(V1 != "" | V4 != "") %>%
  mutate(V4 = ifelse(V4 == "", lead(V4), V4),
         V5 = ifelse(V5 == "", lead(V5), V5),
         V6 = ifelse(V6 == "", lead(V6), V6)) %>%
  filter(V4 != lag(V4)) %>%
  mutate(V1 = ifelse(V1 == "", "Wildcat", V1)) %>%
  set_names(c("lake_name", "tp1", "tp2", "tp_average")) %>%  #shift names from first row
  mutate(tp1 = as.numeric(tp1),
         tp2 = as.numeric(tp2),
         tp_average = as.numeric(tp_average))

out1 = out[[1]][,1:2] %>%
  as_tibble() %>%
  filter(V1 != "") %>%
  mutate(V12 = case_when(V2 == "" & lead(V2) == "" ~ paste0(V1,lead(V1),lead(V1,2)), 
                        V2 == "" ~ paste0(V1,lead(V1)),
                     TRUE ~ V1)) %>%
  slice(-1) %>%
  mutate(num = parse_number(V12),
         V122 = V12) %>%
   extract(V12, c("tp1", "tp2"),
           "(?s)(\\d+\\.\\d+).(?s)(\\d+\\.\\d+)", 
           convert = TRUE) %>%
  mutate(lake_name = str_replace_all(V122, "(?s)(\\d+\\.\\d+)", "")) %>%
  mutate(lake_name = str_replace_all(lake_name, "Wildcat", "")) %>%
  select(lake_name, tp1, tp2) %>%
  distinct(tp1, tp2, .keep_all = T) %>%
  mutate(tp_average = (tp1 + tp2)/2) %>%
  bind_rows(out2)

out <- extract_tables(pdf_file, pages = 5)


out2 = out[[1]][,3:8] %>%
  as_tibble() %>%
  select(-V2, -V3) %>%
  filter(V1 != "" | V4 != "") %>%
  mutate(V4 = ifelse(V4 == "", lead(V4), V4),
         V5 = ifelse(V5 == "", lead(V5), V5),
         V6 = ifelse(V6 == "", lead(V6), V6)) %>%
  filter(V4 != lag(V4)) %>%
  mutate(V1 = ifelse(V1 == "", "Wildcat", V1)) %>%
  set_names(c("lake_name", "tp1", "tp2", "tp_average")) %>%  #shift names from first row
  mutate(tp1 = as.numeric(tp1),
         tp2 = as.numeric(tp2),
         tp_average = as.numeric(tp_average))

out1 = out[[1]][,1:2] %>%
  as_tibble() %>%
  filter(V1 != "") %>%
  mutate(V12 = case_when(V2 == "" & lead(V2) == "" ~ paste0(V1,lead(V1),lead(V1,2)), 
                         V2 == "" ~ paste0(V1,lead(V1)),
                         TRUE ~ V1)) %>%
  slice(-1) %>%
  mutate(num = parse_number(V12),
         V122 = V12) %>%
  extract(V12, c("tp1", "tp2"),
          "(?s)(\\d+\\.\\d+).(?s)(\\d+\\.\\d+)", 
          convert = TRUE) %>%
  mutate(lake_name = str_replace_all(V122, "(?s)(\\d+\\.\\d+)", "")) %>%
  mutate(lake_name = str_replace_all(lake_name, "Wildcat", "")) %>%
  select(lake_name, tp1, tp2) %>%
  distinct(tp1, tp2, .keep_all = T) %>%
  mutate(tp_average = (tp1 + tp2)/2) %>%
  bind_rows(out2)


out <- extract_tables(pdf_file, pages = 4:5, area = list(c(126, 284, 174, 417)))

out_tp = as_tibble(out[[1]]) %>%
  bind_rows(as_tibble(out[[2]])) %>%
  select(-V2) %>%
  set_names(c("lake_name", "date", "tp1", "tp2", "tp_average")) %>%   #shift names from first row
    filter(!(lake_name == ""))

out_sd = as_tibble(out[[3]]) %>%
  bind_rows(as_tibble(out[[4]])) %>%
  select(-V2) %>%
  set_names(c("lake_name", "sd_may", "sd_aug", "sd_average"))   %>%   #shift names from first row
  filter(!(lake_name == ""))


# 2005
pdf_file = paste0("data/",article_path[3])

out <- extract_tables(pdf_file, pages = 6)

out2 = out[[1]]%>%
  as_tibble() %>%
  select(-V2, -V5, -V7) %>%
  filter(V1 != "" | V6 != "")
  
out_first = out2 %>%
  select(V1:V4) %>%
  set_names(c("lake_name", "sd1", "sd2")) 
  

out = out2 %>%
  select(V6:V9) %>%
  set_names(c("lake_name", "sd1", "sd2")) %>%
  bind_rows(out_first) %>%
  filter(lake_name != "")


%>%
  
  set_names(c("lake_name", "tp1", "tp2", "tp_average")) %>%  #shift names from first row
  mutate(tp1 = as.numeric(tp1),
         tp2 = as.numeric(tp2),
         tp_average = as.numeric(tp_average))

out1 = out[[1]][,1:2] %>%
  as_tibble() %>%
  filter(V1 != "") %>%
  mutate(V12 = case_when(V2 == "" & lead(V2) == "" ~ paste0(V1,lead(V1),lead(V1,2)), 
                         V2 == "" ~ paste0(V1,lead(V1)),
                         TRUE ~ V1)) %>%
  slice(-1) %>%
  mutate(num = parse_number(V12),
         V122 = V12) %>%
  extract(V12, c("tp1", "tp2"),
          "(?s)(\\d+\\.\\d+).(?s)(\\d+\\.\\d+)", 
          convert = TRUE) %>%
  mutate(lake_name = str_replace_all(V122, "(?s)(\\d+\\.\\d+)", "")) %>%
  mutate(lake_name = str_replace_all(lake_name, "Wildcat", "")) %>%
  select(lake_name, tp1, tp2) %>%
  distinct(tp1, tp2, .keep_all = T) %>%
  mutate(tp_average = (tp1 + tp2)/2) %>%
  bind_rows(out2)

out <- extract_tables(pdf_file, pages = 5)


out2 = out[[1]][,3:8] %>%
  as_tibble() %>%
  select(-V2, -V3) %>%
  filter(V1 != "" | V4 != "") %>%
  mutate(V4 = ifelse(V4 == "", lead(V4), V4),
         V5 = ifelse(V5 == "", lead(V5), V5),
         V6 = ifelse(V6 == "", lead(V6), V6)) %>%
  filter(V4 != lag(V4)) %>%
  mutate(V1 = ifelse(V1 == "", "Wildcat", V1)) %>%
  set_names(c("lake_name", "tp1", "tp2", "tp_average")) %>%  #shift names from first row
  mutate(tp1 = as.numeric(tp1),
         tp2 = as.numeric(tp2),
         tp_average = as.numeric(tp_average))

out1 = out[[1]][,1:2] %>%
  as_tibble() %>%
  filter(V1 != "") %>%
  mutate(V12 = case_when(V2 == "" & lead(V2) == "" ~ paste0(V1,lead(V1),lead(V1,2)), 
                         V2 == "" ~ paste0(V1,lead(V1)),
                         TRUE ~ V1)) %>%
  slice(-1) %>%
  mutate(num = parse_number(V12),
         V122 = V12) %>%
  extract(V12, c("tp1", "tp2"),
          "(?s)(\\d+\\.\\d+).(?s)(\\d+\\.\\d+)", 
          convert = TRUE) %>%
  mutate(lake_name = str_replace_all(V122, "(?s)(\\d+\\.\\d+)", "")) %>%
  mutate(lake_name = str_replace_all(lake_name, "Wildcat", "")) %>%
  select(lake_name, tp1, tp2) %>%
  distinct(tp1, tp2, .keep_all = T) %>%
  mutate(tp_average = (tp1 + tp2)/2) %>%
  bind_rows(out2)

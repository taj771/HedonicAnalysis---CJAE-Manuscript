
rm(list=ls(all=TRUE))

# Load packages
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# This line will install these packages if not already installed and load them
p_load(tidyverse, sf, cansim, janitor, 
       furrr, progressr, lubridate, readxl, 
       pdftools,# tabulizer, 
       fuzzyjoin, parzer, zoo, nngeo, haven, ggmap,
       tidystringdist, spatialrisk,
       fixest, modelsummary)


source("code/01. Main_Table_2.R")
source("code/02. Main_Table_3.R")
source("code/03. Main_Table_4.R")
source("code/04. Main_Table_5.R")
source("code/05. Main_Table_6.R")
source("code/06. Main_Table_7.R")
source("code/07.Appendix_Table_2.R")
source("code/08.Appendix_Table_6.R")
source("code/09.Appendix_Table_7.R")










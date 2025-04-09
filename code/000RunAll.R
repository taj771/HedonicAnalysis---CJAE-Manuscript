
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

# Get CanvecData
source("code/00ImportCanvecData.R")
source("code/01SetCanvecLakeSize.R")

#source("code/10CleanDataStream.R")

# Get WQ data
source("code/11CleanGreatLakesData.R")
source("code/12ExtractSDMuskoka.R")
source("code/13ConvertLPPData.R")
source("code/14CompileWQData.R")
source("code/15MergeWQLake.R")

# House data
source("code/20ObtainHousePriceIndexData.R")
source("code/21CleanHousingData.R")
#source("code/22FindMissingHouseLatLong.R") ggmap
source("code/24MergeClosestCMA.R")

# Merge data
source("code/31CalcDistHouseLakes.R")
source("code/32MergeWQdataHouse.R")

# Create all variables used in analysis
source("code/40CreateAnalysisSample.R")

# Estimate models
source("code/50EstimateModels.R")

# subset data
# closest to
# lake: only include lakes and reservoirs
# water: include rivers as well
# water_sd: 

# data_type
# lake: merge based on lakes
# station: merge based on stations

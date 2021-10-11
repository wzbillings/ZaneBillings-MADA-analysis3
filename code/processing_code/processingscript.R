###############################
# Processing Script
# Zane Billings
# 
# This script loads the raw data, processes and cleans it, 
# and saves it as Rds file in the processed_data folder
###############################

# Load needed packages
library(dplyr) # for data processing
library(tidyr) # for data processing
library(here) # to set paths

###############################
# Importing data
###############################

# path to data
data_location <- here::here("data","raw_data","SympAct_Any_Pos.Rda")

# load data.
rawdata <- readRDS(data_location)

###############################
# Data processing
###############################

processeddata <- rawdata |>
  # Remove all variables with Score, Total, FluA, FluB, Dxname, or Activity
  # as part of the name
  dplyr::select(
    -contains("Score"),
    -contains("Total"),
    -contains("FluA"),
    -contains("FluB"),
    -contains("Dxname"),
    -contains("Activity"),
    # Also remove Unique.Visit
    -Unique.Visit
  ) |>
  # Remove all NA observations
  tidyr::drop_na()

###############################
# Saving data to file
###############################

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)



###############################
# Processing Script
# Zane Billings
# 
# This script loads the raw data, processes and cleans it, 
# and saves it as Rds file in the processed_data folder
###############################

# Load needed packages
library(dplyr) # for data processing
library(here) # to set paths

# path to data
data_location <- here::here("data","raw_data","exampledata.xlsx")

# load data.
rawdata <- readxl::read_excel(data_location)


# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)



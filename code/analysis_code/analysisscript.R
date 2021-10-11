###############################
# Analysis Script
# Zane Billings
#
# This script loads the processed, cleaned data, does a simple analysis
# and saves the results to the results folder.
###############################

# Load needed packages

# path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data fitting/statistical analysis
######################################

# fit linear model
lmfit <- lm(Weight ~ Height, mydata)  

# place results from fit into a data frame with the tidy function
lmtable <- broom::tidy(lmfit)

#look at fit results
print(lmtable)

# save fit results table  
table_file = here("results", "resulttable.rds")
saveRDS(lmtable, file = table_file)

  
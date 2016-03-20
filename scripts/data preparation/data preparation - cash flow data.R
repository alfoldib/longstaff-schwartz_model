## Preparing cash-flow data

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(stringr)

setwd("C:/Users/Boldi/Documents/Publish/Interest rate models/longstaff-schwartz_model")

## Read the file which contains the data
file_name <- "./data/raw/hun_cash_flow_20150908.csv"

# Getting date from the filename
date <- gsub("\\D", "", file_name)

# Data were received via e-mail from ÁKK Zrt.
data <- read.csv2(file_name, header = T, 
                  stringsAsFactors = F, na.strings = "NULL")


# Converting the fieldnames from uppercase separated to underline separated
names(data) <- tolower(gsub('([[:upper:]])', '_\\1', names(data)))


# Convert columns which contains dates to date datatype
for (col in grep("date", names(data))) {
  data[, col] <- as.Date(data[, col], format = "%Y.%m.%d")
}

cash_flow <- data.table(data)


# Save prepared data
save(cf_data, file = paste("./data/hun_cash_flow_", date, ".rData", sep = ""))
# Preparing cash-flow data

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(stringr)

setwd("C:/Users/Boldi/Documents/Publish/Interest rate models/longstaff-schwartz_model")

file_name <- "./data/raw/hun_cash_flow_20150908.csv"
date <- gsub("\\D", "", file_name)

data <- read.csv2(file_name, header = T, 
                  stringsAsFactors = F, na.strings = "NULL")

names(data) <- tolower(gsub('([[:upper:]])', '_\\1', names(data)))

for (col in grep("date", names(data))) {
  data[, col] <- as.Date(data[, col], format = "%Y.%m.%d")
}

cash_flow <- data.table(data)
save(cf_data, file = paste("./data/hun_cash_flow_", date, ".rData", sep = ""))
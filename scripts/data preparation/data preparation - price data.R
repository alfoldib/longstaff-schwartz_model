### Preparing market price data

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(stringr)

setwd("C:/Users/Boldi/Documents/Publish/Interest rate models/longstaff-schwartz_model")

## Read the file which contains the data
# File name
file_name <- "./data/raw/hun_best_prices_20150927.csv"

# Getting date from the filename
date <- gsub("\\D", "", file_name)

# Read dataset and header separately
# - there is a double header (one in hungarian and one in english)
# - dataset was downloaded from "http://akk.hu/hu/oldal/statisztika"
data <- read.csv2(file_name, header = F, 
                  stringsAsFactors = F, na.strings = "", skip = 2)

header <- read.csv2(file_name, header = T, 
                    stringsAsFactors = F, na.strings = "", nrow = 1)

## Format the header
# Clear header from characters which are not letters
header[1, ] <- gsub("\\(|\\)|%", "", header[1, ])
for (col in names(header)) { header[, col] <- str_trim(header[, col])}

# Hard coded formating
# - the dataset had two columns with the same name
tmp <- "Bid Dealer"

for (col in names(header)) { 
  if (header[1, col] == "Dealer") {
    header[1, col] <- tmp
    tmp <- "Ask Dealer"
  }
}

rm(tmp)

header[1, ] <- tolower(gsub(" ", "_", header[1, ]))

names(data) <- header[1, ]
rm(header)


## Prepare dataset
# Convert columns which contains dates to date datatype
for (col in grep("date", names(data))) {
  data[, col] <- as.Date(data[, col], format = "%Y.%m.%d")
}

best_price <- data.table(data)

# Creating gross and net mid price variable (adjusting for missing prices)
best_price[, net_mid_price := (bid_net_price + ask_net_price) / 
       ifelse(bid_net_price == 0 | ask_net_price == 0, 1, 2)]
best_price[, gross_mid_price := net_mid_price + accrued_interest]
best_price <- best_price[order(security_name, date)]

# Save prepared dataset
save(best_price, file = paste("./data/hun_best_price_", date, ".rData", sep = ""))

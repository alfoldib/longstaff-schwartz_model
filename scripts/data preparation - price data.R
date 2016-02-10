# Preparing market price data

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(stringr)

setwd("C:/Users/Boldi/Documents/Publish/Interest rate models/longstaff-schwartz_model")

file_name <- "./data/raw/hun_best_prices_20150927.csv"
date <- gsub("\\D", "", file_name)

data <- read.csv2(file_name, header = F, 
                  stringsAsFactors = F, na.strings = "", skip = 2)

header <- read.csv2(file_name, header = T, 
                    stringsAsFactors = F, na.strings = "", nrow = 1)

header[1, ] <- gsub("\\(|\\)|%", "", header[1, ])
for (col in names(header)) { header[, col] <- str_trim(header[, col])}

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

for (col in grep("date", names(data))) {
  data[, col] <- as.Date(data[, col], format = "%Y.%m.%d")
}

best_price <- data.table(data)

best_price[, net_mid_price := (bid_net_price + ask_net_price) / 
       ifelse(bid_net_price == 0 | ask_net_price == 0, 1, 2)]
best_price[, gross_mid_price := net_mid_price + accrued_interest]
best_price <- best_price[order(security_name, date)]

save(best_price, file = paste("./data/hun_best_price_", date, ".rData", sep = ""))

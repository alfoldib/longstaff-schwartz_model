# Preparing treasury bond yield data

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(stringr)

setwd("C:/Users/Boldi/Documents/Publish/Interest rate models/longstaff-schwartz_model")

file_name <- "./data/raw/hun_yields_20150927.csv"
date <- gsub("\\D", "", file_name)

data <- read.csv2(file_name, header = F, 
                  stringsAsFactors = F, na.strings = "", skip = 2)

header <- read.csv2(file_name, header = T, 
                    stringsAsFactors = F, na.strings = "", nrow = 1)

header[1, ] <- gsub("\\(|\\)|%", "", header[1, ])
for (col in names(header)) { header[, col] <- str_trim(header[, col])}

header[1, ] <- tolower(gsub(" ", "_", header[1, ]))

names(data) <- header[1, ]
rm(header)

for (col in grep("date", names(data))) {
  data[, col] <- as.Date(data[, col], format = "%Y.%m.%d")
}

yields <- data.table(data)
setnames(yields, c("isin_code", "security_name"), c("security_name", "isin_code"))


# Overnight yields
file_name <- "./data/raw/hun_hufonia_20150927.csv"

data <- read.csv2(file_name, header = F, 
                  stringsAsFactors = F, na.strings = "", skip = 2)

names(data) <- c("date", "yield", "turnover")

for (col in grep("date", names(data))) {
  data[, col] <- as.Date(data[, col], format = "%Y.%m.%d")
}

data <- data.table(data)

data <- data[order(date)]
data[, change := c(NA, diff(yield))]
data[, tenor := "O/N"]
data[, turnover := NULL]
data[, setdiff(names(yields), names(data)) := NA]

yields <- rbind(data[, names(yields), with = F], yields)

save(yields, file = paste("./data/hun_yields_", date, ".rData", sep = ""))

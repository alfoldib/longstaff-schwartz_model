## Preparing treasury bond yield data

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(stringr)

setwd("C:/Users/Boldi/Documents/Publish/Interest rate models/longstaff-schwartz_model")

## Read the file which contains the data
# File name
file_name <- "./data/raw/hun_yields_20150927.csv"


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

header[1, ] <- tolower(gsub(" ", "_", header[1, ]))

names(data) <- header[1, ]
rm(header)


## Prepare dataset
# Convert columns which contains dates to date datatype
for (col in grep("date", names(data))) {
  data[, col] <- as.Date(data[, col], format = "%Y.%m.%d")
}

yields <- data.table(data)

# Security name and isin code were mixed up
setnames(yields, c("isin_code", "security_name"), c("security_name", "isin_code"))


## Adding Overnight market interest rate
file_name <- "./data/raw/hun_hufonia_20150927.csv"

# Read file which contains the dataset
# downloaded from "http://www.mnb.hu/statisztika/statisztikai-adatok-informaciok/"
#                         "adatok-idosorok/v-egyeb-penzugyi-adatok"
data <- read.csv2(file_name, header = F, 
                  stringsAsFactors = F, na.strings = "", skip = 2)

# Manual renaming of the dataset
names(data) <- c("date", "yield", "turnover")

# Convert columns which contains dates to date datatype
for (col in grep("date", names(data))) {
  data[, col] <- as.Date(data[, col], format = "%Y.%m.%d")
}

data <- data.table(data)

data <- data[order(date)]

# Creating change variable, which is the first order difference of the time.series
data[, change := c(NA, diff(yield))]
data[, tenor := "O/N"]
data[, turnover := NULL]

# Adding columns which are present in previously imported dataset
data[, setdiff(names(yields), names(data)) := NA]

# Combine the two datasets
yields <- rbind(data[, names(yields), with = F], yields)

# Save the prepared dataset
save(yields, file = paste("./data/hun_yields_", date, ".rData", sep = ""))

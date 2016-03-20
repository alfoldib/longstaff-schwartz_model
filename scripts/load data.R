# Subroutine which loads the data

  data_dir <- "./data"
  files <- list.files(data_dir)[grepl(".rData", list.files(data_dir))]
  
  for (file in files) {
    load(paste(data_dir, "/", file, sep = ""))
  }
  
  rm(data_dir, file, files)
  
  # Change names so it is uniform in all databases
  setnames(best_price, c("security_name", "isin_code"), c("alias", "isin"))
  setnames(yields, c("security_name", "isin_code"), c("alias", "isin"))
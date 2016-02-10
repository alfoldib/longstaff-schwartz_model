# Eljárás az adatbetöltéshez

  data_dir <- "./data"
  files <- list.files(data_dir)[grepl(".rData", list.files(data_dir))]
  
  for (file in files) {
    load(paste(data_dir, "/", file, sep = ""))
  }
  
  rm(data_dir, file, files)
  
  setnames(best_price, c("security_name", "isin_code"), c("alias", "isin"))
  setnames(yields, c("security_name", "isin_code"), c("alias", "isin"))
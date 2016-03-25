# Calibrating the model to market prices and estimating the zero coupon yield curve

rm(list=ls(all=TRUE))
gc()

require(DEoptim)
require(parallel)
require(data.table)

setwd("C:/Users/Boldi/Documents/Publish/Interest rate models/longstaff-schwartz_model")

# Loading data and functions
source("./scripts/load data.R")
source("./functions/longstaff-schwarcz functions.R")
source("./functions/goodness-of-fit functions.R")
source("./functions/support functions for DE optimization.R")

# Getting currency for each bond id (alias)
ccy <- cf_data[, .N, by = list(alias, currency)]
ccy <- merge(best_price, ccy, by = "alias", all.x = T)

# Preparation cash-flow data 
# - only bonds denominated in HUF with fixed interest rate
prep_cf_data <- cf_data[interest_type != "FLOAT" & currency == "HUF",
                        list(
                          alias,
                          issue_date,
                          maturity_date,
                          cf_date = duedate,
                          cf_amount = ((ifelse(is.na(principal_amount) | principal_amount < 0, 
                                               0, 
                                               principal_amount) + 
                                         ifelse(is.na(interest_amount), 
                                                0, 
                                                interest_amount)) / 
                                         face_value * 100)
                        )]
prep_cf_data <- prep_cf_data[cf_amount != 0]

# Preparation of market prices 
# - filtering out those prices where I don't have cash-flow
prep_prices <- best_price[,
                           list(
                             alias, 
                             date, 
                             bid = bid_net_price,
                             ask = ask_net_price,
                             acc_int = accrued_interest,
                             price = gross_mid_price
                            )]

prep_prices <- merge(prep_prices, prep_cf_data[, .N, by = list(alias, issue_date, maturity_date)], by = "alias")
prep_prices[, N := NULL]

# Storing names of parameters in a vector
param_name <- c("alpha", "beta", "gamma", "delta", "eta", "nu")

# Gathering the dates from the market price dataset
dates <- sort(unique(best_price$date))
dates <- dates[dates >= as.Date("2015-06-01") & dates < as.Date("2015-07-01")]

# Looping through all dates from the dataset
# !! It takes a while, first run it for 1 date!!
for (curr_date in dates) {
  
  # If you run it without the loop, please add a relevant date here
  curr_date <- as.Date(curr_date, origin = "1970-01-01")
  
  # Write current date on the console
  cat("Starting date", as.character(curr_date), "\n")
  
  # Next command is to overcome buffered output in RGui
  # in order to see where the loop at it's iteration process
  flush.console()
  
  # Filter current market prices
  curr_prices <- prep_prices[date == curr_date]
  curr_prices <- curr_prices[order(alias)]
  
  # Filter cash-flow data of current bonds
  curr_bonds <- merge(prep_cf_data, curr_prices[, list(alias)], by = "alias")
  
  # Creating the cash-flow matrix
  cf_matrix <- dcast(curr_bonds[cf_date > curr_date], cf_date ~ alias, value.var = "cf_amount", fill = 0)
  cf_matrix[, t := (cf_date - curr_date) / 365]
  
  # Transform cash-flow data.table into matrix variable type
  curr_cf_matrix <- as.matrix.data.frame(cf_matrix[, !grepl("date|t", names(cf_matrix)), with = F])
  rownames(curr_cf_matrix) <- round(cf_matrix$t, 4)
  curr_cf_matrix <- curr_cf_matrix[, sort(colnames(curr_cf_matrix))]
  
  # Getting values of the state variables
  curr_yield <- state_data[date == curr_date & short_name == "yield", value]
  curr_var   <- state_data[date == curr_date & short_name == "var", value]
  
  # Determine lower and upper bound of parameter search space
  var_yield_rate <- curr_var / curr_yield
  lower_bound    <- c(0, var_yield_rate, 0, 0, 0, -100)
  upper_bound    <- c(var_yield_rate, 100, 100, 100, 100, 100)
  
  # Searching the optimal parameter vector with DE optimization
  # - Parallel computing, with all available cores you have on the computer
  optimum <- DEoptim(obj_function, lower_bound, upper_bound, 
                     control = list(trace = F, itermax = 5000, 
                                    parallelType = 1, 
                                    parVar = c("curr_prices", "curr_cf_matrix",
                                               "curr_yield", "curr_var",
                                               "wmae", "mod_discount",
                                               "A", "B", "C", "D")))
  
  # Gather relevant data from a single optimization
  curr_prices <- merge(curr_prices, extract_bond_spec_res(optimum$optim$bestmem), by = "alias", all.x = T)
  curr_obj_value <- data.table(date = curr_date, obj_value = optimum$optim$bestval)
  curr_est_params <- data.table(date = curr_date, name = param_name, value = optimum$optim$bestmem)
  
  # Storing the relevant data from the optimization
  # (fitted price, estimation error, durations, etc.)
  if (exists("estim_price_data")) {
    estim_price_data <- rbind(estim_price_data, copy(curr_prices))
    
  } else {
    estim_price_data <- copy(curr_prices)
    
  }
  
  # Storing estimated best parameters from the optimization
  if (exists("est_params")) {
    est_params <- rbind(est_params, copy(curr_est_params))
    
  } else {
    est_params <- copy(curr_est_params)
    
  }
  
  # Storing the value of the objection function after the optimization
  if (exists("obj_fun_values")) {
    obj_fun_values <- rbind(obj_fun_values, copy(curr_obj_value))
    
  } else {
    obj_fun_values <- copy(curr_obj_value)
    
  }
  
  
}

# Save data
save(estim_price_data, file = "./Data/estimated_price_database.rdata")
save(est_params, file = "./Data/estimated_parameters_database.rdata")
save(obj_fun_values, file = "./Data/objective_function_values_database.rdata")
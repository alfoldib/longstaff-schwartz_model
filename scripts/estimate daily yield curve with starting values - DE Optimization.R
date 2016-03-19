rm(list=ls(all=TRUE))
gc()

require(DEoptim)
require(parallel)
require(data.table)
require(foreach)

setwd("C:/Users/Boldi/Documents/Szakdoga")

source("./Scripts/load data.R")
source("./Functions/Longstaff - Schwarcz functions.R")
source("./Functions/Goodness-of-fit functions.R")

load("./Data/start parameters.rdata")

# Vektorosra módosított diszkont kötvény ár függvény
mod_discount <- Vectorize(discount, vectorize.args = "tau")

# Célfüggvény
obj_function <- function(p) {
  
  discount_curve <- mod_discount(as.numeric(rownames(curr_cf_matrix)), 
                                 curr_yield, curr_var,
                                 p[1], p[2], p[3], p[4], p[5], p[6])
  
  p_hat <- colSums(curr_cf_matrix * discount_curve)
  d_hat <- colSums(((curr_cf_matrix * discount_curve) / p_hat) * 
                     as.numeric(rownames(curr_cf_matrix)))
  
  res <- wmae(curr_prices$price, p_hat, 1/d_hat)
  
  if(is.na(res) | is.nan(res)) {
    res <- Inf
  }
  
  return(res)
}

extract_bond_spec_res <- function(p) {
  discount_curve <- mod_discount(as.numeric(rownames(curr_cf_matrix)), 
                                 curr_yield, curr_var,
                                 p[1], p[2], p[3], p[4], p[5], p[6])
  
  p_hat <- colSums(curr_cf_matrix * discount_curve)
  d_hat <- colSums(((curr_cf_matrix * discount_curve) / p_hat) * 
                     as.numeric(rownames(curr_cf_matrix)))
  error <- curr_prices$price - p_hat
  
  res <- cbind(p_hat, d_hat, error)
  res <- data.table(alias = rownames(res), 
                    fitted = res[, "p_hat"], 
                    dur = res[, "d_hat"],
                    err = res[, "error"])
  
  return(res)
}

ccy <- cf_data[, .N, by = list(alias, currency)]
ccy <- merge(best_price, ccy, by = "alias", all.x = T)

# Cash-flow adatok elõkészítése - csak fix kamatozású forintban denominált államkötvények
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

# Ár adatok elõkészítése - csak olyan áradatkiválasztása, ahol van cash-flow adat
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

param_name <- c("alpha", "beta", "gamma", "delta", "eta", "nu")


# Becslési napok meghatározása
dates <- sort(unique(best_price$date))
dates <- dates[dates >= as.Date("2004-01-01") & dates < as.Date("2004-06-30")]

curr_date <- as.Date("2004-04-09")

for (curr_date in dates) {
  
  curr_date <- as.Date(curr_date, origin = "1970-01-01")
  
  cat("Starting date", as.character(curr_date), "\n")
  
  # Next command is to overcome buffered output in RGui
  flush.console()
  
  # Next command simulates a "long" process (taking 1 sec)
  # Sys.sleep(1)
  
  # Szûrés az aktuális piaci árakra
  curr_prices <- prep_prices[date == curr_date]
  curr_prices <- curr_prices[order(alias)]
  
  # Szûrés az aktuálisan jegyzett kötvények cash-flow-ira, cash-flow mátrix elõállítása
  curr_bonds <- merge(prep_cf_data, curr_prices[, list(alias)], by = "alias")
  cf_matrix <- dcast.data.table(curr_bonds[cf_date > curr_date], 
                                cf_date ~ alias, 
                                value.var = "cf_amount", fill = 0)
  cf_matrix[, t := (cf_date - curr_date) / 365]
  
  # Cash-flow mátrix elõállítása - mátrix adattípusban
  curr_cf_matrix <- as.matrix.data.frame(cf_matrix[, !grepl("date|t", names(cf_matrix)), with = F])
  rownames(curr_cf_matrix) <- round(cf_matrix$t, 4)
  curr_cf_matrix <- curr_cf_matrix[, sort(colnames(curr_cf_matrix))]
  
  # Állapot változók jelenlegi értékének meghatározása
  curr_yield <- state_data[date == curr_date & short_name == "yield", value]
  curr_var   <- state_data[date == curr_date & short_name == "var", value]
  
  # Induló paraméterek meghatározása
  curr_start_params <- start_params[target_date == curr_date]
  
  # Paraméterek korlátainak meghatározása
  var_yield_rate <- curr_var / curr_yield
  
  
  # Korlátok meghatározása
  if (curr_start_params[var_name == "alpha", coeff] > curr_start_params[var_name == "beta", coeff]) {
    lower_bound    <- c(var_yield_rate, 0, 0, 0, 0, -10000)
    upper_bound    <- c(10000, var_yield_rate, 10000, 10000, 10000, 10000)
    
  } else {
    lower_bound    <- c(0, var_yield_rate, 0, 0, 0, -10000)
    upper_bound    <- c(var_yield_rate, 10000, 10000, 10000, 10000, 10000)
    
  }
  
  # Kezdõ értékeket tartalmazó populáció elõállítása
  n <- length(lower_bound) 
  
  # Populáció mérete (legalább 10-szeresét érdemes megadni a paraméter vektor hosszának)
  npop <- n * 20
  
  # GMM-mel kapott paramétervektor replikálása a populáció méretére
  init_pop <- matrix(rep(curr_start_params[,coeff], npop), ncol = n, byrow = T)

  # Nulla várható értékû, kis varianciájú zaj hozzáadása 
  noise <- matrix((runif(n * npop) - .5) / 100, ncol = n)
  
  # - ha az eredeti kezdõérték nulla közeli, akkor negatívba mehet
  init_pop <- init_pop + noise
  
  # -> ahol negatívba ment (alsó korlát alá), ott kinullázom
  lb_mat <- matrix(rep(lower_bound, npop), ncol = n, byrow = T)
  init_pop[init_pop < lb_mat] <- 0
  
  # Várható érték változás vizsgálata az eredeti kezdõérték becsléshez képest
  # rbind(apply(init_pop, 2, mean), curr_start_params[,coeff])
  
  # Legjobban illeszkedõ paraméterek keresése DE optimalizálás segítségével
  optimum <- DEoptim(obj_function, lower_bound, upper_bound, 
                     control = list(trace = 100, itermax = 5000, NP = npop, initialpop = init_pop, 
                                    parallelType = 2, 
                                    parVar = c("curr_prices", "curr_cf_matrix",
                                               "curr_yield", "curr_var",
                                               "wmae", "mod_discount",
                                               "A", "B", "C", "D")))
  
  # Becslés specifikus adatok elõállítása
  curr_prices <- merge(curr_prices, extract_bond_spec_res(optimum$optim$bestmem), by = "alias", all.x = T)
  curr_obj_value <- data.table(date = curr_date, obj_value = optimum$optim$bestval)
  curr_est_params <- data.table(date = curr_date, name = param_name, value = optimum$optim$bestmem)
  
  # Hiba, becsült kötvényár, átlagidõ és egyéb kötvényspec adatok tárolása
  if (exists("estim_price_data")) {
    estim_price_data <- rbind(estim_price_data, copy(curr_prices))
    
  } else {
    estim_price_data <- copy(curr_prices)
    
  }
  
  # Becsült, legjobban illeszkedõ paraméterek tárolása
  if (exists("est_params")) {
    est_params <- rbind(est_params, copy(curr_est_params))
    
  } else {
    est_params <- copy(curr_est_params)
    
  }
  
  # Célfüggvény legjobb értékének tárolása
  if (exists("obj_fun_values")) {
    obj_fun_values <- rbind(obj_fun_values, copy(curr_obj_value))
    
  } else {
    obj_fun_values <- copy(curr_obj_value)
    
  }
  
  
}

save(estim_price_data, file = "./Data/estimated_price_database.rdata")
save(est_params, file = "./Data/estimated_parameters_database.rdata")
save(obj_fun_values, file = "./Data/objective_function_values_database.rdata")

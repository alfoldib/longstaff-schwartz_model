## Objective function for DE optimization 
#  (calibrating to observed market data)
obj_function <- function(p) {
  # Calculating the discount curve given the a set of parameters
  discount_curve <- mod_discount(as.numeric(rownames(curr_cf_matrix)), 
                                 curr_yield, curr_var,
                                 p[1], p[2], p[3], p[4], p[5], p[6])
  
  # Calculating bond prices 
  # by discounting cash-flow data with previously calculated discount curve
  p_hat <- colSums(curr_cf_matrix * discount_curve)
  
  # Calculating duration with cash-flow data and prices calculated in previous step
  d_hat <- colSums(((curr_cf_matrix * discount_curve) / p_hat) * 
                     as.numeric(rownames(curr_cf_matrix)))
  
  # Determine pricing error as a weighted mean absolute deviation
  # The weights are the inverse of the duration
  res <- wmae(curr_prices$price, p_hat, 1/d_hat)
  
  # If the result is not interpretable, then replace it with infinity
  # in order not to break the optimization process
  if(is.na(res) | is.nan(res)) {
    res <- Inf
  }
  
  return(res)
}


## Function to calculate the results from a single calibration
extract_bond_spec_res <- function(p) {
  # Calculating the discount curve given the a set of parameters
  discount_curve <- mod_discount(as.numeric(rownames(curr_cf_matrix)), 
                                 curr_yield, curr_var,
                                 p[1], p[2], p[3], p[4], p[5], p[6])
  
  # Calculating bond prices 
  # by discounting cash-flow data with previously calculated discount curve
  p_hat <- colSums(curr_cf_matrix * discount_curve)
  
  # Calculating duration with cash-flow data and prices calculated in previous step
  d_hat <- colSums(((curr_cf_matrix * discount_curve) / p_hat) * 
                     as.numeric(rownames(curr_cf_matrix)))
  
  # Determine the pricing error (market price - fitted price)
  error <- curr_prices$price - p_hat
  
  # Combine the results into a data.table
  res <- cbind(p_hat, d_hat, error)
  res <- data.table(alias = rownames(res), 
                    fitted = res[, "p_hat"], 
                    dur = res[, "d_hat"],
                    err = res[, "error"])
  
  return(res)
  
}
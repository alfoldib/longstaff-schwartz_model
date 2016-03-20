## Objective function for DE optimization 
#  (calibrating to observed market data)
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


## Function to calculate the results from a single calibration
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
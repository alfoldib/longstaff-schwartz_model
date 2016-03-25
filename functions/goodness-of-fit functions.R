## Support / Goodness-of-fit functions

# Mean Absolute Error
mae <- function(actual, estimate) {
  mae <- mean(abs(actual - estimate))
  mae
}

# Root Mean Squared Error
rmse <- function(actual, estimate) {
  rmse <- sqrt(mean((actual - estimate)^2))
  rmse
}

# Weighted Mean Absolute Error
wmae <- function(actual, estimate, weight) {
  wmae <- sum(weight * abs(actual - estimate)) / sum(weight)
  wmae
}
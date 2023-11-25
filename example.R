

# Your function U
U <- function(X, R) {
  # Your logic for the function U
  # Replace the following line with your actual function
  result <- (X - 2)^2 + R * sin(X)
  return(result)
}

# Wrapper function for optimization with a fixed value of R
optimize_X <- function(X, R) {
  return(U(X, R))
}

# Values of R for which you want to find the optimum of X
R_values <- c(1, 2, 3, 4)

# Loop over different values of R
optimal_X_values <- sapply(R_values, function(R) {
  result <- optim(par = initial_guess_for_X, fn = optimize_X, R = R)
  return(result$par)
})

# Print the optimal values of X for each value of R
print(optimal_X_values)
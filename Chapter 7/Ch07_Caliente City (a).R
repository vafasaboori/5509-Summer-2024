# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Solve BIP Problem ----

# Define the response time matrix
response_time <- matrix(c(  2,  8, 18,  9, 23, 22, 16, 28,
                            9,  3, 10, 12, 16, 14, 21, 25,
                           17,  8,  4, 20, 21,  8, 22, 17,
                           10, 13, 19,  2, 18, 21,  6, 12,
                           21, 12, 16, 13,  5, 11,  9, 12,
                           25, 15,  7, 21, 15,  3, 14,  8,
                           14, 22, 18,  7, 13, 15,  2,  9,
                           30, 24, 15, 14, 17,  9,  8,  3), nrow = 8, byrow = TRUE)

# Define the costs (in $1000s)
costs <- c(350, 250, 450, 300, 50, 400, 300, 200)

# Generate constraints matrix
constraints <- matrix(0, nrow = 8, ncol = 8)
for (i in 1:8) {
  for (j in 1:8) {
    if (response_time[i, j] <= 10) {
      constraints[i, j] = 1
    }
  }
}

# Set inequality signs
inequality_signs <- rep(">=", 8)

# Set right hand side parameters
rhs <- rep(1, 8)

# Solve the optimization problem
result <- lp("min", costs, constraints, inequality_signs, rhs, all.bin = TRUE)

# Extract the solution
solution <- result$solution
solution

# Extract the value of the objective function
obj_value <- result$objval
obj_value

library(scales) # This package is used to format numbers as currency.
?scales

cat("Objective function value (Total Cost):", dollar(obj_value*1000), "\n")
# The "dollar" function from the scales package is used to format as currency

# Print the optimized solution and objective function value
cat("Optimized solution:\n")
print(solution)

# Identify which tracts need fire stations
tracts <- which(solution == 1)

cat("Fire stations need to be built in the following tracts:\n")
print(tracts)

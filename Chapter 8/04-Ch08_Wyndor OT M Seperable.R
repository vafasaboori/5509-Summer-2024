# Clear the environment
rm(list = ls())

# Load the lpSolve package
library(lpSolve)

# Define the objective function coefficients
obj <- c(350, 300, 250, 100, 633.33, 500, 366.67, -300)
# Maximize P = 350D1 + 300D2 +250D3 + 100 DT + 633.33 W1 + 500W2 +366.67W3 -300WT 

# Define the constraint matrix
const.mat <- matrix(c(
  1, 0, 0, 0, 0, 0, 0, 0,  # D1 <= 1
  0, 1, 0, 0, 0, 0, 0, 0,  # D2 <= 1
  0, 0, 1, 0, 0, 0, 0, 0,  # D3 <= 1
  0, 0, 0, 1, 0, 0, 0, 0,  # DT <= 1 Overtime
  0, 0, 0, 0, 1, 0, 0, 0,  # W1 <= 1
  0, 0, 0, 0, 0, 1, 0, 0,  # W2 <= 1
  0, 0, 0, 0, 0, 0, 1, 0,  # W3 <= 1
  0, 0, 0, 0, 0, 0, 0, 1,  # WT <= 3 Overtime
  1, 1, 1, 1, 0, 0, 0, 0,  # D1 + D2 + D3 + DT <= 4
  0, 0, 0, 0, 1, 1, 1, 1,  # W1 + W2 + W3 + WT <= 12
  3, 3, 3, 3, 2, 2, 2, 2   # 3(D1 + D2 + D3 + DT) + 2(W1 + W2 + W3 + WT) <= 18
), ncol = 8, byrow = TRUE)

# Define the constraint directions
const.dir <- rep("<=", 11)

# Define the right-hand side of the constraints
const.rhs <- c(1, 1, 1, 1, 1, 1, 1, 3, 4, 12, 18)

# Solve the linear programming problem
lp <- lp("max", obj, const.mat, const.dir, const.rhs)

# Print the results
cat("Objective value:", lp$objval, "\n")
cat("Solution:")
print(lp$solution)


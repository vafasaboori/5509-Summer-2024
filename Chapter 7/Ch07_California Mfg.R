# Clearing the Environment ----
rm(list=ls())

# Import lp function from lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# X1: Factory in LA
# X2: Factory in SF
# X3: Warehouse in LA
# X4: Warehouse in SF
# Max NPV = 8X1 + 5X2 + 6X3 + 4X4 in Millions (Objective Function)
# Subject to (Constraints)
# 6X1 + 3X2 + 5X3 + 2X4 <= 10 (Capital Available in Millions)
#              X3 +  X4 <= 1  (Mutually Exclusive: Only One Warehouse)
# -X1       +  X3       <= 0  (Contingent: X3 <= X1 Warehouse if Factory in LA)
#       -X2       +  X4 <= 0  (Contingent: X4 <= X2 Warehouse if Factory in SF)
# X1, X2, X3, X4 = Binary Integer

# Solving BIP model ----
# Set coefficients of the objective function
f.obj <- c(8, 5, 6, 4)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(6,  3,  5,  2,
                  0,  0,  1,  1,
                 -1,  0,  1,  0,
                  0, -1,  0,  1), nrow = 4, byrow = TRUE)

# Set inequality signs
f.dir <- rep("<=",4)

# Set right-hand side parameters
f.par <- c(10, 1, 0, 0)

# Solve the optimization problem and store the result
result <- lp("max", f.obj, f.con, f.dir, f.par, all.bin = TRUE) 
# or binary.vec = 1:4 or binary.vec = c(1,2) if e.g. only two variables are binary

# Variables final values
solution <- result$solution

# Objective function value
objective_value <- result$objval

# Print the final result
cat("Optimized solution:\n")
print(solution)

cat("\n Objective function value:", objective_value, "Millions")

# In class exercise: Sensitivity Analysis ----
# Perform parameter analysis by varying the capital available
# from 5 million to 15 million in 1 million increments
# Use a loop to iterate over the different values (while)
# Solve the optimization problem for each value.

# Solution to in class exercise: Sensitivity Analysis ----

# Initialize data frame to store results
results_df <- data.frame(
  CapitalAvailable = numeric(),
  X1 = numeric(),
  X2 = numeric(),
  X3 = numeric(),
  X4 = numeric(),
  ObjectiveValue = numeric()
)

# Initial capital available
capital_available <- 5

# Increment for capital available
increment <- 1

# Maximum capital available
max_capital_available <- 15

# Loop for sensitivity analysis
while (capital_available <= max_capital_available) { 
  # Set right-hand side parameters
  f.par <- c(capital_available, 1, 0, 0)
  
  # Solve the optimization problem and store the result
  result <- lp("max", f.obj, f.con, f.dir, f.par, all.bin = TRUE)
  
  # Variables final values
  solution <- result$solution
  
  # Objective function value
  objective_value <- result$objval
  
  # Store the results in the data frame
  results_df <- rbind(results_df, data.frame(
    CapitalAvailable = capital_available,
    X1 = solution[1],
    X2 = solution[2],
    X3 = solution[3],
    X4 = solution[4],
    ObjectiveValue = objective_value
  ))
  
  # Increment capital available
  capital_available <- capital_available + increment
}

# Print the results data frame
print(results_df)


# Clearing the Environment
rm(list=ls())

# Import lpSolve package
library(lpSolve)

# Algebraic Model
# X1: Project Up
# X2: Project Stable
# X3: Project Choice
# X4: Project Hope
# X5: Project Release
# Max Expected Profit = 300X1 + 120X2 + 170X3 + 100X4 + 70X5 in Millions (Objective Function)
# Subject to (Constraints)
# 400X1 + 300X2 + 600X3 + 500X4 + 200X5 <= 1200 (Budget in Millions)
# X1, X2, X3, X4, X5 = Binary Integer

# Set coefficients of the objective function
f.obj <- c(300, 120, 170, 100, 70)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(400, 300, 600, 500, 200), nrow = 1, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- c("<=")

# Set right hand side parameters
f.par <- c(1200)

# Final value (p)
result <- lp("max", f.obj, f.con, f.dir, f.par, all.bin = TRUE)

# Extract the solution and objective function value
solution <- result$solution
objective_value <- result$objval

# Print the solution and objective function value
cat("Optimized solution:\n", solution)
cat("Objective function value:", objective_value, "Millions\n")

# Sensitivity Analysis (In class workshop) ----
 # What happens if the budget changes from 800M to 1600M in 100M increments?

# Solution to the sensitivity analysis ----

# Initialize budget
budget <- 800
max_budget <- 1600
increment <- 100

# Initialize data frame to store results
results_df <- data.frame(
  Budget = numeric(),
  X1 = numeric(),
  X2 = numeric(),
  X3 = numeric(),
  X4 = numeric(),
  X5 = numeric(),
  ObjectiveValue = numeric()
)

# Perform sensitivity analysis
while (budget <= max_budget) {
  # Set right-hand side parameter
  f.par <- budget
  
  # Solve the optimization problem
  result <- lp("max", f.obj, f.con, f.dir, f.par, all.bin = TRUE)
  
  # Extract the solution and objective function value
  solution <- result$solution
  objective_value <- result$objval
  
  # Store the results in the data frame
  results_df <- rbind(results_df, data.frame(
    Budget = budget,
    X1 = solution[1],
    X2 = solution[2],
    X3 = solution[3],
    X4 = solution[4],
    X5 = solution[5],
    ObjectiveValue = objective_value
  ))
  
  # Increment the budget
  budget <- budget + increment
}

# Print the results data frame
print(results_df)


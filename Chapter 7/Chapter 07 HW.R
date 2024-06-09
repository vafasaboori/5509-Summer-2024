# Question 5 ----

# Clearing the Environment
rm(list=ls())

# Load the lpSolve package
library(lpSolve)

# Define the objective function coefficients
objective_coef <- c(1, 1.8, 1.6, 0.8, 1.4)

# Define the constraint matrix and the right-hand side vector
constraint_mat <- matrix(c(6, 12, 10, 4, 8), nrow = 1)
constraint_rhs <- c(20)

# Create an empty matrix to store the results
results <- matrix(0, nrow = 8, ncol = 7)
colnames(results) <- c("Investment Capital", "x1", "x2", "x3", "x4", "x5", "Objective Value")

# Perform the what-if analysis for different amounts of investment capital
investment_capital <- c(16, 18, 20, 22, 24, 26, 28, 30)

for (i in 1:length(investment_capital)) {
  # Set the right-hand side of the constraint to the current investment capital value
  constraint_rhs <- investment_capital[i]
  
  # Solve the linear programming problem
  lp_solution <- lp(direction = "max",
                    objective.in = objective_coef,
                    const.mat = constraint_mat,
                    const.dir = "<=",
                    const.rhs = constraint_rhs,
                    all.bin = TRUE)
  
  # Store the results in the matrix
  results[i, 1] <- investment_capital[i]
  results[i, 2:6] <- lp_solution$solution
  results[i, 7] <- lp_solution$objval
}

# Print the parameter analysis report
print(results)


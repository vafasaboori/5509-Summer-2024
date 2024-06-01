# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Min C = 1TV + 2PM in Millions (Objective Function)
# Subject to (Constraints)
#  0TV + 1PM >= 3
#  3TV + 2PM >= 18
# -1TV + 4PM >= 4
# TV >= 0
# PM >= 0

# Original Problem ----
# Set coefficients of the objective function
f.obj <- c(1, 2)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(0, 1,
                  3, 2,
                 -1, 4), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- c(">=",
           ">=",
           ">=")

# Set right hand side parameters
f.par <- c(3,
           18,
           4)

# Solve the linear programming problem
lp_result <- lp("min", f.obj, f.con, f.dir, f.par)

# Print the final value (p)
lp_result

# Print variable final values
lp_result$solution


# Sensitivity Analysis ----

# Perform sensitivity analysis 
sens <- lp("min", f.obj, f.con, f.dir, f.par, compute.sens = TRUE)

# Perform sensitivity analysis for objective function coefficients
round(sens$sens.coef.from, 3)
round(sens$sens.coef.to, 3)

# Perform sensitivity analysis for shadow prices
round(sens$duals, 3)

# Sensitivity Analysis (Allowable RHS, constraints first, variables next)
options(scipen = 0, digits = 3)
round(sens$duals.from, 3)
round(sens$duals.to, 3)

# Additional Steps (Robust) In Class Exercise (Liquid Detergent) ----
# What if the estimates for increase in sales of liquid detergent are uncertain? ----
# range of uncertainty for increase in sales per TV is uniform (2.5–3.5)
# range of uncertainty for increase in sales per Print Media is uniform (1.5–2.5)

# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
results <- vector("list", num_simulations)
# vector initialized as a list, i.e. each element can be any type of object
# This vector store the solutions from each simulation run of LP.

c_values <- numeric(num_simulations)
# vector initialized with all elements set to 0.
# This vector store the obj fun values (cost) from each simulation run of LP.

# Run simulations
for (i in 1:num_simulations) {
  # Simulate uncertain parameters
  TV <- runif(1, min = 2.5, max = 3.5)
  Print <- runif(1, min = 1.5, max = 2.5)
  
  # Update the constraint matrix with the simulated parameters
  f.con[2, 1] <- TV
  f.con[2, 2] <- Print
  
  # Solve the linear program
  lp_result <- lp("min", f.obj, f.con, f.dir, f.par)
  
  # Store the results
  results[[i]] <- lp_result$solution
  # double brackets [[ ]] for extracting or assigning values to a element in a list
  
  c_values[i] <- lp_result$objval
  # single brackets [ ] for extracting or assigning values to subsets of a list.
}

# Find the optimal solution based on the simulated results
best_solution <- results[[which.min(c_values)]]
# which.max() returns the index of the maximum value in the vector p_values
# It finds the position of the element with the highest value.

best_p <- min(c_values)

# Print the optimal solution
cat("Optimal Solution:\n")
cat("TV =", best_solution[1], "\n")
cat("Print =", best_solution[2], "\n")
cat("Cost =", best_p, "\n")


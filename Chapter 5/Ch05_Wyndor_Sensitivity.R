# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Original Problem ----
# Algebraic Model
# Max P = 300D + 500W (Objective Function)
# Subject to (Constraints)
# 1D + 0W <= 4
# 0D + 2W <= 12
# 3D + 2W <= 18
# D >= 0
# W >= 0

# Set coefficients of the objective function
f.obj <- c(300, 500)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1, 0,
                  0, 2,
                  3, 2), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- c("<=",
           "<=",
           "<=")

# Set right hand side parameters
f.par <- c(4,
           12,
           18)

# Final value (p)
lp_result <- lp("max", f.obj, f.con, f.dir, f.par)

# Final value (p)
lp_result

# Variables final values
lp_result$solution

# Q1: What if the unit profit of one of Wyndor’s new products is inaccurate? ----
  
# Sensitivity Analysis (Objective Function Coefficients)
sensitivity_analysis <- lp("max", f.obj, f.con, f.dir, f.par, compute.sens = TRUE)

sensitivity_analysis$sens.coef.from # Lower bounds of sensitivity ranges
sensitivity_analysis$sens.coef.to # Upper bounds of sensitivity ranges

# disabling scientific notation: scientific penalty unless wider than 999 digits.
options(scipen =1)

# Reduced costs
sensitivity_analysis$duals[4:5]

# Q2: What if the unit profits of both of Wyndor’s new products are inaccurate? ----
  # Optional: Try to implement 100% rule in R (try to automate the process)

# Q3: What if available hours (Constraint RHS) changes in one of the plants? ----

# Sensitivity Analysis (Shadow Prices, constraints first, variables next)
sensitivity_analysis$duals

# Sensitivity Analysis (Allowable RHS, constraints first, variables next)
sensitivity_analysis$duals.from
sensitivity_analysis$duals.to

# Q4: What if available hours (Constraint RHS) changes in one of the plants? ----
# Optional: Try to implement 100% rule in R (try to automate the process)

# Q5: What if the production rates of doors and windows at plant 3 are uncertain? ----
  # range of uncertainty for hours required per door is uniform (2.5–3.5)
  # range of uncertainty for the hours required per window is uniform (1.5–2.5)

# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
results <- vector("list", num_simulations)
  # vector initialized as a list, i.e. each element can be any type of object
  # This vector store the solutions from each simulation run of LP.
  # Remember! results is a list where each element is a vector.

p_values <- numeric(num_simulations)
  # vector initialized with all elements set to 0.
  # This vector store the obj fun values (profit) from each simulation run of LP.

# Run simulations
for (i in 1:num_simulations) {
  # Simulate uncertain parameters
  hours_door <- runif(1, min = 2.5, max = 3.5) 
  hours_window <- runif(1, min = 1.5, max = 2.5)
  # runif generates random numbers from a uniform distribution.
  # The first argument, 1, specifies the number of random numbers to generate.
  
  # Update the constraint matrix with the simulated parameters
  f.con[3, 1] <- hours_door
  f.con[3, 2] <- hours_window
  
  # Print simulated values for hours required for doors and windows
  print(paste("Simulated hours_door:", hours_door, "hours_window:", hours_window))
  
  # Solve the linear program
  lp_result <- lp("max", f.obj, f.con, f.dir, f.par)
  
  # Store the results
  results[[i]] <- lp_result$solution
  # Double brackets [[ ]] are used specifically to access single elements within a list.
  # Remember! results is a list where each element is a vector.
  
  p_values[i] <- lp_result$objval
  # single brackets [ ] for extracting or assigning values to subsets of a list.
}

# Find the optimal solution based on the simulated results
best_solution <- results[[which.max(p_values)]]
# which.max() returns the index of the maximum value in the vector p_values
# It finds the position of the element with the highest value.

best_p <- max(p_values)

# Print the optimal solution
cat("Optimal Solution:\n")
cat("D =", best_solution[1], "\n")
cat("W =", best_solution[2], "\n")
cat("P =", best_p, "\n")

# Revised Q5
    # What if the production rates of doors and windows at plant 3 are uncertain? ----
    # range of uncertainty for hours required per door is uniform (2.5–3.5)
    # range of uncertainty for the hours required per window is uniform (1.5–2.5)
# 3rd constraint rhs (18) be satisfied at least 95% of the time (chance constraint).

# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
results <- vector("list", num_simulations)
p_values <- numeric(num_simulations)

# Run simulations
for (i in 1:num_simulations) {
  # Simulate uncertain parameters
  hours_door <- runif(1, min = 2.5, max = 3.5) 
  hours_window <- runif(1, min = 1.5, max = 2.5)
  
  # Decide whether to apply the original or relaxed constraint
  if (runif(1) <= 0.95) { # runif(1) generates one random number between 0 an 1
    # Apply the original constraint 95% of the time
    f.par[3] <- 18
  } else {
    # Violate the constraint 5% of the time
    f.par[3] <- runif(1, min = 19, max = 100)  # Simulate a value above the original RHS
  }
  
  # Print simulated values for hours required for doors, windows, and RHS
  print(paste("Simulated hours_door:", hours_door, "hours_window:", hours_window, "Constraint RHS:", f.par[3]))
  
  # Update the constraint matrix with the simulated parameters
  f.con[3, 1] <- hours_door
  f.con[3, 2] <- hours_window
  
  # Solve the linear program
  lp_result <- lp("max", f.obj, f.con, f.dir, f.par)
  
  # Store the results
  results[[i]] <- lp_result$solution
  p_values[i] <- lp_result$objval
}

# Find the optimal solution based on the simulated results
best_solution <- results[[which.max(p_values)]]
best_p <- max(p_values)

# Print the optimal solution
cat("Optimal Solution:\n")
cat("D =", best_solution[1], "\n")
cat("W =", best_solution[2], "\n")
cat("P =", best_p, "\n")

# In class Assignment: Normal Distribution -----
# hours required per door is normal (mean=3.0, sd=0.5)
# hours required per window is uniform (mean=2.0, sd=0.5)

# Solution for In Class Exercise ----

# In class Assignment: Normal Distribution -----
# hours required per door is normal (mean=3.0, sd=0.5)
# hours required per window is uniform (mean=2.0, sd=0.5)

# Solution for In Class Exercise ----

# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
results <- vector("list", num_simulations)
p_values <- numeric(num_simulations)

# Run simulations
for (i in 1:num_simulations) {
  # Simulate uncertain parameters with normal distribution
  hours_door <- rnorm(1, mean = 3.0, sd = 0.5)
  hours_window <- rnorm(1, mean = 2.0, sd = 0.5) # Adjusted to be within a reasonable range
  
  # Decide whether to apply the original or relaxed constraint for the third constraint
  if (runif(1) <= 0.95) {
    # Apply the original constraint 95% of the time
    f.par[3] <- 18
  } else {
    # Relax the constraint 5% of the time
    f.par[3] <- runif(1, min = 19, max = 100)  # Simulate a wider range for the RHS
  }
  
# Print simulated values for hours required for doors, windows, and RHS
print(paste("Simulated hours_door:", hours_door, "hours_window:", hours_window, "Constraint RHS:", f.par[3]))
  
  
  # Update the constraint matrix with the simulated parameters
  f.con[3, 1] <- hours_door
  f.con[3, 2] <- hours_window
  
  # Solve the linear program
  lp_result <- lp("max", f.obj, f.con, f.dir, f.par)
  
  # Store the results
  results[[i]] <- lp_result$solution
  p_values[i] <- lp_result$objval
  
  # Print simulated values for hours required for doors and windows
  cat("Simulation", i, ": Hours required for doors =", hours_door, ", Hours required for windows =", hours_window, "\n")
}

# Find the optimal solution based on the simulated results
best_solution <- results[[which.max(p_values)]]
best_p <- max(p_values)

# Print the optimal solution
cat("Optimal Solution:\n")
cat("D =", best_solution[1], "\n")
cat("W =", best_solution[2], "\n")
cat("P =", best_p, "\n")


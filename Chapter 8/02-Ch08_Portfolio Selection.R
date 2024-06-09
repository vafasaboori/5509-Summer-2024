# Clear the environment
rm(list = ls())

# Load nloptr package ----
library(nloptr)

# Algebraic Model -----
# Objective function:
# Risk = (0.25S1)^2 + (0.45S2)^2 + (0.05S3)^2 + 2(0.04)S1S2 + 2*(-0.005)S1S3 + 2*(-0.01)S2S3

# Constraints:
# 21S1 + 30S2 + 8*S3 >= 18
# S1 + S2 + S3 == 1
# S1 >= 0, S2 >= 0, S3 >= 0

# General form of NLP
# Min f(x)          objective function (always min)
# such that
# g(x) <= 0       inequality constraints (always <= 0)
# h(x) = 0        equality constraints
# xL <= x <= xU   lower and upper bounds

# Solve NLP ----

# Define the objective function
objective <- function(x) {
  return((0.25 * x[1])^2 + (0.45 * x[2])^2 + (0.05 * x[3])^2 
         + 2 * (0.04)   * x[1] * x[2] 
         + 2 * (-0.005) * x[1] * x[3] 
         + 2 * (-0.01)  * x[2] * x[3])
}

# Define the inequality constraints
ineq_constraints <- function(x) {
  return(c(18 - (21 * x[1] + 30 * x[2] + 8 * x[3]),   # 21S1 + 30S2 + 8*S3 >= 18
           x[1] + x[2] + x[3] - 1 - 1e-6,
         - x[1] - x[2] - x[3] + 1 + 1e-6)) # workaround for equality constraints (Note*)
}
# Note*: workaround: formulate S1 + S2 + S3 = 1 as two inequality constraints:
# S1 + S2 + S3 ≤ 1 + ε or:   S1 + S2 + S3 - 1 - ε ≤ 0
# S1 + S2 + S3 ≥ 1 - ε or: - S1 - S2 - S3 + 1 + ε ≤ 0

# Define the bounds for variables
lower_bounds <- c(0, 0, 0)
upper_bounds <- c(1, 1, 1)

# Set the optimization options
opts <- list("algorithm" = "NLOPT_LN_COBYLA", # COBYLA algorithm for nonlinear programming
             "xtol_rel" = 1.0e-8) # how close to the optimal solution before terminating (conveyance tolerance)

# Solve the nonlinear programming problem
sol <- nloptr(x0 = c(0, 0, 0), # initial values of S1, S2 and S3
              eval_f = objective, # obj fun to be evaluated during the optimization process
               lb = lower_bounds,
              ub = upper_bounds, # lower and upper bounds
              eval_g_ineq = ineq_constraints, # inequality constraints to be evaluated
              opts = opts) # additional options for the optimization algorithm

# Extract the solution
solution <- sol$solution
objfun <- sol$objective

# Convert the solution to percentages with 2 decimal places
solution_percentages <- paste0(round(solution * 100, 2), "%")

# Format the objective value (Variance) as a percentage with 2 decimal places
objective_var_percentage <- paste0(round((objfun) * 100, 2), "%")

# Format the objective value Std. Dev. (risk) and as a percentage with 2 decimal places
objective_std_percentage <- paste0(round(sqrt(objfun) * 100, 2), "%")

# Print the solution (percentages) and objective value (percentage)
cat("Solution (Percentages):", solution_percentages)
cat("Objective Value (Variance):", objective_var_percentage)
cat("Objective Value (Standard Deviation):", objective_std_percentage)

# Perform parameter analysis -----
min <- 10 # min expected return
max <- 30 # max expected return

# Initialize a data frame to store results
results <- data.frame(
  `Minimum Expected Return` = integer(),
  `Stock1` = numeric(),
  `Stock2` = numeric(),
  `Stock3` = numeric(),
  `Variance` = character(),
  `Standard Deviation` = character(),
  `Return` = numeric()
)

for (i in seq(min, max, by = 2)) {
  # Define the modified inequality constraints
  modified_ineq_constraints <- function(x) {
    return(c(i - (21 * x[1] + 30 * x[2] + 8 * x[3]),
             x[1] + x[2] + x[3] - 1 + 1e-6,
            -x[1] - x[2] - x[3] + 1 + 1e-6))
  }
  
  # Solve the modified nonlinear programming problem
  sol <- nloptr(x0 = c(0, 0, 0),
                eval_f = objective,
                lb = lower_bounds,
                ub = upper_bounds,
                eval_g_ineq = modified_ineq_constraints,
                opts = opts)
  
  # Extract the solution
  solution <- sol$solution
  objfun <- sol$objective
  
  # Append the results to the data frame
  results <- rbind(
    results,
    data.frame(
      `Minimum Expected Return` = i,
      `Stock1` = round(solution[1] * 100, 2),
      `Stock2` = round(solution[2] * 100, 2),
      `Stock3` = round(solution[3] * 100, 2),
      `Variance` = paste0(round((objfun) * 100, 2), "%"),
      `Standard Deviation` = paste0(round(sqrt(objfun) * 100, 2), "%"),
      `Return` = i
    )
  )
}

# Print the results as a table
print(results)


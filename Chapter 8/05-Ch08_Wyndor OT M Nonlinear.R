# Clearing the Environment ----
rm(list=ls())

library(nloptr)

# Define the objective function
objective <- function(x) {
  DR <- x[1]
  DT <- x[2]
  WR <- x[3]
  WT <- x[4]
  
  P <- 375 * DR + 275 * DT - 25 * (DR + DT)^2 + 700 * WR + 300 * WT - 66.667 * (WR + WT)^2
  return(-P)  # We maximize, so we negate the objective function
}

# Define the constraints
ineq_constraint <- function(x) {
  DR <- x[1]
  DT <- x[2]
  WR <- x[3]
  WT <- x[4]
  
  constraints <- c(
    DR - 3,   # DR <= 3
    DT - 1,   # DT <= 1
    WR - 3,   # WR <= 3
    WT - 3,   # WT <= 3
    DR + DT - 4,   # DR + DT <= 4
    2 * (WR + WT) - 12,   # 2(WR + WT) <= 12
    3 * (DR + DT) + 2 * (WR + WT) - 18   # 3(DR + DT) + 2(WR + WT) <= 18
  )
  
  return(constraints)
}

# Define the lower and upper bounds for variables
lb <- c(0, 0, 0, 0)  # Lower bounds: DR >= 0, DT >= 0, WR >= 0, WT >= 0
ub <- c(Inf, Inf, 3, 3)  # Upper bounds: DR <= Inf, DT <= Inf, WR <= 3, WT <= 3

# Solve the optimization problem
result <- nloptr(
  x0 = c(0, 0, 0, 0),   # Initial guess for variables
  eval_f = objective,   # Objective function
  lb = lb,              # Lower bounds
  ub = ub,              # Upper bounds
  eval_g_ineq = ineq_constraint,  # Inequality constraints
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 0.001)  
  # Use the COBYLA algorithm with absolute x-tolerance of 0.001
)

# Extract the optimal solution and objective value
optimal_solution <- result$solution
optimal_objective <- -result$objective  # Negate the objective value back to positive

# Print the optimal solution and objective value
cat("Optimal Solution:\n")
cat("DR =", optimal_solution[1], "\n")
cat("DT =", optimal_solution[2], "\n")
cat("WR =", optimal_solution[3], "\n")
cat("WT =", optimal_solution[4], "\n")
cat("Optimal Objective Value: P =", optimal_objective, "\n")


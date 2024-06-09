# Clearing the Environment ----
rm(list=ls())

# Load nloptr package ----
library(nloptr) # Nonlinear Optimization in R

# Algebraic Model -----
# Maximize Profit = $375(D) − $25(D^2) + $700(W) − $66.667(W^2)
# subject to 
  # D ≤ 4 
  # W ≤ 6 
  # 3D + 2W ≤ 18
# and
  # D≥ 0 W ≥ 0

# General form of NLP
  # Min f(x)          objective function (always min)
  # such that
    # g(x) <= 0       inequality constraints (always <= 0)
    # h(x) = 0        equality constraints
    # xL <= x <= xU   lower and upper bounds

# Solve NLP ----
# Objective function
obj_fun <- function(x) {
  D <- x[1]
  W <- x[2]
  Profit <- 375 * D - 25 * D^2 + 700 * W - 66.667 * W^2
  return(-Profit)  # Negative for maximization (nloptr designed for min by default)
  }

# Inequality constraint function
ineq_fun <- function(x) {
  D <- x[1]
  W <- x[2]
  constraints <- c(D - 4, # D <= 4
                   W - 6, # 2W <= 12
                   3 * D + 2 * W - 18 # 3D + 2W <=18
                   ) # inequality constraints (always <= 0)
  return(constraints)
}

# Bounds on decision variables
lb <- c(0, 0)  # Lower bounds
ub <- c(4, 6)  # Upper bounds (somewhat redundant)

# Set options for termination criterion
opts <- list("algorithm" = "NLOPT_LN_COBYLA", # COBYLA algorithm for nonlinear programming
             "xtol_rel" = 1e-06) # how close to the optimal solution before terminating (conveyance tolerance)

# Solve the optimization problem
result <- nloptr(x0 = c(0, 0), # initial values of D and W
                 eval_f = obj_fun, # obj fun to be evaluated during the optimization process
                 lb = lb,  
                 ub = ub, # lower and upper bounds
                 eval_g_ineq = ineq_fun, # inequality constraints to be evaluated
                 opts = opts) # additional options for the optimization algorithm

# Extract the optimal solution
solution <- result$solution

# Print the optimal solution -----
cat("Optimal Solution:\n")
cat("D =", solution[1], "\n")
cat("W =", solution[2], "\n")
cat("Profit =", -result$objective, "\n")


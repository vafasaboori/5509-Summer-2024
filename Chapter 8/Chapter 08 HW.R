# Clearing the Environment ----
rm(list=ls())

# Load nloptr package ----
library(nloptr) # Nonlinear Optimization in R

# Algebraic Model -----
# Maximize Profit = $200R1 – $100R1^2 + $300R2 – $100R2^2
# subject to	R1 + R2 ≤ 2  (maximum total production rate)
# and	R1 ≥ 0, R2 ≥ 0. 


# General form of NLP
  # Min f(x)          objective function (always min)
  # such that
    # g(x) <= 0       inequality constraints (always <= 0)
    # h(x) = 0        equality constraints
    # xL <= x <= xU   lower and upper bounds

# Solve NLP ----
# Objective function
obj_fun <- function(x) {
  R1 <- x[1]
  R2 <- x[2]
  Profit <- 200 * R1 - 100 * R1^2 + 300 * R2 - 100 * R2^2
  return(-Profit)  # Negative for maximization (nloptr designed for min by default)
  }

# Inequality constraint function
ineq_fun <- function(x) {
  R1 <- x[1]
  R2 <- x[2]
  constraints <- R1 + R2 - 2 # R1 + R2 ≤ 2
                   # inequality constraints (always <= 0)
  return(constraints)
}

# Bounds on decision variables
lb <- c(0, 0)  # Lower bounds
ub <- c(2, 2)  # Upper bounds (somewhat redundant)

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
cat("R1 =", solution[1], "\n")
cat("R2 =", solution[2], "\n")
cat("Profit =", -result$objective, "\n")


# part c: Confirm that the model is QP Convex.

# Define the function for Profit
profit_function <- function(R1, R2) {
  200 * R1 - 100 * R1^2 + 300 * R2 - 100 * R2^2
}

# Generate data points for R1 and R2
R1 <- seq(0, 2, length.out = 100)
R2 <- seq(0, 2, length.out = 100)
grid <- expand.grid(R1 = R1, R2 = R2) 
# Creates a data frame (grid) for all possible combinations of R1 and R2.


# Calculate the values of Profit for the grid points
grid$Profit <- profit_function(grid$R1, grid$R2)
# Adds a new column to grid with result of profit_function for R1 and R2.

# Convert the grid data to a matrix for plotting
Z <- matrix(grid$Profit, nrow = length(R1), ncol = length(R2))

# Create 3D surface plot
persp(x = R1, y = R2, z = Z, theta = 30, phi = 30, expand = .5, # create 3d plots
      # theta (Horizontal Viewing Angle)
      # phi (Vertical Viewing Angle)
      # expand (Plot Scaling Factor)
      col = "lightblue", 
      ticktype = "detailed", # provides detailed tick marks
      xlab = "R1", ylab = "R2", zlab = "Profit",
      main = "3D Surface Plot of Profit Function")


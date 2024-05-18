# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Min C = 1TV + 2PM in Millions (Objective Function)
# Subject to (Constraints)
# 0TV + 1PM >= 3
# 3TV + 2PM >= 18
# -1TV + 4PM >= 4
# TV >= 0
# PM >= 0

# Set coefficients of the objective function ---
f.obj <- c(1, 2)

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(0, 1,
                  3, 2,
                  -1, 4), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs ----
f.dir <- c(">=",
           ">=",
           ">=")

# Set right hand side parameters ----
f.par <- c(3,
           18,
           4)

# Solve the linear programming problem ----
result <- lp("min", f.obj, f.con, f.dir, f.par)

# Print the final objective value ----
Cost <- result$objval
Cost

# Print the final variable values ----
variables <- result$solution
variables


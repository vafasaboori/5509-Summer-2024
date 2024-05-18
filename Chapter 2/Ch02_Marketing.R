# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Min Cost = D + 1.5E (Objective Function)
# Subject to (Constraints)
# 0.10D + 0.20E >= 100
# 0.15D + 0.30E >= 150
# 0.20D + 0.20E >= 120
# 0.35D + 0.25E >= 200
# E<= 1/3(D+E) or  -1/3D + 2/3E <=0 or  -0.33D  0.67E <=0
# D >= 0
# E >= 0

# Set coefficients of the objective function ----
f.obj <- c(1, 1.5)

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(0.10, 0.20,
                  0.15, 0.30,
                  0.20, 0.20,
                  0.35, 0.25,
                  -0.3333, 0.6666), nrow = 5, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs ----
f.dir <- c(">=",
           ">=",
           ">=",
           ">=",
           "<=")

# Set right hand side parameters ----
f.par <- c(100,
           150,
           120,
           200,
           0)

# Solve the linear programming problem ----
result <- lp("min", f.obj, f.con, f.dir, f.par)

# Print the final objective value ----
Cost <- result$objval
Cost

# Print the final variable values -----
variables <- result$solution
variables


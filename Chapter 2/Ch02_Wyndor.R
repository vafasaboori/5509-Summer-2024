# Clearing the Environment ---- 
rm(list=ls())
# foldable code regions Collapse: cmd + option + o
# foldable code regions Expand: Shift + cmd + option + o

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Max P = 300D + 500W (Objective Function)
# Subject to (Constraints)
# 1D + 0W <= 4
# 0D + 2W <= 12
# 3D + 2W <= 18
# D >= 0
# W >= 0

# Set coefficients of the objective function ----
f.obj <- c(300, 500)

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1, 0, 
                  0, 2, 
                  3, 2), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs ----
f.dir <- c("<=",
           "<=",
           "<=")

# Set right hand side parameters ----
f.par <- c(4,
           12,
           18)

# Solve the linear programming problem ----
result <- lp("max", f.obj, f.con, f.dir, f.par)
result

# Print the final objective value ----
p <- result$objval
print(p)

# Print the final variable values ----
variables <- result$solution
print(variables)

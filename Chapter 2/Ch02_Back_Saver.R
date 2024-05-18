# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Max Profit = 32C + 24M (Objective Function)
# Subject to (Constraints)
# 3C  +  2M  <= 5400
# .75C + 0.67M <= 35*40
# C >= 0
# M >= 0

# Set coefficients of the objective function ----
f.obj <- c(32, 24)

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(3, 2,
                  0.75, 0.666666666, #entering 0.6666666 to get the exact result
                  1, 0,
                  0, 1), nrow = 4, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs ----
f.dir <- c("<=",
           "<=",
           "<=",
           "<=")

# Set right hand side parameters ----
f.par <- c(5000,
           1400,
           1000,
           1200)

# Solve the linear programming problem ----
result <- lp("max", f.obj, f.con, f.dir, f.par)

# Print the final objective value ----
Profit <- result$objval
Profit

# Print the final variable values ----
variables <- result$solution
variables


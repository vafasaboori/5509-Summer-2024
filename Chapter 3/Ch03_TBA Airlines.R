# Clearing the Environment ----
rm(list = ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model
# Max E = 7S + 22L (Objective Function)
# Subject to (Constraints)
# 25S + 75L <= 250 (Capital Available)
# S <= 5 (Max Small Airplanes)
# S >= 0, L >= 0 (Both Integer)

# Set coefficients of the objective function ----
f.obj <- c(7, 22)

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(25, 75,
                  1, 0), nrow = 2, byrow = TRUE)

# Set inequality signs ----
f.dir <- c("<=", 
           "<=")

# Set right hand side parameters ----
f.par <- c(250, 
           5)

# Solve the linear programming problem ----
lp_result <- lp("max", f.obj, f.con, f.dir, f.par, int.vec = 1:2)
# int.vec gives the indices of variables that are required to be integer.
# The length of this vector will therefore be the number of integer variables.

# Print the final value
lp_result

# Print variables' final values
lp_result$solution

# Print int.vec information
lp_result$int.vec


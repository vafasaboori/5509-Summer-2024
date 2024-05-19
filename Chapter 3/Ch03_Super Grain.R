# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Max E = 1300TV + 600M + 500SS (Objective Function)
# Subject to (Constraints)
# 300TV + 150M + 100SS <=4000 (Ad Spending)
# 90TV + 30M +40SS <= 1000 (Planning Costs)
# TV <= 5
# TV >= 0, M >= 0, SS >= 0

# Set coefficients of the objective function ----
f.obj <- c(1300, 600, 500)

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(300, 150, 100,
                  90,  30,  40,
                  1,   0,   0), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs ----
f.dir <- c("<=",
           "<=",
           "<=")

# Set right hand side parameters ----
f.par <- c(4000,
           1000,
           5)

# Final value (p) ----
lp_result <- lp("max", f.obj, f.con, f.dir, f.par)
lp_result
# or
lp_result$objval

# Variables final values
lp_result$solution


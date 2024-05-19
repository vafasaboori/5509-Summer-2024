# Clearing the Environment ----
rm(list = ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Max NPV = 45*OB + 70*H + 50*SC (Objective Function)
# Subject to (Constraints)
# 40*OB + 80*H + 90*SC <= 25 (Down)
# 100*OB + 160*H + 140*SC <= 45 (1st yr)
# 190*OB + 240*H + 160*SC <= 65 (2nd yr)
# 200*OB + 310*H + 220*SC <= 80 (3rd yr)
# OB >= 0, H >= 0, SC > 0

# Set coefficients of the objective function ----
f.obj <- c(45, 70, 50)

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(40, 80, 90,
                  100, 160, 140,
                  190, 240, 160,
                  200, 310, 220), nrow = 4, byrow = TRUE)

# Set inequality signs ----
f.dir <- rep("<=", 4)

# Set right hand side parameters ----
f.par <- c(25, 
           45, 
           65, 
           80)

# Solve the linear programming problem ----
lp_result <- lp("max", f.obj, f.con, f.dir, f.par)

# Print the final value
print(lp_result)

# Print variables' final values
print(lp_result$solution)


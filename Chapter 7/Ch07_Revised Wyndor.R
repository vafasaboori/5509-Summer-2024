# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Max P = 300D + 500W - 700Y1 - 1300Y2 (Objective Function)
# Subject to (Constraints)
#  D                 <= 4
#     2W             <= 12
# 3D +2W             <= 18
#  D     -99Y1       <= 0  (D <= 99Y1)
#      W       -99Y2 <= 0  (W <= 99Y2)
# D >= 0
# W >= 0

# Solve Mixed Problem ----
#Set coefficients of the objective function
f.obj <- c(300, 500, -700, -1300)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1,  0,  0,  0,
                  0,  2,  0,  0,
                  3,  2,  0,  0,
                  1,  0, -99, 0,
                  0,  1,  0, -99), nrow = 5, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- rep("<=", 5)

# Set right hand side parameters
f.par <- c(4,
           12,
           18,
           0,
           0)

# Solve the optimization problem ----
result <- lp("max", f.obj, f.con, f.dir, f.par, int.vec = 1:2, binary.vec = 3:4)

# Extract objective value
objective_value <- result$objval

# Variables final values
solution <- result$solution
variable_names <- c("D", "W", "Y1", "Y2")

# Output the objective value and variable values
cat("Objective Value:", objective_value, "\n")
cat("Variable Values:\n")
for (i in 1:length(solution)) {
  cat(variable_names[i], ": ", solution[i], "\n")
}

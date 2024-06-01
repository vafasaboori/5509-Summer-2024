# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Min C = 1TV + 2PM in Millions (Objective Function)
# Subject to (Constraints)
#  0TV + 1PM >= 3
#  3TV + 2PM >= 18
# -1TV + 4PM >= 4
# TV >= 0
# PM >= 0

# Original Problem ----
# Set coefficients of the objective function
f.obj <- c(1, 2)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(0, 1,
                  3, 2,
                 -1, 4), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- c(">=",
           ">=",
           ">=")

# Set right hand side parameters
f.par <- c(3,
           18,
           4)

# Solve the linear programming problem
lp_result <- lp("min", f.obj, f.con, f.dir, f.par)

# Print the final value (p)
lp_result

# Print variable final values
lp_result$solution


# Sensitivity Analysis ----

# Perform sensitivity analysis 
sens <- lp("min", f.obj, f.con, f.dir, f.par, compute.sens = TRUE)

# Perform sensitivity analysis for objective function coefficients
sens$sens.coef.from
sens$sens.coef.to

# Cleaner Results
round(sens$sens.coef.from, 3)
round(sens$sens.coef.to, 3)

# Perform sensitivity analysis for shadow prices
round(sens$duals, 3)

# Sensitivity Analysis (Allowable RHS, constraints first, variables next)
options(scipen = 0, digits = 3)
round(sens$duals.from, 3)
round(sens$duals.to, 3)

# Additional Steps (Robust) In Class Exercise (Liquid Detergent Cionstraint) ----
# What if the estimates for increase in sales of liquid detergent are uncertain? ----
# range of uncertainty for increase in sales per TV is uniform (2.5–3.5)
# range of uncertainty for increase in sales per Print Media is uniform (1.5–2.5)

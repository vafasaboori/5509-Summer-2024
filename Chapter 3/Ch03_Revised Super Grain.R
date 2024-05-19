# Clearing the Environment ----
rm(list = ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Max E = 1300TV + 600M + 500SS (Objective Function)
# Subject to 
# Resource Constraints
# 300TV + 150M + 100SS <= 4000 (Ad Spending)
# 90TV + 30M + 40SS <= 1000 (Planning Costs)
# TV <= 50
# Benefit Constraints
# 1.2TV + 0.1M >= 5 (min kids million)
# 0.5TV + 0.2M + 0.2SS >= 5 (min parents million)
# Fixed Requirement Constraints
# 40M + 120SS = 1400 (Coupon Budget)
# TV >= 0, M >= 0, SS >= 0

# Set coefficients of the objective function ----
f.obj <- c(1300, 600, 500)

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(300, 150, 100,
                  90,  30,  40,
                  1,   0,   0,
                  1.2, 0.1, 0,
                  0.5, 0.2, 0.2,
                  0, 40, 120), nrow = 6, byrow = TRUE)

# Set inequality signs ----
f.dir <- c("<=",
           "<=",
           "<=",
           ">=",
           ">=",
           "=")

# Set right-hand side parameters ----
f.par <- c(4000,
           1000,
           5,
           5,
           5,
           1490)

# Solve linear programming problem ----
lp_result <- lp("max", f.obj, f.con, f.dir, f.par)
print(lp_result)

# Check if the problem is solved successfully ----
if (lp_result$status == 0) 
  # 0 indicates that the solver found an optimal solution
  { #curly braces specify the block of code to be executed based on a condition.
  # Print optimal solution
  cat("Optimal Solution:\n")
  cat("TV =", lp_result$solution[1], "\n") # \n for line break
  cat("M =", lp_result$solution[2], "\n")
  cat("SS =", lp_result$solution[3], "\n")
  } else 
    {
      cat("Linear programming problem could not be solved.")
      }


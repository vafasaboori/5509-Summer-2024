# Clearing the Environment ----
rm(list = ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Min Cost = 700F1C1 + 900F1C2 + 800F1C3 + 800F2C1 + 900F2C2 + 700F2C3 (Obj. F.)
# Subject to 
# Fixed Requirement Constraints
# F1C1 + F1C2 + F1C3                      = 12 (Factory 1)
#                     F2C1 + F2C2 + F2C3  = 15 (Factory 1)
# F1C1              + F2C1                = 10 (Customer 1)
#        F1C2              + F2C2         = 8 (Customer 2)
#               F1C3              + F2C3  = 9 (Customer 3)
# F1C1, F1C2, F1C3, F2C1, F2C2, F2C3 >= 0

# Define the Algebraic Model ----
obj_coeffs <- c(700, 900, 800, 800, 900, 700)
const_coeffs <- matrix(c(1, 1, 1, 0, 0, 0,
                         0, 0, 0, 1, 1, 1,
                         1, 0, 0, 1, 0, 0,
                         0, 1, 0, 0, 1, 0,
                         0, 0, 1, 0, 0, 1), nrow = 5, byrow = TRUE)

const_signs <- rep("=", 5)

rhs_values <- c(12, 15, 10, 8, 9)

# Solve the linear programming problem ----
result <- lp(direction = "min",
             objective.in = obj_coeffs,
             const.mat = const_coeffs,
             const.dir = const_signs,
             const.rhs = rhs_values,
             all.int = TRUE)  # Specify that all variables are integers

# Check if the problem is solved successfully ----
if (result$status == 0) {
  # Print optimal solution
  cat("Optimal Solution:\n")
  cat("Factory 1, Customer 1 =", result$solution[1], "\n")
  cat("Factory 1, Customer 2 =", result$solution[2], "\n")
  cat("Factory 1, Customer 3 =", result$solution[3], "\n")
  cat("Factory 2, Customer 1 =", result$solution[4], "\n")
  cat("Factory 2, Customer 2 =", result$solution[5], "\n")
  cat("Factory 2, Customer 3 =", result$solution[6], "\n")
} else {
  cat("Unable to find an optimal solution for the linear programming problem.")
}


# Clearing the Environment ----
rm(list = ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Min Cost = 170S1 + 160*S2 + 175S3 + 180S4 + 195S5 (Objective Function)
# Subject to (Constraints)
# S1                      >= 48
# S1 + S2                 >= 79
# S1 + S2                 >= 65
# S1 + S2 + S3            >= 87
#      S2 + S3            >= 64
#           S3 + S4       >= 73
#           S3 + S4       >= 82
#                S4       >= 43
#                S4 + S5  >= 52
#                     S5  >= 15

# Set matrix corresponding to coefficients of constraints by rows ----
# Do not consider the non-negativity constraint; it is automatically assumed
constraints <- matrix(c(1, 0, 0, 0, 0,
                        1, 1, 0, 0, 0,
                        1, 1, 0, 0, 0,
                        1, 1, 1, 0, 0,
                        0, 1, 1, 0, 0,
                        0, 0, 1, 1, 0,
                        0, 0, 1, 1, 0,
                        0, 0, 0, 1, 0,
                        0, 0, 0, 1, 1,
                        0, 0, 0, 0, 1), nrow = 10, byrow = TRUE)

# Set inequality signs ----
ineq.signs <- rep(">=", 10)

# Set right-hand side parameters ----
rhs <- c(48, 
         79, 
         65, 
         87, 
         64, 
         73, 
         82, 
         43, 
         52, 
         15)

# Final value (p) ----
result <- lp(direction = "min", 
             objective.in = c(170, 160, 175, 180, 195),
             const.mat = constraints, 
             const.dir = ineq.signs, 
             const.rhs = rhs, 
             all.int = TRUE)

cat("Optimal Cost:") #cat: concatenate and print, example: cat("Hello", "world!")
print(result)

# Print the optimal solution
cat("Optimal Solution:")
print(result$solution)


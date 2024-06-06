# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Xn: Fire Station in Tract n (n: 1 to 8)
# Min Cost = 350X1 + 250X2 + 450X3 + 300X4 + 50X5 + 400X6 + 300X7 + 200X8 in thousands (Objective Function)
# Subject to (Set Covering Constraints)
#               X1 +    X2         +    X4                                 >= 1 (Tract 1)
#               X1 +    X2 +    X3                                         >= 1 (Tract 2)
#                       X2 +    X3                +    X6                  >= 1 (Tract 3)
#               X1                 +    X4                +    X7          >= 1 (Tract 4)
#                                              X5         +    X7          >= 1 (Tract 5)
#                               X3                +    X6          +   X8  >= 1 (Tract 6)
#                                       X4                +    X7  +   X8  >= 1 (Tract 7)
#                                                      X6 +    X7  +   X8  >= 1 (Tract 8)
# Xn: Binary Integer (n: 1 to 8)

# Solve BIP Problem ----
# Set coefficients of the objective function
f.obj <- c(350, 250, 450, 300, 50, 400, 300, 200)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1, 1, 0, 1, 0, 0, 0, 0, 
                  1, 1, 1, 0, 0, 0, 0, 0,
                  0, 1, 1, 0, 0, 1, 0, 0,
                  1, 0, 0, 1, 0, 0, 1, 0,
                  0, 0, 0, 0, 1, 0, 1, 0,
                  0, 0, 1, 0, 0, 1, 0, 1,
                  0, 0, 0, 1, 0, 0, 1, 1,
                  0, 0, 0, 0, 0, 1, 1, 1), nrow = 8, byrow = TRUE) 
# R Challenge: Could this matrix be automatically created from the response time matrix?

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- rep(">=", 8)

# Set right hand side parameters
f.par <- rep(1, 8)

# Solve the optimization problem
result <- lp("min", f.obj, f.con, f.dir, f.par, all.bin = TRUE)

# Extract the solution
solution <- result$solution

# Extract the value of the objective function
obj_value <- result$objval

# Print the optimized solution and objective function value
cat("Optimized solution:\n")
print(solution)

cat("Objective function value:", obj_value, "\n")

# In Class Exercise -----
# Optimize this code, so there is no need for calculations outside R environment.

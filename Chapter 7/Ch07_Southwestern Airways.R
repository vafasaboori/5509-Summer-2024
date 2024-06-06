# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Xj: Sequence "j" is assigned to a crew (j: 1 to 12)
# Min Cost = 2X1 +3X2 +4X3 +6X4 +7X5 +5X6 +7X7 +8X8 +9X9 +9X10 +8X11 +9X12 in thousands (Objective Function)
# Subject to (Set Covering Constraints)
#             X1 +X2 +X3 +X4 +X5 +X6 +X7 +X8 +X9 +X10 +X11 +X12  <=3 (Only 3 Crews Available)
#             X1         +X4         +X7         +X10            >=1 (SFO-LAX)
#                 X2         +X5         +X8          +X11       >=1 (SFO-DEN)
#                     X3         +X6         +X9           +X12  >=1 (SFO-SEA)
#                         X4         +X7     +X9 +X10      +X12  >=1 (LAX-ORD)
#             X1                 +X6             +X10 +X11       >=1 (LAX-SFO)
#                         X4 +X5             +X9                 >=1 (ORD-DEN)
#                                     X7 +X8     +X10 +X11 +X12  >=1 (ORD-SEA)
#                 X2     +X4 +X5             +X9                 >=1 (DEN-SFO)
#                             X5         +X8          +X11       >=1 (DEN-ORD)
#                     X3             +X7 +X8               +X12  >=1 (SEA-SFO)
#                                 X6         +X9 +X10 +X11 +X12  >=1 (SFO-LAX)
# Xj: Binary Integer (j: 1 to 12)

# Solve BIP Problem ----

# Define the cost of each sequence (in thousands of dollars)
cost <- c(2, 3, 4, 6, 7, 5, 7, 8, 9, 9, 8, 9)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed

# Define the constraint matrix
constraints <- matrix(c(
  # Number of crews constraint
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  
  # Flight coverage constraints
  1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0,  # SFO-LAX
  0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0,  # SFO-DEN
  0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1,  # SFO-SEA
  0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1,  # LAX-ORD
  1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0,  # LAX-SFO
  0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0,  # ORD-DEN
  0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1,  # ORD-SEA
  0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0,  # DEN-SFO
  0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0,  # DEN-ORD
  0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1,  # SEA-SFO
  0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1   # SEA-LAX
), nrow = 12, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Define the direction of the constraints
directions <- c("<=", rep(">=", 11))

# Define the right-hand side of the constraints
rhs <- c(3, rep(1, 11))

# Solve the binary integer programming problem
result <- lp("min", cost, constraints, directions, rhs, all.bin = TRUE)

# Print the optimized objective function value
cat("Optimized Objective Function Value:", result$objval, "thousands\n")

# Print the variables final values
cat("Variables Final Values:\n")
for (i in 1:12) {
  cat("X", i, ":", result$solution[i], "\n")
}


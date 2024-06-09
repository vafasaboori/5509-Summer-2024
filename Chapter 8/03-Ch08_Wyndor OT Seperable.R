# Clear the environment
rm(list = ls())

# Load the required packages
library(lpSolve)

# Algebraic Model
# Maximize P = 300DR + 200DO + 500WR + 100WO
# Subject to
# DR                      < 3
#     DO                  < 1
#         WR              < 3
#             WO          < 3
# DR + DO 				        < 4
#       2(WR + WO) 			  < 12
# 3(DR + DO) + 2(WR + WO) < 18
# DR > 0, DO > 0, WR > 0, WO  > 0

# Define the coefficients of the objective function
objective <- c(300, 200, 500, 100)

# Define the inequality constraints matrix
# Each row represents a constraint, and each column represents a variable
ineq.constraint <- matrix(c(
  1, 0, 0, 0,   # DR coefficient
  0, 1, 0, 0,   # DO coefficient
  0, 0, 1, 0,   # WR coefficient
  0, 0, 0, 1,   # WO coefficient
  1, 1, 0, 0,   # DR + DO coefficient
  0, 0, 2, 2,   # WR + WO coefficient
  3, 3, 2, 2    # 3(DR + DO) + 2(WR + WO) coefficient
), ncol = 4, byrow = TRUE)

# Define the direction of the inequality constraints
direction <- rep("<", 7)

# Define the right-hand side values of the inequality constraints
rhs <- c(3, 1, 3, 3, 4, 12, 18)

# Solve the linear programming problem
solution <- lp("max", objective, ineq.constraint, direction, rhs)

# Extract the optimal objective value and variable values
optimal.P <- solution$objval
optimal.variables <- solution$solution

# Print the results
cat("Optimal P =", optimal.P, "\n")
cat("Optimal variable values:\n")
cat("DR =", optimal.variables[1], "\n")
cat("DO =", optimal.variables[2], "\n")
cat("WR =", optimal.variables[3], "\n")
cat("WO =", optimal.variables[4], "\n")


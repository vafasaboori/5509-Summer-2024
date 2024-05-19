# Clearing the Environment ----
rm(list = ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----

# Decision Variables
# Let XY = Employee (X) doing task (Y)
# Employees: Ann (A), Ian (I), Joan (J), Sean (S)
# Tasks: Word Processing (W), Graphics (G), Packets (P), Registration (R)
# Objective Function
# Min Cost =  490AW + 574AG + 378AP + 560AR
#           + 564IW + 540IG + 384IP + 612IR
#           + 507JW + 728JG + 468JP + 559JR
#           + 480SW + 765SG + 375SP + 690SR
# Subject to 
# Only one employee should be assigned to each task
# AW + AG + AP + AR = 1
# IW + IG + IP + IR = 1
# JW + JG + JP + JR = 1
# SW + SG + SP + SR = 1
# Only one task should be assigned to each employee
# AW + IW + JW + SW = 1
# AG + IG + JG + SG = 1
# AP + IP + JP + SP = 1
# AR + IR + JR + SR = 1
# All variables >=0 

# Set coefficients of the objective function ----
f.obj <- c(490, 574, 378, 560, 564, 540, 384, 612, 507, 728, 468, 559, 480, 765, 375, 690)

# Set matrix corresponding to coefficients of constraints by rows ----
f.con <- matrix(c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                  1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
                  0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
                  0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,
                  0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 8, byrow = TRUE)

# Set inequality signs ----
f.dir <- rep("=", 8)

# Set right-hand side parameters ----
f.par <- rep(1, 8)

# Solve the linear programming problem ----
results <- lp("min", f.obj, f.con, f.dir, f.par)
results
results$solution

# Check if the problem is solved successfully ----
if (results$status == 0) {
  # Print optimal solution
  cat("Optimal Solution:\n")
  cat("Ann, Word Processing =", results$solution[1], "\n")
  cat("Ann, Graphics =", results$solution[2], "\n")
  cat("Ann, Packets =", results$solution[3], "\n")
  cat("Ann, Registration =", results$solution[4], "\n")
  cat("Ian, Word Processing =", results$solution[5], "\n")
  cat("Ian, Graphics =", results$solution[6], "\n")
  cat("Ian, Packets =", results$solution[7], "\n")
  cat("Ian, Registration =", results$solution[8], "\n")
  cat("Joan, Word Processing =", results$solution[9], "\n")
  cat("Joan, Graphics =", results$solution[10], "\n")
  cat("Joan, Packets =", results$solution[11], "\n")
  cat("Joan, Registration =", results$solution[12], "\n")
  cat("Sean, Word Processing =", results$solution[13], "\n")
  cat("Sean, Graphics =", results$solution[14], "\n")
  cat("Sean, Packets =", results$solution[15], "\n")
  cat("Sean, Registration =", results$solution[16], "\n")
} else {
  cat("Unable to find an optimal solution for the linear programming problem.")
}

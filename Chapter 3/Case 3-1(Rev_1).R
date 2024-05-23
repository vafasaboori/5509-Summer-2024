# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Option 1 ----
# Continue Shipping Exclusively by Rail

# Algebraic Model
# Min Cost = 61S1M1 + 72S1M2 + 45S1M3 + 55S1M4 + 66S1M5
#            69S2M1 + 78S2M2 + 60S2M3 + 49S2M4 + 56S2M5
#            59S3M1 + 66S3M2 + 63S3M3 + 61S3M4 + 47S3M5   (Obj. F.)
# Subject to 
# Fixed Requirement Constraints
# S1M1 + S1M2 + S1M3 + S1M4 + S1M5                                                                     = 15 (Source 1)
#                                   S2M1 + S2M2 + S2M3 + S2M4 + S2M5                                   = 20 (Source 2)
#                                                                     S3M1 + S3M2 + S3M3 + S3M4 + S3M5 = 15 (Source 3)
# S1M1                            + S2M1                            + S3M1                             = 11 (Market 1)
#        S1M2                            + S2M2                            + S3M2                      = 12 (Market 2)
#              S1M3                             + S2M3                            + S3M3               = 9  (Market 3)
#                      S1M4                            + S2M4                            + S3M4        = 10 (Market 4)
#                            S1M5                             + S2M5                            + S3M5 = 8  (Market 5)
# SiMj >=0 


# Set coefficients of the objective function
f.obj <- c(61,	72,	45,	55,	66,
           69,	78,	60,	49,	56,
           59,	66,	63,	61,	47)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # Source 1
                  0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, # Source 2
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, # Source 3
                  1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, # Market 1 
                  0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, # Market 2
                  0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, # Market 3
                  0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, # Market 4
                  0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1  # Market 5
), nrow = 8, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- rep("=", 8)

# Set right hand side parameters
f.par <- c(15,
           20,
           15,
           11,
           12,
           9,
           10,
           8)

# Solving the LP Problem
result_option_1 <- lp("min", f.obj, f.con, f.dir, f.par)

# Results
# Option 1: Continue Shipping Exclusively by Rail 
# Optimal Solution
print(result_option_1$solution)
# Objective Function Value
print(result_option_1$objval)


# Option 2 ----
# Switch to shipping exclusively by water (except where only rail is feasible)

# Set coefficients of the objective function
f.obj <- c(58.5,	68.3,	47.8,	55.0,	63.5,
           65.3,	74.8,	55.0,	49.0,	57.5,
           59.0,	61.3,	63.5,	58.8,	50.0)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # Source 1
                  0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, # Source 2
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, # Source 3
                  1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, # Market 1 
                  0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, # Market 2
                  0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, # Market 3
                  0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, # Market 4
                  0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1  # Market 5
), nrow = 8, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- rep ("=", 8)

# Set right hand side parameters
f.par <- c(15,
           20,
           15,
           11,
           12,
           9,
           10,
           8)

# Solving the LP Problem
result_option_2 <- lp("min", f.obj, f.con, f.dir, f.par)

# Results
# Option 2: Switch to shipping  by water (except where only rail is feasible)
# Optimal Solution
print(result_option_2$solution)
# Objective Function Value
print(result_option_2$objval)


# Option 3 ----

# Ship by either rail or water, depending on which is less expensive for the particular route

# Set coefficients of the objective function
f.obj <- c(58.5,	68.3,	45.0,	55.0,	63.5,
           65.3,	74.8,	55.0,	49.0,	56.0,
           59.0,	61.3,	63.0,	58.8,	47.0)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # Source 1
                  0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, # Source 2
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, # Source 3
                  1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, # Market 1 
                  0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, # Market 2
                  0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, # Market 3
                  0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, # Market 4
                  0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1  # Market 5
), nrow = 8, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- rep("=", 8)

# Set right hand side parameters
f.par <- c(15,
           20,
           15,
           11,
           12,
           9,
           10,
           8)

# Solving the LP Problem
result_option_3 <- lp("min", f.obj, f.con, f.dir, f.par)

# Results
# Option 3: Ship by either rail or water, depending on which is less expensive.
# Optimal Solution
print(result_option_3$solution)
# Objective Function Value
print(result_option_3$objval)

# When comparing the three options, it is best to use the combination plan, 
# shipping entirely by rail leads to the highest costs.
# If shipping by water are more expensive than rail, stay with rail Option 1.  
# If the reverse is true, then use Option 2.  
# If the cost comparisons will remain roughly the same, use Option 3.  
# Option 3 is clearly the most feasible but could be logistically too cumbersome.  
# More knowledge of the situation is necessary to determine this.


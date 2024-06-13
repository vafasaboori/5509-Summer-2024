# Clearing the Environment ----
rm(list=ls())

# part d) Quadratic Programming ----

# Load nloptr package
library(nloptr) # Nonlinear Optimization in R

# Objective function
objective_function <- function(x) {
  TV <- x[1]
  M <- x[2]
  SS <- x[3]
  
  # Sales
  sales <-  -0.1036 * TV^2 + 1.1264 * TV - 0.04 + 
            -0.002 * M^2 + 0.124 * M + 0.14 + 
            -0.0321 * SS^2 + 0.706 * SS - 0.09
  
  # Costs
  cost_of_ads <- 0.3 * TV + 0.15 * M + 0.1 * SS
  planning_cost <- 0.09 * TV + 0.03 * M + 0.04 * SS
  
  # Profit
  profit <- 0.75 * sales - cost_of_ads - planning_cost
  
  # Return negative profit because nloptr minimizes
  return(-profit)
}

# Small tolerance for inequality conversion
epsilon <- 1e-6

# Inequality constraints
inequality_constraints <- function(x) {
  TV <- x[1]
  M <- x[2]
  SS <- x[3]
  
  return(c(
    (300 * TV + 150 * M + 100 * SS - 4000), # 300TV + 150M + 100SS ≤ 4,000 (Ad)
    (90 * TV + 30 * M + 40 * SS - 1000),    # 90TV + 30M + 40SS	≤ 1,000 (Planning)
    TV - 5,                                 # TV ≤ 5 (Max TV Spots)
    5 - (1.2 * TV + 0.1 * M),               # 1.2TV + 0.1M ≥ 5 (Young Children)
    5 - (0.5 * TV + 0.2 * M + 0.2 * SS),    # 0.5TV + 0.2M + 0.2SS ≥ 5 (parents)

    # 40M + 120SS = 1,490 (coupon) convert to inequality
    40 * M + 120 * SS - 1490 - epsilon,    # 40M + 120SS <= 1,490 + epsilon
    1490 - epsilon - (40 * M + 120 * SS)   # 40M + 120SS >= 1,490 - epsilon    
  ))
}

# Initial values and bounds
x0 <- c(1, 1, 1)  # Initial guess
lb <- c(0, 0, 0)  # Lower bounds
ub <- c(5, Inf, Inf)  # Upper bounds

# Solve
result <- nloptr(
  x0 = x0,
  eval_f = objective_function,
  lb = lb,
  ub = ub,
  eval_g_ineq = inequality_constraints,
  opts = list(
    "algorithm" = "NLOPT_LN_COBYLA",
    "xtol_rel" = 1.0e-8
  )
)

# Extract optimized values
optimized_values <- result$solution

# Extract objective function value
objective_value <- -result$objective # Since we minimized the negative profit

# Print results
cat("Optimized values:\n")
cat("TV spots: ", optimized_values[1], "\n")
cat("Magazine ads: ", optimized_values[2], "\n")
cat("Sunday supplements: ", optimized_values[3], "\n")
cat("Objective function value (Profit): ", objective_value, "\n")

# part e) seperable programming -----

# Clear the environment
rm(list = ls())

# Load the required packages
library(lpSolve)

# Algebraic Model
# Gross Profit = Sales * 0.75 
# Gross Profit = 0.75 * ( T1 + 0.75T2 + 0.7T3 + 0.35T4 + 0.2T5
#             + 0.14M1 + 0.1M2 + 0.07M3 + 0.05M4 + 0.04M5
#             + 0.6S1 + 0.5S2 + 0.4S3 + 0.25S4 + 0.125S5)

# Cost of Ads 300 * (T1 + T2 + T3 + T4 + T5) +
#             150 * (M1 + M2 + M3 + M4 + M5)
#             100 * (S1 + S2 + S3 + S4 + S5)

# Cost of Planning 
#             90 * (T1+T2+T3+T4+T5) +
#             30 * (M1 + M2 + M3 + M4 + M5)
#             40 * (S1 + S2 + S3 + S4 + S5)

# Max Profit = Gross Profit - ((Cost of Ads + Cost of Planning)/1000)

# Subject to
#             300 * (T1+T2+T3+T4+T5) +
#             150 * (M1 + M2 + M3 + M4 + M5)
#             100 * (S1 + S2 + S3 + S4 + S5) <= 4000

#             90 * (T1+T2+T3+T4+T5) +
#             30 * (M1 + M2 + M3 + M4 + M5)
#             40 * (S1 + S2 + S3 + S4 + S5) <= 1000

#             1.2 * (T1+T2+T3+T4+T5) +
#             0.1 * (M1 + M2 + M3 + M4 + M5)
#             0 * (S1 + S2 + S3 + S4 + S5) >= 5

#             0.5 * (T1+T2+T3+T4+T5) +
#             0.2 * (M1 + M2 + M3 + M4 + M5)
#             0.2 * (S1 + S2 + S3 + S4 + S5) >= 5

#             T1+T2+T3+T4+T5 <= 5

#             0 * (T1+T2+T3+T4+T5) +
#             40 * (M1 + M2 + M3 + M4 + M5)
#             120 * (S1 + S2 + S3 + S4 + S5) = 1490

#             T1, T2, T3, T4, T5 <=1
#             M1, M2, M3, M4, M5 <=5
#             S1, S2, S3, S4, S5 <=2

# Gross Profit raw coefficients without 0.75 multiplier
profit_coeffs <- c(1, 0.75, 0.7, 0.35, 0.2,
                   0.14, 0.1, 0.07, 0.05, 0.04,
                   0.6, 0.5, 0.4, 0.25, 0.125)


# Coefficients for Cost of Ads and Cost of Planning
costs_ads <- c(rep(300, 5), rep(150, 5), rep(100, 5))
costs_planning <- c(rep(90, 5), rep(30, 5), rep(40, 5))

# Objective function: profit to be maximized
obj_fun <- 0.75 * profit_coeffs - (costs_ads + costs_planning) / 1000

# Constraints
# Ads Cost Constraint <= 4000
ads_constraint <- c(rep(300, 5), rep(150, 5), rep(100, 5))

# Planning Cost Constraint <= 1000
planning_constraint <- c(rep(90, 5), rep(30, 5), rep(40, 5))


# Other Constraints
kids_constraint <- c(rep(1.2, 5), rep(0.1, 5), rep(0, 5))
parents_constraint <- c(rep(0.5, 5), rep(0.2, 5), rep(0.2, 5))
T_Max_Constraint <- c(rep(1, 5), rep(0, 10))
cupon_constraint <- c(rep(0, 5), rep(40, 5), rep(120, 5))

# Maximum constraints for individual variables
# T1, T2, T3, T4, T5 <= 1
max_T_constraint <- diag(c(rep(1, 5), rep(0, 10)))

# M1, M2, M3, M4, M5 <= 5
max_M_constraint <- diag(c(rep(0, 5), rep(1, 5), rep(0, 5)))

# S1, S2, S3, S4, S5 <= 2
max_S_constraint <- diag(c(rep(0, 10), rep(1, 5)))


# Combine all constraints into a matrix
constraints <- rbind(
  ads_constraint,
  planning_constraint,
  kids_constraint,
  parents_constraint,
  T_Max_Constraint,
  cupon_constraint,
  max_T_constraint,
  max_M_constraint,
  max_S_constraint
)

# Directions of constraints
directions <- c("<=", "<=", ">=", ">=", "<=", "=",
                rep("<=", 15), # for max_T_constraint
                rep("<=", 15), # for max_M_constraint
                rep("<=", 15)  # for max_S_constraint
)

# RHS of constraints
rhs <- c(4000, 1000, 5, 5, 5, 1490, rep(1, 15), rep(5, 15), rep(2, 15))

# Solve the LP
solution <- lp("max", obj_fun, constraints, directions, rhs)

# Print the status of the solution
print(paste("Status:", solution$status))

# Print the objective value
print(paste("Objective Value:", solution$objval))

# Print the values of the decision variables
variable_names <- c("T1", "T2", "T3", "T4", "T5",
                    "M1", "M2", "M3", "M4", "M5",
                    "S1", "S2", "S3", "S4", "S5")

# Combine variable names with their values
decision_variables <- data.frame(Variable = variable_names, Value = solution$solution)

# Print the decision variables
print(decision_variables)


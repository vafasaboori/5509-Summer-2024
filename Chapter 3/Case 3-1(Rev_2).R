# Clearing the Environment ----
rm(list=ls())

# Import lpSolve and lpSolveAPI package ----
library(lpSolve)
library(lpSolveAPI) # provides an R API for the lp solve library

# Algebraic Model ----
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

# Define the data for the problem ----
sources <- 1:3
markets <- 1:5
wood_avail <- c(15, 20, 15)
demand <- c(11, 12, 9, 10, 8)

# Shipping costs by rail 
shipping_cost_rail <- matrix(c(61, 72, 45, 55, 66, 
                               69, 78, 60, 49, 56, 
                               59, 66, 63, 61, 47), nrow = 3, ncol = 5, byrow = TRUE)

# Shipping costs by ship
shipping_cost_ship <- matrix(c(31, 38, 24, NA, 35, 
                               36, 43, 28, 24, 31, 
                               NA, 33, 36, 32, 26), nrow = 3, ncol = 5, byrow = TRUE)

# Ship investment costs
ship_investment <- matrix(c(275, 303, 238, NA, 285,
                            293, 318, 270, 250, 265,
                            NA, 283, 275, 268, 240), nrow = 3, ncol = 5, byrow = TRUE)

# Number of sources and markets
num_sources <- length(sources)
num_markets <- length(markets)

# Option 1: Ship exclusively by rail ----

# Create the LP problem ----
lp_model_1 <- make.lp(0, num_sources * num_markets) 
# sets up a framework to define the obj func, constraints, etc. of the LP problem.
# make.lp(num.rows, num.cols)
  # num.rows is the number of constraints (rows) in the LP model
  # num.cols specifies the number of decision variables (columns). 
  # num.rows set to 0 initially. Constraints are added later  
  # number of columns is num_sources * num_markets, representing the decision variables.

# Set the objective function (total cost)
obj <- rep(0, num_sources * num_markets)
  # create a vector of zeros with a length equal to number of decision variables. 
  # This vector will be used to define the coefficients of the objective function.
  # It will be updated with the  shipping costs and investment costs later in the script.

for (i in 1:num_sources) {
  for (j in 1:num_markets) {
    obj[(i - 1) * num_markets + j] <- shipping_cost_rail[i, j]
  }
}
# nested loops to convert a matrix, shipping_cost_rail, into a vector, obj

lp.control(lp_model_1, sense = "min") 
# modify control parameters of an LP, for instance "sense" of the model to min
# sense could take max", "min", and "="

set.objfn(lp_model_1, obj)
# set the objective function coefficients of an LP model
  # obj is a vector specifying the coefficients of the objective function.

# Add constraints: supply at sources ----
for (i in 1:num_sources) { # loop over each source to add 3 constraints for them
  constr <- rep(0, num_sources * num_markets) # temp vector for constraint(s) all zeros
  constr[((i - 1) * num_markets + 1):(i * num_markets)] <- 1
  # creates the following: 
  # 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # Source 1
  # 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, # Source 2
  # 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, # Source 3
  add.constraint(lp_model_1, constr, "=", wood_avail[i]) # adds constraint to LP model
}


# Add constraints: demand at markets ----
for (j in 1:num_markets) { # loop over each market to add 5 constraints for them
  constr <- rep(0, num_sources * num_markets) # temp vector for constraint(s) all zeros
  for (i in 1:num_sources) {
    constr[(i - 1) * num_markets + j] <- 1
  }
  # creates the following:
  # 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, # Market 1 
  # 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, # Market 2
  # 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, # Market 3
  # 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, # Market 4
  # 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1  # Market 5
  add.constraint(lp_model_1, constr, "=", demand[j]) # adds constraint to LP model
}

# Solve the LP problem
solve(lp_model_1)

# Extract the solution
solution <- get.variables(lp_model_1)
solution

# Print the solution
cat("Shipping Plan (Option 1):\n")
for (i in 1:num_sources) {
  for (j in 1:num_markets) { # nested loop for all sources and markets in the solution vector
    index <- (i - 1) * num_markets + j # index for variables in the solution vector 
    cat("Source", i, "->", "Market", j, ": ", solution[index], "\n")
  }
}
cat("Total Equivalent Uniform Annual Cost (Option 1): $",
    round(get.objective(lp_model_1), 2), "\n")
# get.objective shows the total cost from the LP model 


# Option 2: Ship exclusively by water (except where only rail is feasible)  ----

# Create the LP problem for Option 2 ----
lp_model_2 <- make.lp(0, num_sources * num_markets)
obj <- rep(0, num_sources * num_markets)

for (i in 1:num_sources) {
  for (j in 1:num_markets) {
    if (is.na(shipping_cost_ship[i, j])) {
      obj[(i - 1) * num_markets + j] <- shipping_cost_rail[i, j]
    } else {
      obj[(i - 1) * num_markets + j] <- shipping_cost_ship[i, j] + ship_investment[i, j] * 0.1
    }
  }
}

lp.control(lp_model_2, sense = "min")
set.objfn(lp_model_2, obj)

# Add constraints: supply at sources ----
for (i in 1:num_sources) {
  constr <- rep(0, num_sources * num_markets)
  constr[((i - 1) * num_markets + 1):(i * num_markets)] <- 1
  add.constraint(lp_model_2, constr, "=", wood_avail[i])
}

# Add constraints: demand at markets ----
for (j in 1:num_markets) {
  constr <- rep(0, num_sources * num_markets)
  for (i in 1:num_sources) {
    constr[(i - 1) * num_markets + j] <- 1
  }
  add.constraint(lp_model_2, constr, "=", demand[j])
}

# Solve the LP problem for Option 2
solve(lp_model_2)

# Extract the solution for Option 2
solution_2 <- get.variables(lp_model_2)

# Print the solution for Option 2
cat("\nShipping Plan (Option 2):\n")
for (i in 1:num_sources) {
  for (j in 1:num_markets) {
    index <- (i - 1) * num_markets + j
    cat("Source", i, "->", "Market", j, ": ", solution_2[index], "\n")
  }
}
cat("Total Equivalent Uniform Annual Cost (Option 2): $",
    round(get.objective(lp_model_2), 2), " \n")


# Option 3: Ship by either rail or water depending on the cost----
# Create the LP problem ----
lp_model_3 <- make.lp(0, num_sources * num_markets)
obj <- rep(0, num_sources * num_markets)

for (i in 1:num_sources) {
  for (j in 1:num_markets) {
    rail_cost <- shipping_cost_rail[i, j]
    ship_cost <- shipping_cost_ship[i, j]
    if (is.na(ship_cost)) {
      # Ship route not feasible, use rail cost
      obj[(i - 1) * num_markets + j] <- rail_cost
    } else {
      # Ship route feasible, compare costs (include investment)
      total_cost <- min(rail_cost, ship_cost + (0.1 * ship_investment[i, j]))
      obj[(i - 1) * num_markets + j] <- total_cost
    }
  }
}

lp.control(lp_model_3, sense = "min")
set.objfn(lp_model_3, obj)

# Add constraints: supply at sources ----
for (i in 1:num_sources) {
  constr <- rep(0, num_sources * num_markets)
  constr[((i - 1) * num_markets + 1):(i * num_markets)] <- 1
  add.constraint(lp_model_3, constr, "=", wood_avail[i])
}

# Add constraints: demand at markets ----
for (j in 1:num_markets) {
  constr <- rep(0, num_sources * num_markets)
  for (i in 1:num_sources) {
    constr[(i - 1) * num_markets + j] <- 1
  }
  add.constraint(lp_model_3, constr, "=", demand[j])
}

# Solve the LP problem for Option 3
solve(lp_model_3)

# Extract the solution for Option 3
solution_3 <- get.variables(lp_model_3)

# Print the solution for Option 3
cat("\nShipping Plan (Option 3):\n")
for (i in 1:num_sources) {
  for (j in 1:num_markets) {
    index <- (i - 1) * num_markets + j
    rail_cost <- shipping_cost_rail[i, j]
    ship_cost <- shipping_cost_ship[i, j]
    if (is.na(ship_cost)) {
      # Ship route not feasible, use rail
      cat("Source", i, "->", "Market", j, ": ", solution_3[index], " (Rail)\n")
    } else {
      # Ship route feasible, compare costs
      if (rail_cost <= ship_cost + (0.1 * ship_investment[i, j])) {
        # Rail cost is cheaper or equal, use rail
        cat("Source", i, "->", "Market", j, ": ", solution_3[index], " (Rail)\n")
      } else {
        # Ship cost is cheaper, use ship
        cat("Source", i, "->", "Market", j, ": ", solution_3[index], " (Ship)\n")
      }
    }
  }
}

cat("Total Equivalent Uniform Annual Cost (Option 3): $",
    round(get.objective(lp_model_3), 2), "\n")


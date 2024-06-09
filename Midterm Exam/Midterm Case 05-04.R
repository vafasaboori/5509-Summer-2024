# Load necessary libraries
library(lpSolve)

# Define cost matrix
cost <- matrix(c( 300,    0,   700,
                  NA,   400,   500,
                  600,  300,   200,
                  200,  500,    NA,
                    0,   NA,   400,
                  500,  300,     0), nrow = 6, byrow = TRUE) 
# This is different from Excel model. 
# NA routes cannot be modeled in R. 
# As a workaround NAs were assigned with a high cost.

# Define number of students in each area
students <- c(450, 600, 550, 350, 500, 450)

# Define grade percentages per area (for 6th, 7th, 8th grades respectively)
grades <- matrix(c(0.32, 0.38, 0.30,
                   0.37, 0.28, 0.35,
                   0.30, 0.32, 0.38,
                   0.28, 0.40, 0.32,
                   0.39, 0.34, 0.27,
                   0.34, 0.28, 0.38), nrow = 6, byrow = TRUE)

# Calculate total number of students
total_students <- sum(students)

# Constraints for the number of students in each grade at each school
min_students <- total_students / 3 * 0.30  # 30% of total
max_students <- total_students / 3 * 0.36  # 36% of total
# This is different from Excel model. 
# 30% and 36% should apply to the total of each school (optimized solution)
# This will lead into a recursive loop in R.
# As a workaround, total number of students in all schools is used.

# Number of variables (6 areas x 3 schools)
num_vars <- 18

# Flatten the cost matrix to a vector for lpSolve
objective <- as.vector(cost)
objective[is.na(objective)] <- 1e30  # Use a large number for infeasible assignments
# This is different from Excel model. 
# NA routes cannot be modeled in R. 
# As a workaround NAs were assigned with a high cost.

# Constraints matrix
num_constraints <- 6 + 6 * 3 * 2
constraints <- matrix(0, nrow = num_constraints, ncol = num_vars)
#This is modeled differently compared to Excel results

# Right-hand side vector
rhs <- numeric(num_constraints)
directions <- character(num_constraints)

# Set constraints for total number of students from each area
for (i in 1:6) {
  constraints[i, (i - 1) * 3 + (1:3)] <- 1
  rhs[i] <- students[i]
  directions[i] <- "=="
}

# Set constraints for grade distribution
row_index <- 7  # Start after the first 6 rows for student total constraints

for (grade in 1:3) {  # For each grade
  for (school in 1:3) {  # For each school
    grade_sum_min <- numeric(num_vars)
    grade_sum_max <- numeric(num_vars)
    
    for (area in 1:6) {  # For each area
      col_index <- (area - 1) * 3 + school
      grade_sum_min[col_index] <- grades[area, grade] * students[area]
      grade_sum_max[col_index] <- -grades[area, grade] * students[area]
    }
    
    constraints[row_index, ] <- grade_sum_min
    constraints[row_index + 1, ] <- grade_sum_max
    rhs[row_index] <- min_students
    rhs[row_index + 1] <- -max_students
    directions[row_index] <- ">="
    directions[row_index + 1] <- "<="
    row_index <- row_index + 2  # Move to next set of constraints
  }
}

# Diagnostic and Correction Chunk

# Define valid directions
valid_directions <- c("<=", "==", ">=")

# Print the directions vector to identify invalid entries
print("Initial directions vector:")
print(directions)

# Identify invalid entries in the directions vector
invalid_entries <- directions[!directions %in% valid_directions]
print("Invalid entries found:")
print(invalid_entries)

# Correcting invalid directions
# Re-assign valid directions for each constraint type

# Total number of students from each area constraints
directions[1:6] <- "=="

# Grade distribution constraints
# There are 6 constraints per grade per school: 3 grades * 3 schools * 2 (>= and <=)
row_index <- 7
for (i in 1:(6 * 3)) {
  directions[row_index] <- ">="
  directions[row_index + 1] <- "<="
  row_index <- row_index + 2
}

# Print the corrected directions
print("Corrected directions vector:")
print(directions)

# Validate the directions vector again to ensure all are valid
if (!all(directions %in% valid_directions)) {
  stop("Invalid direction found in the directions vector after correction.")
}

# Solve the linear program again
result <- lp(direction = "min",
             objective.in = objective,
             const.mat = constraints,
             const.dir = directions,
             const.rhs = rhs)

# Check for success and print the result
if (result$status == 0) {
  cat("Optimal solution found:\n")
  solution <- matrix(result$solution, nrow = 6, byrow = TRUE)
  colnames(solution) <- c("School 1", "School 2", "School 3")
  rownames(solution) <- c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6")
  print(solution)
  cat("Total busing cost: $", result$objval, "\n")
} else {
  cat("No optimal solution found. Status code:", result$status, "\n")
}


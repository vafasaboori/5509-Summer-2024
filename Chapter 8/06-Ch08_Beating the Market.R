# Clearing the Environment ----
rm(list=ls())

# Set working directory ----
setwd("~/Documents/5509 Summer 2024/Chapter 8")

# Load the GA and readxl packages
library(GA) # Genetic algorithms inspired by natural selection and genetics. 
library(readxl)

# Read the data from the Excel file
data <- read_excel("market.xlsx")

# Extract the relevant columns
stock_returns <- as.matrix(data[, 3:7])
market_returns <- data[, 8] # only one column

# Define the objective function
objective <- function(x) { # x is a vector representing stock portfolio return in each Q
  weights <- x / sum(x)  # Normalize vector x so that the sum of its elements equals 1.
  # Normalization ensures that the total investment is 100%.
  portfolio_returns <- stock_returns %*% weights  # Calculate the portfolio returns for each quarter
  #  %*% is the matrix multiplication operator in R. We are  performing a SUMPRODUCT operation.
  # The result is a vector of returns (computed as a weighted sum) for each quarter
  num_beat_market <- sum(portfolio_returns > market_returns)  # Count the number of quarters where the portfolio beats the market
  return(num_beat_market)
}

# Define the portfolio weights' bounds
lower_bound <- rep(0, 5) # vector of zeros with a length of 5 
upper_bound <- rep(1, 5) # vector of ones with a length of 5 

# Run the genetic algorithm optimization
result <- ga(
  type = "real-valued", # Problem type: real-valued variables
                        # portfolio weights can take any real value within bounds.
  fitness = objective,  # Objective function
  lower = lower_bound,  # Lower bounds for portfolio weights
  upper = upper_bound,  # Upper bounds for portfolio weights
  popSize = 500,  # Sets the population size
  maxiter = 5000,  # Sets the maximum number of generations
  run = 500  # Sets the number of independent runs
)

# Extract the optimal solution and objective value
optimal_weights <- result@solution[1, ]  
                        # Extract the optimal portfolio weights (first row)
optimal_weights <- optimal_weights / sum(optimal_weights)  
                        # Normalize the weights to sum up to 1
optimal_num_beat_market <- result@fitnessValue[1]  
                        # Extract the optimized obj fun (max No. beating the market)

# Print the optimal solution and objective value
cat("Optimal Portfolio Weights:", optimal_weights, "\n")
cat("Number of Quarters Beating the Market:", optimal_num_beat_market, "\n")


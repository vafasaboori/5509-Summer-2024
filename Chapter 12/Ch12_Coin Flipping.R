# Clearing the Environment ----
rm(list=ls())

# Function to simulate the game and return the number of flips required
simulate_game <- function() 
{
  flips <- 0 # number of flips required to reach a difference of three 
  heads <- 0
  tails <- 0
  
  while (abs(heads - tails) < 3) 
  {
    flips <- flips + 1
    coin <- sample(c("H", "T"), 1, replace = TRUE)
    
    if (coin == "H") 
    {
      heads <- heads + 1
    } 
    else 
    {
      tails <- tails + 1
    }
  }
  
  return(flips) # Once the condition is met, exits the loop and returns flips
  
}

num_simulations <- 100000

total_flips <- 0
total_winnings <- 0

for (i in 1:num_simulations) 
{
  flips <- simulate_game()
  winnings <- 8 - flips
  
  total_flips <- total_flips + flips
  total_winnings <- total_winnings + winnings
}

average_flips <- total_flips / num_simulations
average_winnings <- total_winnings / num_simulations

cat("Average Number of Flips:", average_flips, "\n")
cat("Average Amount Won:", average_winnings, "\n")


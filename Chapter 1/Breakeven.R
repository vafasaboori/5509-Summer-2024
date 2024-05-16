# Clearing the Environment ----
rm(list=ls())

# Set Unit Revenue and Marginal Cost ----
unit_revenue <- 2000
marginal_cost <- 1000

# Create Quantity Column ----
quantity <- seq(0, 30000, length.out = 11)

# Create Cost Columns ----
fixed_cost <- rep(10000000, length(quantity))
variable_cost <- marginal_cost * quantity
total_cost <- fixed_cost + variable_cost

# Create Revenue Column ----
revenue <- unit_revenue * quantity

# Build data frame ----
df <- data.frame(quantity, fixed_cost, variable_cost, total_cost, revenue)

# Obtain break even quantity ----
break_even_quantity <- fixed_cost[1] / (unit_revenue - marginal_cost)
break_even_quantity

# Import ggplot2 and Tidyverse Package ----
library(ggplot2)
library(tidyverse)

# Build Break Even Point Graph ----
df %>%
  ggplot(aes(x = quantity)) +
  geom_line(aes(y = fixed_cost, col = "Fixed Cost"), size = 2) +
  geom_line(aes(y = variable_cost, col = "Variable Cost"), size = 2) +
  geom_line(aes(y = total_cost, col = "Total Cost"), size = 2) +
  geom_line(aes(y = revenue, col = "Revenue"), size = 2) +
  geom_point(aes(x = break_even_quantity, y = break_even_quantity * unit_revenue),
             colour = "tomato", size = 10) +
  labs(x = "Quantity", y = "Dollars")


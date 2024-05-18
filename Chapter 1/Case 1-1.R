# Clearing the environment ----
rm(list=ls())

# Part a to c)  break-even analysis ----

# Set constants
unit_revenue <- 4500
marginal_cost <- 2000
fixed_cost <- 250000

# Create quantity column
quantity <- seq(0, 500, length.out = 11)

# Calculate variable cost, total cost, and revenue
variable_cost <- marginal_cost * quantity
total_cost <- fixed_cost + variable_cost
revenue <- unit_revenue * quantity

# Build the data frame
df <- data.frame(quantity, fixed_cost, variable_cost, total_cost, revenue)

# Obtain the break-even quantity
break_even_quantity <- fixed_cost / (unit_revenue - marginal_cost)
break_even_quantity

# Part d) Build the Break Even Graph -----
library(ggplot2)

ggplot(data = df, aes(x = quantity)) +
  geom_line(aes(y = fixed_cost, color = "Fixed Cost")) +
  geom_line(aes(y = variable_cost, color = "Variable Cost")) +
  geom_line(aes(y = total_cost, color = "Total Cost")) +
  geom_line(aes(y = revenue, color = "Revenue")) +
  geom_point(aes(x = break_even_quantity, y = break_even_quantity * unit_revenue),
             color = "black", size = 2) +
  labs(x = "Quantity", y = "Cost/Revenue")


# Part f: If Q = 300 What is Max Fixed Cost? ----
# Q = Fixed Cost / (Unit Revenue - Marginal Cost)
# FC = Q * (UR - MC)

FC <- 300 * (unit_revenue - marginal_cost)
FC

# Part g: If Q = 300 What is Max Marginal cost? ----
# Q = Fixed Cost / (Unit Revenue - Marginal Cost)
# MC = (Q * UR - FC) / Q

MC <- (300 * unit_revenue - fixed_cost) / 300
MC

# Part h: IF MC and FC up by 50%, and Q=300, Profitable? ----
# Set Marginal Cost 2000*1.5 = 3000
# Set Fixed Cost 250,000 * 1.5 = 375,000

marginal_cost <- 3000
fixed_cost <- 375000

variable_cost <- marginal_cost * quantity
total_cost <- fixed_cost + variable_cost
revenue <- unit_revenue * quantity
df <- data.frame(quantity, fixed_cost, variable_cost, total_cost, revenue)
break_even_quantity <- fixed_cost / (unit_revenue - marginal_cost)
break_even_quantity

# Build the Break Even Graph
ggplot(data = df, aes(x = quantity)) +
  geom_line(aes(y = fixed_cost, color = "Fixed Cost")) +
  geom_line(aes(y = variable_cost, color = "Variable Cost")) +
  geom_line(aes(y = total_cost, color = "Total Cost")) +
  geom_line(aes(y = revenue, color = "Revenue")) +
  geom_point(aes(x = break_even_quantity, y = break_even_quantity * unit_revenue),
             color = "black", size = 4) +
  labs(x = "Quantity", y = "Cost/Revenue")

# BE = 250, since Sales forecast is 300, it would still be profitable.

# Part i: Min selling price (Unit Revenue) at which still profitable ----
# Q = Fixed Cost / (Unit Revenue - Marginal Cost)
# UR = (Q * MC + FC) / Q

# Set constants back to original values (analysis is done independently)
unit_revenue <- 4500
marginal_cost <- 2000
fixed_cost <- 250000

UR <- (300 * marginal_cost + fixed_cost) / 300
UR

# Part j: 300 produced, 200 sold, profitable? ----
Profit <- unit_revenue * 200 - (fixed_cost + marginal_cost * 300)
Profit

# Yes, Still profitable

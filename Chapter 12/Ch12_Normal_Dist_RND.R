# Clearing the Environment ----
rm(list=ls())

# Generate random numbers from a normal distribution ----
n <- 10000  # Number of random numbers
mean_val <- 1.5  # Mean of the distribution
sd_val <- 0.7  # Standard deviation of the distribution

# Generate random numbers
random_numbers <- rnorm(n, mean = mean_val, sd = sd_val)

# Plotting the histogram with a density estimate ----
hist(random_numbers, freq = FALSE, 
     # freq = FALSE vertical axis densities (proportions), not frequencies (counts).
     main = "Normal Distribution", xlab = "Random Numbers")

# Add a density curve
curve(dnorm(x, mean = mean_val, sd = sd_val), add = TRUE, col = "blue", lwd = 2)
# add = TRUE indicates overlaying the plotted object onto an existing plot 


# Alternative: Using ggplot2 for plotting ----
library(ggplot2)
df <- data.frame(x = random_numbers)
ggplot(df, aes(x)) + 
geom_histogram(aes(y = ..density..),
               # y = ..density.. normalizes vertical axis to show proportions (densities)
               bins = 30, color = "black", fill = "white") +
geom_density(alpha = 0.2, fill = "#FF6666") +
  # geom_density() adds a density plot to visualize the distribution of a variable
ggtitle("Normal Distribution") +
xlab("Random Numbers") +
theme_minimal()


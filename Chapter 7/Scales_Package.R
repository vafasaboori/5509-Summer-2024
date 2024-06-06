# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(scales)

# Format a variety of numbers

# Format as currency
currency_example <- dollar(1234567.89)
cat("Currency:", currency_example, "\n")
# Output: "$1,234,567.89"

# Format as percent
percent_example <- percent(0.1234, accuracy = 0.01)
cat("Percent:", percent_example, "\n")
# Output: "12.34%"

# Format with commas
comma_example <- comma(1234567.89)
cat("Comma:", comma_example, "\n")
# Output: "1,234,567.89"

# Format as ordinal
ordinal_example <- ordinal(4)
cat("Ordinal:", ordinal_example, "\n")
# Output: "4th"

# Format in scientific notation
scientific_example <- scientific(1234567)
cat("Scientific:", scientific_example, "\n")
# Output: "1.23e+06"

# Format a date
date_example <- date_format("%B %d, %Y")(as.Date("2023-06-05"))
cat("Date:", date_example, "\n")
# Output: "June 05, 2023"


# Clearing the Environment ----
rm(list=ls())

day <- "Monday"
message <- switch(day,
                  "Monday" = "Start of the week",
                  "Tuesday" = "Second day",
                  "Wednesday" = "Middle of the week",
                  "Thursday" = "Almost there",
                  "Friday" = "End of the week",
                  "Saturday" = "Weekend",
                  "Sunday" = "Weekend",
                  "Unknown day")
print(message)


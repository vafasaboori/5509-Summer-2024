# Clearing the Environment ----
rm(list=ls())

# Load the required package ----
library(TSP)

# Define the cities and their distances -----
cities <- c("SEA", "ANA", "BAL", "BOS", "CHI", "CLE", "DET", "HOU", "KC", "MIN", "NY", "OAK", "TB", "TEX", "TOR")

distances <- matrix(c(
  0,    1134, 2708, 3016, 2052, 2391, 2327, 2309, 1858, 1653, 2841, 810,  3077, 2131, 2564,
  1134, 0,    2647, 3017, 2048, 2382, 2288, 1532, 1577, 1857, 2794, 387,  2490, 1399, 2523,
  2708, 2647, 0,    427,  717,  358,  514,  1446, 1070, 1113, 199,  2623, 950,  1357, 457,
  3016, 3017, 427,  0,    994,  657,  799,  1848, 1435, 1390, 222,  3128, 1293, 1753, 609,
  2052, 2048, 717,  994,  0,    348,  279,  1083, 542,  410,  809,  2173, 1160, 921,  515,
  2391, 2382, 358,  657,  348,  0,    172,  1297, 819,  758,  471,  2483, 1108, 1189, 296,
  2327, 2288, 514,  799,  279,  172,  0,    1302, 769,  685,  649,  2399, 1184, 1156, 240,
  2309, 1532, 1446, 1848, 1083, 1297, 1302, 0,    742,  1176, 1628, 1917, 981,  256,  1530,
  1858, 1577, 1070, 1435, 542,  819,  769,  742,  0,    443,  1233, 1681, 1171, 505,  1006,
  1653, 1857, 1113, 1390, 410,  758,  685,  1176, 443,  0,    1217, 1979, 1573, 949,  906,
  2841, 2794, 199,  222,  809,  471,  649,  1628, 1233, 1217, 0,    2930, 1150, 1559, 516,
  810,  387,  2623, 3128, 2173, 2483, 2399, 1917, 1681, 1979, 2930, 0,    2823, 1752, 2627,
  3077, 2490, 950,  1293, 1160, 1108, 1184, 981,  1171, 1573, 1150, 2823, 0,    1079, 1348,
  2131, 1399, 1357, 1753, 921,  1189, 1156, 256,  505,  949,  1559, 1752, 1079, 0,    1435,
  2564, 2523, 457,  609,  515,  296,  240,  1530, 1006, 906,  516,  2627, 1348, 1435, 0
), nrow = length(cities), ncol = length(cities), byrow = TRUE)

# Create a TSP object ----
tsp <- TSP(distances) # Initializes a TSP object with given distance matrix.
                      # The TSP object contains methods of finding the optimal tour

# Set the start and end points as Seattle
start_city <- "SEA"
start_index <- match(start_city, cities) 
# search for the position of the start_city in the cities vector and returns its index. 

# Solve the TSP using the nearest neighbor algorithm
solution <- solve_TSP(tsp, method = "nearest_insertion", start = start_index)
# The nearest neighbor algorithm is a simple heuristic approach
# It repeatedly selects the nearest un-visited city as the next destination until all cities have been visited.

# Get the optimal tour
optimal_tour <- cities[solution]  # "cities" vector is indexed using the "solution" vector
                                  # "solution" vector has the order of cities to visit

# Complete the tour by returning to the starting city
optimal_tour <- c(optimal_tour, start_city)

# Print the optimal tour
cat("Optimal Tour: ", paste(optimal_tour, collapse = " -> "), "\n")
# collapse parameter in the paste function specifies the separator between the elements

# Load libraries

library(ggplot2)
library(maps)


# Get map data of the United States
us_map <- map_data("state")

# Create a data frame with city coordinates
city_coords <- data.frame(
  city = cities,
  x = c(-122.33, -117.87, -76.61, -71.06, -87.63, -81.69, -83.05, -95.36, -94.58, -93.27, -74.01, -122.27, -82.46, -97.74, -79.38),
  y = c(47.61, 33.68, 39.29, 42.36, 41.88, 41.50, 42.33, 29.76, 39.10, 44.98, 40.71, 37.80, 27.96, 32.75, 43.65)
) # latitude and longitude of cities

# Create a data frame with tour coordinates
tour_coords <- city_coords[solution, ] # "city_coords" data-frame is indexed using the "solution" vector 
                                       # The , after solution is to select all columns in the data frame
                                       # "solution" vector has the order of cities to visit

# Create the plot
ggplot() +
  geom_map(data = us_map, 
           map = us_map,
           aes(map_id = region), fill = "lightgray", color = "black", size = 0.2) +
  geom_point(data = city_coords, aes(x, y), size = 3, color = "black") +
  geom_path(data = tour_coords, aes(x, y, group = 1), size = 1, color = "red") +
  geom_text(data = city_coords, aes(x, y, label = city), size = 3, vjust = -1) +
  labs(x = "Longitude", y = "Latitude", title = "Cities on the Map") +
  theme_minimal()

# region is aesthetic mapping for the regions or states of the United States.


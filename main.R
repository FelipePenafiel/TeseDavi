# Main script for data processing and regression analysis

# Load required libraries
source(file.path("R", "functions", "load_libraries.R"))
load_libraries()

# Run regressions
source(file.path("R", "perform_regressions.R"))

# Explore the results
source(file.path("R", "result_analysis.R"))
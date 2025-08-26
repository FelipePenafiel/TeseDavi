# Main script for data processing and regression analysis

# Load required libraries
source(file.path("functions", "load_libraries.R"))
load_libraries()

# Build the unified dataset
source(file.path("R", "merge_tables.R"))

# Run regression analyses
source(file.path("R", "regression_analyses.R"))

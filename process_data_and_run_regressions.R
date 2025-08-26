# Main script for data processing and regression analysis

# Load required libraries
library(readxl)
library(dplyr)
library(writexl)
library(MASS)
library(car)
library(ggplot2)
library(tidyr)
library(openxlsx)

# Build the unified dataset
source(file.path("R", "merge_tables.R"))

# Run regression analyses
source(file.path("R", "regression_analyses.R"))

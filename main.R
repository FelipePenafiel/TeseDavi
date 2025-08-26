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

# Build the data
source(file.path("R", "Merge_Table.R"))

# Run regression analyses
source(file.path("R", "Regressoes_Analises.R"))

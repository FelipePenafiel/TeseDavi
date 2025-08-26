# Forest Regression Analysis

This repository contains R scripts for building a consolidated dataset and running several regression models to analyze aboveground biomass (AGB) in forest plots.

## Requirements

The analyses are written for R. The following R packages are required:

- readxl
- dplyr
- writexl
- MASS
- car
- ggplot2
- tidyr
- openxlsx
- lmtest

## Project Structure

- `main.R` – orchestrates the workflow: loads libraries, builds the dataset, runs regressions and explores results.
- `R/build_data.R` – combines raw data files into a unified table saved to `data/derived`.
- `R/perform_regressions.R` – fits linear, multiple and non-linear (Gompertz and Schnute) models and produces diagnostics.
- `R/result_analysis.R` – utilities for inspecting and plotting regression results.
- `data/raw` – place raw input files such as `agb_parcela_results.csv` and `Database_Consolidado_Corrigido.xlsx`.
- `data/derived` – generated datasets like `tabela_unificada.xlsx`.

## Usage

1. Add the necessary raw data files to `data/raw/`.
2. Install the dependencies by sourcing the helper function:

   ```r
   source("R/functions/load_libraries.R")
   load_libraries()
   ```
3. Run the complete workflow:

   ```bash
   Rscript main.R
   ```

The script will build the derived dataset, run the regression models and generate diagnostic plots.

## Notes

The repository does not ship with the original raw data. Only derived products created by running the scripts will appear under `data/derived`.

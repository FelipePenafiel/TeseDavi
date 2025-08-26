# Result analysis functions

source("R/functions/load_libraries.R")
source("R/functions/analysis_utils.R")

# Load required packages
default_packages <- load_libraries()

# Directories
raw_dir <- file.path("data", "raw")
derived_dir <- file.path("data", "derived")

#' Run simple linear regression analysis
#'
#' @param dados Data frame returned by `load_and_prepare_data`
#' @return A list containing fitted models, outliers and plots
run_linear_simple <- function(dados) {
  modelo <- lm(AGB ~ Age, data = dados)
  outliers <- identify_linear_outliers(modelo, dados)
  dados_limpos <- dados[-outliers$combined, ]
  modelo_limpo <- lm(AGB ~ Age, data = dados_limpos)
  list(
    original_model = modelo,
    cleaned_model = modelo_limpo,
    outliers = outliers,
    plots = list(
      with_outliers = plot_linear_fit(dados, modelo, "Modelo de Regressão Linear Simples com Outliers"),
      without_outliers = plot_linear_fit(dados_limpos, modelo_limpo, "Modelo de Regressão Linear Simples sem Outliers")
    ),
    metrics = list(
      original = compute_linear_metrics(modelo),
      cleaned = compute_linear_metrics(modelo_limpo)
    ),
    data_without_outliers = dados_limpos
  )
}

#' Run multiple linear regression analysis
#'
#' @param dados Data frame returned by `load_and_prepare_data`
#' @return A list with models and detected outliers
run_linear_multiple <- function(dados) {
  modelo <- lm(AGB ~ Age + Temperature + Precipitation, data = dados)
  outliers <- identify_linear_outliers(modelo, dados)
  dados_limpos <- dados[-outliers$combined, ]
  modelo_limpo <- lm(AGB ~ Age + Temperature + Precipitation, data = dados_limpos)
  list(
    original_model = modelo,
    cleaned_model = modelo_limpo,
    outliers = outliers,
    metrics = list(
      original = compute_linear_metrics(modelo),
      cleaned = compute_linear_metrics(modelo_limpo)
    ),
    data_without_outliers = dados_limpos
  )
}

#' Run Schnute regression analysis
#'
#' @param dados Data frame returned by `load_and_prepare_data`
#' @return A list with models and outlier indices
run_schnute <- function(dados) {
  modelo <- nls(
    AGB ~ (BETA * (1 - exp(-A * Age))) ^ THETA,
    data = dados,
    start = list(BETA = 9.2, A = 0.19, THETA = 2.9),
    algorithm = "port"
  )
  outliers <- identify_residual_outliers(modelo)
  dados_limpos <- dados[-outliers, ]
  modelo_limpo <- nls(
    AGB ~ (BETA * (1 - exp(-A * Age))) ^ THETA,
    data = dados_limpos,
    start = list(BETA = 9.2, A = 0.19, THETA = 2.9),
    algorithm = "port"
  )
  list(
    original_model = modelo,
    cleaned_model = modelo_limpo,
    outliers = outliers,
    data_without_outliers = dados_limpos
  )
}

#' Run Gompertz regression analysis
#'
#' @param dados Data frame returned by `load_and_prepare_data`
#' @return A list with models and outlier indices
run_gompertz <- function(dados) {
  modelo <- nls(
    AGB ~ BETA * exp(-exp(-A * (Age - THETA))),
    data = dados,
    start = list(BETA = 9.2, A = 0.19, THETA = 2.9),
    algorithm = "port"
  )
  outliers <- identify_residual_outliers(modelo)
  dados_limpos <- dados[-outliers, ]
  modelo_limpo <- nls(
    AGB ~ BETA * exp(-exp(-A * (Age - THETA))),
    data = dados_limpos,
    start = list(
      BETA = max(dados_limpos$AGB, na.rm = TRUE),
      A = 0.1,
      THETA = mean(dados_limpos$Age, na.rm = TRUE)
    ),
    algorithm = "port"
  )
  list(
    original_model = modelo,
    cleaned_model = modelo_limpo,
    outliers = outliers,
    data_without_outliers = dados_limpos
  )
}

# Example usage (uncomment to run):
# dados <- load_and_prepare_data(derived_dir)
# resultado_linear <- run_linear_simple(dados)

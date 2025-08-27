# Helper functions for result analysis

#' Load and prepare forest data
#'
#' @param derived_dir Path to derived data directory
#' @param numeric_cols Columns to convert to numeric
#' @param missing_value Placeholder for missing values
#' @return A data.frame with numeric columns converted and missing values as NA
load_and_prepare_data <- function(derived_dir,
                                  numeric_cols = c("AGB", "Age", "Temperature", "Precipitation"),
                                  missing_value = -9999) {
  dados <- read_excel(file.path(derived_dir, "tabela_unificada.xlsx"), col_types = "text")
  dados[numeric_cols] <- lapply(dados[numeric_cols], function(col) {
    as.numeric(gsub(",", ".", col))
  })
  dados[dados == missing_value] <- NA
  dados
}

#' Identify outliers in linear models
#'
#' Combines Cook's distance, leverage and Bonferroni corrected studentized residuals.
#'
#' @param model Fitted linear model
#' @param data Data used to fit the model
#' @return A list with indices detected by each method and the union of all indices
identify_linear_outliers <- function(model, data) {
  distancia_cook <- cooks.distance(model)
  limiar_cook <- 4 / (nrow(data) - length(coef(model)))
  outliers_cook <- which(distancia_cook > limiar_cook)

  valores_hat <- hatvalues(model)
  limiar_leverage <- 2 * mean(valores_hat)
  outliers_leverage <- which(valores_hat > limiar_leverage)

  residuos_studentizados <- rstudent(model)
  n <- nrow(data)
  p_valores <- 2 * (1 - pt(abs(residuos_studentizados), df = n - length(coef(model))))
  p_bonferroni <- p.adjust(p_valores, method = "bonferroni")
  outliers_bonferroni <- which(p_bonferroni < 0.05)

  lista_indices <- list(
    cook = outliers_cook,
    leverage = outliers_leverage,
    bonferroni = outliers_bonferroni
  )
  lista_indices$combined <- unique(unlist(lista_indices))
  lista_indices
}

#' Identify outliers based on standardised residuals
#'
#' @param model Fitted model object
#' @param threshold Cut-off for absolute standardised residuals
#' @return Indices of observations considered outliers
identify_residual_outliers <- function(model, threshold = 2) {
  residuos <- residuals(model)
  residuos_padronizados <- residuos / sd(residuos, na.rm = TRUE)
  which(abs(residuos_padronizados) > threshold)
}

#' Plot a simple linear fit
#'
#' @param data Data frame
#' @param model Linear model
#' @param title Plot title
#' @return A ggplot object
plot_linear_fit <- function(data, model, title) {
  ggplot(data, aes(x = Age, y = AGB)) +
    geom_point(color = "blue", size = 2) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    labs(
      title = title,
      x = "Idade do Plantio (Anos)",
      y = "Biomassa Acima do Solo (t MS/ha)"
    ) +
    theme_minimal()
}

#' Compute standard metrics for linear models
#'
#' @param model Fitted linear model
#' @return A named list with R2, adjusted R2, AIC and RMSE
compute_linear_metrics <- function(model) {
  list(
    r2 = summary(model)$r.squared,
    adj_r2 = summary(model)$adj.r.squared,
    aic = AIC(model),
    rmse = sqrt(mean(residuals(model)^2))
  )
}

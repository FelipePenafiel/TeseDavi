#' Utilities for regression analysis
#'
#' Functions to fit regression models with diagnostics and compute
#' common performance metrics.

#' Run regression analysis with diagnostics
#'
#' @param data Data frame containing model variables
#' @param formula Model formula (ignored for some non-linear models)
#' @param model_name Identifier where the prefix indicates the model type
#' @return A list with the fitted model, diagnostics and model type
run_regression_analysis <- function(data, formula, model_name) {
  # Determinando o tipo de modelo com base no nome
  model_type <- unlist(strsplit(model_name, "_"))[1]

  # Ajustando o modelo de acordo com o tipo
  model <- switch(model_type,
                  "Linear" = lm(formula, data = data),
                  "Multipla" = lm(formula, data = data),
                  "Gompertz" = {
                    start_vals <- list(BETA = max(data$AGB, na.rm = TRUE), ALPHA = 0.1, GAMMA = 1)
                    tryCatch(
                      nls(AGB ~ BETA * exp(-exp(GAMMA - ALPHA * Age)),
                          data = data,
                          start = start_vals,
                          algorithm = "port",
                          control = nls.control(maxiter = 1000, warnOnly = TRUE)),
                      error = function(e) {
                        message("Erro no modelo Gompertz: ", e$message)
                        return(NULL)
                      }
                    )
                  },
                  "Schnute" = {
                    start_vals <- list(BETA = max(data$AGB, na.rm = TRUE), A = 0.1, THETA = 1)
                    tryCatch(
                      nls(AGB ~ (BETA * (1 - exp(-A * Age)))^THETA,
                          data = data,
                          start = start_vals,
                          algorithm = "port",
                          lower = c(BETA = 0, A = 0, THETA = 0.01),
                          control = nls.control(maxiter = 1000, warnOnly = TRUE)),
                      error = function(e) {
                        message("Erro no modelo Schnute: ", e$message)
                        return(NULL)
                      }
                    )
                  }
  )

  # Verificando se o modelo foi Ajustada corretamente
  if (is.null(model)) {
    return(NULL)
  }

  # Mostrando um resumo do modelo
  print(summary(model))

  # Criando uma lista para armazenar os diagnósticos
  diagnostics <- list(summary = summary(model))

  if (model_type %in% c("Linear", "Multipla")) {
    # Verificando possíveis outliers usando o teste de Bonferroni
    diagnostics$outlier_test <- tryCatch(
      outlierTest(model),
      error = function(e) {
        message("Erro no teste de outliers para ", model_name, ": ", e$message)
        return(NULL)
      }
    )

    if (!is.null(diagnostics$outlier_test)) {
      print(diagnostics$outlier_test)
    }

    # Calculando a distância de Cook
    diagnostics$cooksd <- cooks.distance(model)

    # Plotando a distância de Cook
    plot(diagnostics$cooksd, type = "h", main = paste("Distância de Cook -", model_name), ylab = "Distância de Cook")
    abline(h = 4 / nrow(data), col = "red")

    # Calculando os valores de alavancagem
    diagnostics$hat_values <- hatvalues(model)

    # Plotando os valores de alavancagem
    plot(diagnostics$hat_values, type = "h", main = paste("Valores de Alavancagem -", model_name), ylab = "Valores de Hat")
    abline(h = (2 * length(coef(model))) / nrow(data), col = "red")

    # Identificando pontos influentes
    influ_threshold_cook <- 4 / nrow(data)
    influ_threshold_leverage <- (2 * length(coef(model))) / nrow(data)

    influente_cook <- which(diagnostics$cooksd > influ_threshold_cook)
    influente_leverage <- which(diagnostics$hat_values > influ_threshold_leverage)
    influentes <- unique(c(influente_cook, influente_leverage))
    diagnostics$influential_points <- influentes

    # Plotando resíduos versus valores Ajustadas
    plot(model$fitted.values, resid(model),
         xlab = "Valores Ajustadas",
         ylab = "Resíduos",
         main = paste("Resíduos vs Ajustadas -", model_name))
    abline(h = 0, col = "red")

  } else {
    # Analisando resíduos para modelos não lineares
    diagnostics$residuals <- residuals(model)
    diagnostics$fitted <- fitted(model)

    # Resíduos padronizados
    res_padronizados <- diagnostics$residuals / sd(diagnostics$residuals)

    # Plotando resíduos versus valores Ajustadas
    plot(diagnostics$fitted, diagnostics$residuals,
         xlab = "Valores Ajustadas",
         ylab = "Resíduos",
         main = paste("Resíduos vs Ajustadas -", model_name))
    abline(h = 0, col = "red")

    # Histograma dos resíduos
    hist(diagnostics$residuals, main = paste("Histograma dos Resíduos -", model_name), xlab = "Resíduos")

    # QQ-plot dos resíduos
    qqnorm(diagnostics$residuals, main = paste("QQ-Plot dos Resíduos -", model_name))
    qqline(diagnostics$residuals, col = "red")

    # Identificando possíveis outliers
    possiveis_outliers <- which(abs(res_padronizados) > 2)
    diagnostics$possible_outliers <- possiveis_outliers

    if (length(possiveis_outliers) > 0) {
      print(possiveis_outliers)
    }
  }

  # Retornando os resultados do modelo e os diagnósticos
  list(model = model, diagnostics = diagnostics, model_type = model_type)
}

#' Calculate model performance metrics
#'
#' @param model Fitted model object
#' @param data Data used for fitting
#' @param model_type Type of model ("Linear", "Multipla", "Gompertz" or "Schnute")
#' @return A list with R-squared, adjusted R-squared and AIC
calculate_model_metrics <- function(model, data, model_type) {
  if (model_type %in% c("Linear", "Multipla")) {
    r2 <- summary(model)$r.squared
    r2_adj <- summary(model)$adj.r.squared
    aic_val <- AIC(model)
  } else {
    ss_res <- sum(residuals(model)^2)
    ss_tot <- sum((data$AGB - mean(data$AGB, na.rm = TRUE))^2)
    n <- nrow(data)
    p <- length(coef(model))
    r2 <- 1 - (ss_res / ss_tot)
    r2_adj <- 1 - ((ss_res / (n - p)) / (ss_tot / (n - 1)))
    aic_val <- AIC(model)
  }

  list(R_squared = r2, Adjusted_R_squared = r2_adj, AIC = aic_val)
}

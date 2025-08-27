#Limpar tudo
rm(list = ls(all=TRUE))
gc()

# Definindo diretórios de dados
raw_dir <- file.path("data", "raw")
derived_dir <- file.path("data", "derived")

# Carregando os dados usando a função utilitária
source("R/functions/load_and_prepare_data.R")
source("R/functions/regression_utils.R")
forest_data <- load_and_prepare_data(file.path(raw_dir, "tabela_unificada.xlsx"))

# Lista para guardar os resultados de cada modelo
results_list <- list()

# 1. Regressão Linear Simples
dados_linear <- na.omit(forest_data[, c("AGB", "Age")])
formula_linear <- AGB ~ Age

# Ajustando o modelo inicial
linear_inicial <- run_regression_analysis(dados_linear, formula_linear, "Linear_Initial")
if (!is.null(linear_inicial)) {
  metrics_linear_inicial <- calculate_model_metrics(linear_inicial$model, dados_linear, "Linear")

  # Removendo pontos influentes e ajustando novamente, se necessário
  if (!is.null(linear_inicial$diagnostics$influential_points) && length(linear_inicial$diagnostics$influential_points) > 0) {
    dados_linear_Ajustada <- dados_linear[-linear_inicial$diagnostics$influential_points, ]
    linear_Ajustada <- run_regression_analysis(dados_linear_Ajustada, formula_linear, "Linear_Adjusted")
    metrics_linear_Ajustada <- if (!is.null(linear_Ajustada)) calculate_model_metrics(linear_Ajustada$model, dados_linear_Ajustada, "Linear") else NULL
  } else {
    linear_Ajustada <- linear_inicial
    metrics_linear_Ajustada <- metrics_linear_inicial
    dados_linear_Ajustada <- dados_linear
  }

  # Salvando os resultados
  results_list$Linear <- list(
    initial = list(model = linear_inicial$model, metrics = metrics_linear_inicial),
    adjusted = if (!is.null(linear_Ajustada)) list(model = linear_Ajustada$model, metrics = metrics_linear_Ajustada) else NULL
  )
}

# 2. Regressão Linear Múltipla
dados_multiplo <- na.omit(forest_data[, c("AGB", "Age", "Temperature", "Precipitation")])
formula_multiplo <- AGB ~ Age + Temperature + Precipitation

# Ajustando o modelo inicial
multiplo_inicial <- run_regression_analysis(dados_multiplo, formula_multiplo, "Multipla_Initial")
if (!is.null(multiplo_inicial)) {
  metrics_multiplo_inicial <- calculate_model_metrics(multiplo_inicial$model, dados_multiplo, "Multipla")

  # Removendo pontos influentes e ajustando novamente, se necessário
  if (!is.null(multiplo_inicial$diagnostics$influential_points) && length(multiplo_inicial$diagnostics$influential_points) > 0) {
    dados_multiplo_Ajustada <- dados_multiplo[-multiplo_inicial$diagnostics$influential_points, ]
    multiplo_Ajustada <- run_regression_analysis(dados_multiplo_Ajustada, formula_multiplo, "Multipla_Adjusted")
    metrics_multiplo_Ajustada <- if (!is.null(multiplo_Ajustada)) calculate_model_metrics(multiplo_Ajustada$model, dados_multiplo_Ajustada, "Multipla") else NULL
  } else {
    multiplo_Ajustada <- multiplo_inicial
    metrics_multiplo_Ajustada <- metrics_multiplo_inicial
    dados_multiplo_Ajustada <- dados_multiplo
  }

  # Salvando os resultados
  results_list$Multipla <- list(
    initial = list(model = multiplo_inicial$model, metrics = metrics_multiplo_inicial),
    adjusted = if (!is.null(multiplo_Ajustada)) list(model = multiplo_Ajustada$model, metrics = metrics_multiplo_Ajustada) else NULL
  )
}

# 3. Regressão de Gompertz
dados_gompertz <- na.omit(forest_data[, c("AGB", "Age")])

# Ajustando o modelo inicial
gompertz_inicial <- run_regression_analysis(dados_gompertz, NULL, "Gompertz_Initial")
if (!is.null(gompertz_inicial)) {
  metrics_gompertz_inicial <- calculate_model_metrics(gompertz_inicial$model, dados_gompertz, "Gompertz")

  # Removendo possíveis outliers e ajustando novamente, se necessário
  if (!is.null(gompertz_inicial$diagnostics$possible_outliers) && length(gompertz_inicial$diagnostics$possible_outliers) > 0) {
    dados_gompertz_Ajustada <- dados_gompertz[-gompertz_inicial$diagnostics$possible_outliers, ]
    gompertz_Ajustada <- run_regression_analysis(dados_gompertz_Ajustada, NULL, "Gompertz_Adjusted")
    metrics_gompertz_Ajustada <- if (!is.null(gompertz_Ajustada)) calculate_model_metrics(gompertz_Ajustada$model, dados_gompertz_Ajustada, "Gompertz") else NULL
  } else {
    gompertz_Ajustada <- gompertz_inicial
    metrics_gompertz_Ajustada <- metrics_gompertz_inicial
    dados_gompertz_Ajustada <- dados_gompertz
  }

  # Salvando os resultados
  results_list$Gompertz <- list(
    initial = list(model = gompertz_inicial$model, metrics = metrics_gompertz_inicial),
    adjusted = if (!is.null(gompertz_Ajustada)) list(model = gompertz_Ajustada$model, metrics = metrics_gompertz_Ajustada) else NULL
  )
}

# 4. Regressão de Schnute
dados_schnute <- na.omit(forest_data[, c("AGB", "Age")])

# Ajustando o modelo inicial
schnute_inicial <- run_regression_analysis(dados_schnute, NULL, "Schnute_Initial")
if (!is.null(schnute_inicial)) {
  metrics_schnute_inicial <- calculate_model_metrics(schnute_inicial$model, dados_schnute, "Schnute")

  # Removendo possíveis outliers e ajustando novamente, se necessário
  if (!is.null(schnute_inicial$diagnostics$possible_outliers) && length(schnute_inicial$diagnostics$possible_outliers) > 0) {
    dados_schnute_Ajustada <- dados_schnute[-schnute_inicial$diagnostics$possible_outliers, ]
    schnute_Ajustada <- run_regression_analysis(dados_schnute_Ajustada, NULL, "Schnute_Adjusted")
    metrics_schnute_Ajustada <- if (!is.null(schnute_Ajustada)) calculate_model_metrics(schnute_Ajustada$model, dados_schnute_Ajustada, "Schnute") else NULL
  } else {
    schnute_Ajustada <- schnute_inicial
    metrics_schnute_Ajustada <- metrics_schnute_inicial
    dados_schnute_Ajustada <- dados_schnute
  }

  # Salvando os resultados
  results_list$Schnute <- list(
    initial = list(model = schnute_inicial$model, metrics = metrics_schnute_inicial),
    adjusted = if (!is.null(schnute_Ajustada)) list(model = schnute_Ajustada$model, metrics = metrics_schnute_Ajustada) else NULL
  )
}

# Função para preparar os resultados para exportação no Excel
prepare_excel_data <- function(model_list, model_name) {
  extract_info <- function(entry) {
    if (is.null(entry)) return(NULL)
    coef_df <- data.frame(
      Parameter = names(coef(entry$model)),
      Estimate = coef(entry$model),
      stringsAsFactors = FALSE
    )
    metrics_df <- data.frame(
      Parameter = c("R_squared", "Adjusted_R_squared", "AIC"),
      Estimate = c(entry$metrics$R_squared, entry$metrics$Adjusted_R_squared, entry$metrics$AIC),
      stringsAsFactors = FALSE
    )
    rbind(coef_df, metrics_df)
  }

  list(
    initial = extract_info(model_list$initial),
    adjusted = extract_info(model_list$adjusted)
  )
}

# Preparando os dados para exportar para o Excel
excel_data <- list()
for (modelo in names(results_list)) {
  dados_excel <- prepare_excel_data(results_list[[modelo]], modelo)

  if (!is.null(dados_excel$initial)) {
    sheet_initial <- paste0(modelo, "_Initial")
    excel_data[[sheet_initial]] <- dados_excel$initial
  }

  if (!is.null(dados_excel$adjusted)) {
    sheet_adjusted <- paste0(modelo, "_Adjusted")
    excel_data[[sheet_adjusted]] <- dados_excel$adjusted
  }
}

# Exportando os resultados para um arquivo Excel
write.xlsx(excel_data, file = file.path(derived_dir, "Regression_Results.xlsx"), overwrite = TRUE)

# Criando uma sequência de valores para a variável Age, cobrindo o intervalo observado
age_seq <- seq(
  from = min(forest_data$Age, na.rm = TRUE),
  to = max(forest_data$Age, na.rm = TRUE),
  length.out = 100
)

# Montando um data frame para armazenar as previsões de todos os modelos
predictions_df <- data.frame(Age = age_seq)

# Loop para gerar previsões de cada modelo na 'results_list'
for (model_name in names(results_list)) {
  model_info <- results_list[[model_name]] # Pegando as informações do modelo

  for (version in names(model_info)) { # Iterando entre os modelos inicial e Ajustada
    version_info <- model_info[[version]]

    # Verificando se o modelo atual está disponível
    if (!is.null(version_info)) {
      current_model <- version_info$model
      label <- paste(model_name, ifelse(version == "initial", "Inicial", "Ajustada"))

      # Preparando novos dados para a previsão
      if (model_name == "Multipla") {
        # Para o modelo múltiplo, precisamos de Age, Temperature e Precipitation
        mean_Temperature <- mean(dados_multiplo$Temperature, na.rm = TRUE)
        mean_Precipitation <- mean(dados_multiplo$Precipitation, na.rm = TRUE)
        newdata <- data.frame(
          Age = age_seq,
          Temperature = mean_Temperature,
          Precipitation = mean_Precipitation
        )
      } else {
        # Para os outros modelos, só precisamos de Age
        newdata <- data.frame(Age = age_seq)
      }

      # Gerando as previsões usando o modelo atual
      predictions <- tryCatch(
        predict(current_model, newdata = newdata),
        error = function(e) {
          message("Erro ao prever para ", label, ": ", e$message)
          return(rep(NA, length(age_seq)))
        }
      )

      # Adicionando as previsões ao data frame
      predictions_df[[label]] <- predictions
    }
  }
}

# Transformando o data frame de previsões para o formato longo (necessário para o ggplot2)
predictions_long <- predictions_df %>%
  pivot_longer(
    cols = -Age, # Todas as colunas, exceto 'Age', serão convertidas
    names_to = "Modelo", # Nome da nova coluna que terá os rótulos dos modelos
    values_to = "AGB_Previsto" # Nome da coluna com os valores previstos
  ) %>%
  drop_na(AGB_Previsto) %>%  # Removendo linhas com valores NA
  mutate(AGB_Previsto = as.numeric(AGB_Previsto)) # Garantindo que seja numérico

# Criando o gráfico combinado com todas as curvas de previsão
plot_combinado <- ggplot(forest_data, aes(x = Age, y = AGB)) +
  geom_point(alpha = 0.6, color = "grey40") + # Dados observados
  geom_line(
    data = predictions_long,
    aes(x = Age, y = AGB_Previsto, color = Modelo),
    size = 1
  ) + # Curvas de previsão
  labs(
    title = "Comparação de Modelos de Regressão",
    x = "Idade do plantio",
    y = "Biomassa acima do solo (t MS/ha)",
    color = "Modelo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5) # Centralizando o título
  )

# Mostrando o gráfico combinado
print(plot_combinado)

# Gerando gráficos individuais para cada modelo (inicial e Ajustada)
for (model_name in names(results_list)) {
  model_info <- results_list[[model_name]] # Pegando as informações do modelo

  for (version in names(model_info)) { # Iterando entre versões inicial e ajustada
    version_info <- model_info[[version]]

    # Verificando se o modelo atual está disponível
    if (!is.null(version_info)) {
      current_model <- version_info$model
      label <- paste(model_name, ifelse(version == "initial", "Inicial", "Ajustada"))

      # Preparando novos dados para a previsão
      if (model_name == "Multipla") {
        # Para o modelo múltiplo, precisamos de Age, Temperature e Precipitation
        mean_Temperature <- mean(dados_multiplo$Temperature, na.rm = TRUE)
        mean_Precipitation <- mean(dados_multiplo$Precipitation, na.rm = TRUE)
        newdata <- data.frame(
          Age = age_seq,
          Temperature = mean_Temperature,
          Precipitation = mean_Precipitation
        )
      } else {
        # Para os outros modelos, só precisamos de Age
        newdata <- data.frame(Age = age_seq)
      }

      # Gerando previsões
      predictions <- tryCatch(
        predict(current_model, newdata = newdata),
        error = function(e) {
          message("Erro ao prever para ", label, ": ", e$message)
          return(rep(NA, length(age_seq)))
        }
      )

      # Preparando os dados para o gráfico individual
      plot_data <- data.frame(Age = age_seq, AGB_Previsto = predictions)

      # Criando o gráfico individual
      p <- ggplot(forest_data, aes(x = Age, y = AGB)) +
        geom_point(alpha = 0.6, color = "grey40") + # Dados observados
        geom_line(
          data = plot_data,
          aes(x = Age, y = AGB_Previsto),
          color = "blue",
          size = 1
        ) + # Curva de previsão
        labs(
          title = paste("Regressão", label),
          x = "Idade do plantio",
          y = "Biomassa acima do solo (t MS/ha)"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) # Centralizando o título

      # Mostrando o gráfico individual
      print(p)
    }
  }
}


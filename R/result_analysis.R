#Limpar tudo
rm(list = ls(all=TRUE))
gc()

# Configuração dos diretórios de dados
raw_dir <- file.path("data", "raw")
derived_dir <- file.path("data", "derived")

# Importação e preparação dos dados
source("R/functions/load_and_prepare_data.R")
source("R/functions/regression_utils.R")
dados_floresta <- load_and_prepare_data(file.path(derived_dir, "tabela_unificada.xlsx"))
dados_floresta_10 <- subset(dados_floresta, Age <= 10)

############################
# REGRESSÃO LINEAR SIMPLES
############################

# Ajuste inicial do modelo de regressão linear simples
linear_result <- run_regression_analysis(dados_floresta, AGB ~ Age, "Linear_Initial")
modelo_linear_simples <- linear_result$model

# Cálculo dos indicadores do modelo inicial (com outliers)
metrics_linear_inicial <- calculate_model_metrics(modelo_linear_simples, dados_floresta, "Linear")
r2_original <- metrics_linear_inicial$R_squared
r2_ajustado_original <- metrics_linear_inicial$Adjusted_R_squared
aic_original <- metrics_linear_inicial$AIC

cat("Indicadores do Modelo de Regressão Linear Simples (Com Outliers):\n")
cat("Coeficiente de Determinação (R²): ", r2_original, "\n")
cat("Coeficiente de Determinação Ajustado (R² Ajustado): ", r2_ajustado_original, "\n")
cat("Critério de Informação de Akaike (AIC): ", aic_original, "\n\n")

# Cálculo do RMSE (Root Mean Square Error)
rmse <- sqrt(mean(residuals(modelo_linear_simples)^2))
cat("Root Mean Square Error (RMSE): ", rmse, "\n")

# Gráficos diagnósticos do modelo inicial (apenas iustração dos pressupostos)
par(mfrow = c(2, 2))
plot(modelo_linear_simples)
par(mfrow = c(1, 1))

# Teste de Breusch-Pagan & Shapiro-Wilk
bptest(modelo_linear_simples)
shapiro.test(dados_floresta$AGB)

# Diagnóstico de Outliers com base nos critérios estatísticos
# 1. Distância de Cook - como se fosse resíduos padronizados, mas critério de seleção outro
distancia_cook <- linear_result$diagnostics$cooksd
limiar_cook <- 4 / (nrow(dados_floresta) - length(coef(modelo_linear_simples)))
outliers_cook <- which(distancia_cook > limiar_cook)

# 2. Alavancagem (Leverage)
valores_hat <- linear_result$diagnostics$hat_values
limiar_leverage <- 2 * mean(valores_hat)
outliers_leverage <- which(valores_hat > limiar_leverage)

# 3. Resíduos Studentizados com Correção de Bonferroni
residuos_studentizados <- rstudent(modelo_linear_simples)
n <- nrow(dados_floresta)
p_valores <- 2 * (1 - pt(abs(residuos_studentizados), df = n - length(coef(modelo_linear_simples))))
p_bonferroni <- p.adjust(p_valores, method = "bonferroni")
outliers_bonferroni <- which(p_bonferroni < 0.05)

# Identificação de Outliers Combinados
outliers_identificados <- unique(c(outliers_cook, outliers_leverage, outliers_bonferroni))

cat("Identificação de Outliers pelos Métodos de Diagnóstico:\n")
cat("- Outliers pela Distância de Cook:\n")
print(outliers_cook)
cat("- Outliers pela Alavancagem:\n")
print(outliers_leverage)
cat("- Outliers pela Correção de Bonferroni:\n")
print(outliers_bonferroni)
cat("- Conjunto Único de Outliers Identificados:\n")
print(outliers_identificados)
dados_floresta[outliers_leverage,]
dados_floresta[outliers_cook,]
dados_floresta[outliers_bonferroni,]

# Remoção de Outliers Identificados
dados_floresta[outliers_identificados, ]
dados_sem_outliers <- dados_floresta[-outliers_identificados, ]

# Reajuste do Modelo de Regressão Linear após Remoção de Outliers
linear_adjusted <- run_regression_analysis(dados_sem_outliers, AGB ~ Age, "Linear_Adjusted")
modelo_linear_simples_ajustado <- linear_adjusted$model

# Indicadores do Modelo Ajustado (Sem Outliers)
metrics_linear_ajustado <- calculate_model_metrics(modelo_linear_simples_ajustado, dados_sem_outliers, "Linear")
r2_ajustado <- metrics_linear_ajustado$R_squared
r2_ajustado_sem_outliers <- metrics_linear_ajustado$Adjusted_R_squared
aic_sem_outliers <- metrics_linear_ajustado$AIC

cat("Indicadores do Modelo de Regressão Linear Simples (Sem Outliers):\n")
cat("Coeficiente de Determinação (R²): ", r2_ajustado, "\n")
cat("Coeficiente de Determinação Ajustado (R² Ajustado): ", r2_ajustado_sem_outliers, "\n")
cat("Critério de Informação de Akaike (AIC): ", aic_sem_outliers, "\n\n")

# Cálculo do RMSE (Root Mean Square Error)
rmse <- sqrt(mean(residuals(modelo_linear_simples_ajustado)^2))
cat("Root Mean Square Error (RMSE): ", rmse, "\n")

# Resumo do Modelo Ajustado
cat("Resumo do Modelo Ajustado (Sem Outliers):\n")
print(summary(modelo_linear_simples_ajustado))

# Gráficos Diagnósticos do Modelo Ajustado
par(mfrow = c(2, 2))
plot(modelo_linear_simples_ajustado)
par(mfrow = c(1, 1))

# Teste de Breusch-Pagan & Shapiro-Wilk
bptest(modelo_linear_simples_ajustado)
shapiro.test(dados_sem_outliers$AGB)

# Visualização: Comparação entre os Dados Originais e Sem Outliers
dados_floresta$Dataset <- "Com Outliers"
dados_sem_outliers$Dataset <- "Sem Outliers"

grafico_com_outliers <- ggplot(data = dados_floresta, aes(x = Age, y = AGB)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Modelo de Regressão Linear Simples com Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)"
  ) +
  theme_minimal()

grafico_sem_outliers <- ggplot(data = dados_sem_outliers, aes(x = Age, y = AGB)) +
  geom_point(color = "green", size = 2) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Modelo de Regressão Linear Simples sem Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)"
  ) +
  theme_minimal()


# Gráfico comparativo
dados_floresta$Conjunto <- "Com Outliers"
dados_sem_outliers$Conjunto <- "Sem Outliers"

dados_combined <- rbind(dados_floresta, dados_sem_outliers)

grafico_comparativo <- ggplot(data = dados_combined, aes(x = Age, y = AGB, color = Conjunto)) +
  # Plotar os pontos dos dados
  geom_point(size = 2, alpha = 0.7) +
  # Adicionar as retas de regressão acima dos pontos
  geom_smooth(aes(linetype = Conjunto), method = "lm", se = FALSE, size = 1.2, color = "black") +
  labs(
    title = "Comparação de Modelos de Regressão Linear Simples: Com e Sem Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)",
    color = "Conjunto de Dados",
    linetype = "Conjunto de Dados"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Exibição dos Gráficos
print(grafico_com_outliers)
print(grafico_sem_outliers)
print(grafico_comparativo)

# Salvamento dos Dados Ajustados e do Modelo
write.csv(dados_sem_outliers, file.path(derived_dir, "dados_sem_outliers.csv"))


############################
# REGRESSÃO LINEAR MÚLTIPLA
############################
# Ajuste inicial do modelo de regressão linear múltipla
multiplo_result <- run_regression_analysis(dados_floresta, AGB ~ Age + Temperature + Precipitation, "Multipla_Initial")
modelo_linear_multiplo <- multiplo_result$model

#Stepwise
stepwise <- step(
  modelo_linear_multiplo,
  k = 2,
  trace = 1
)
summary(stepwise)

# Avaliação do modelo inicial (com outliers)
metrics_multiplo_inicial <- calculate_model_metrics(modelo_linear_multiplo, dados_floresta, "Multipla")
r2_original <- metrics_multiplo_inicial$R_squared
r2_ajustado_original <- metrics_multiplo_inicial$Adjusted_R_squared
aic_original <- metrics_multiplo_inicial$AIC

cat("Indicadores do Modelo de Regressão Linear Múltipla (Com Outliers):\n")
cat("Coeficiente de Determinação (R²): ", r2_original, "\n")
cat("Coeficiente de Determinação Ajustado (R² Ajustado): ", r2_ajustado_original, "\n")
cat("Critério de Informação de Akaike (AIC): ", aic_original, "\n\n")

# Gráficos diagnósticos do modelo inicial
par(mfrow = c(2, 2))
plot(modelo_linear_multiplo)
par(mfrow = c(1, 1))

# Diagnóstico de Outliers no Modelo de Regressão
# 1. Identificação com base na Distância de Cook
distancia_cook <- multiplo_result$diagnostics$cooksd
limiar_cook <- 4 / (nrow(dados_floresta) - length(coef(modelo_linear_multiplo)))
outliers_cook <- which(distancia_cook > limiar_cook)

# 2. Identificação com base na Alavancagem (Leverage)
valores_hat <- multiplo_result$diagnostics$hat_values
limiar_leverage <- 2 * mean(valores_hat)
outliers_leverage <- which(valores_hat > limiar_leverage)

# 3. Resíduos Studentizados com Correção de Bonferroni
residuos_studentizados <- rstudent(modelo_linear_multiplo)
n <- nrow(dados_floresta)
p_valores <- 2 * (1 - pt(abs(residuos_studentizados), df = n - length(coef(modelo_linear_multiplo))))
p_bonferroni <- p.adjust(p_valores, method = "bonferroni")
outliers_bonferroni <- which(p_bonferroni < 0.05)

# Combinação dos Outliers Identificados
outliers_identificados <- unique(c(outliers_cook, outliers_leverage, outliers_bonferroni))


# Exibição dos Outliers Identificados
cat("Outliers Identificados pelos Métodos de Diagnóstico:\n")
cat("- Distância de Cook:\n")
print(outliers_cook)
cat("- Alavancagem (Leverage):\n")
print(outliers_leverage)
cat("- Correção de Bonferroni:\n")
print(outliers_bonferroni)
cat("- Conjunto Único de Outliers Identificados:\n")
print(outliers_identificados)

# Remoção de Outliers Identificados do Conjunto de Dados
dados_floresta[outliers_identificados, ]
dados_sem_outliers <- dados_floresta[-outliers_identificados, ]

# Reajuste do Modelo de Regressão Linear Múltipla após Remoção de Outliers
multiplo_adjusted <- run_regression_analysis(dados_sem_outliers, AGB ~ Age + Temperature + Precipitation, "Multipla_Adjusted")
modelo_linear_multiplo_ajustado <- multiplo_adjusted$model

# Avaliação do Modelo Ajustado (Sem Outliers)
metrics_multiplo_ajustado <- calculate_model_metrics(modelo_linear_multiplo_ajustado, dados_sem_outliers, "Multipla")
r2_ajustado <- metrics_multiplo_ajustado$R_squared
r2_ajustado_sem_outliers <- metrics_multiplo_ajustado$Adjusted_R_squared
aic_sem_outliers <- metrics_multiplo_ajustado$AIC

cat("Indicadores do Modelo Ajustado (Sem Outliers):\n")
cat("Coeficiente de Determinação (R²): ", r2_ajustado, "\n")
cat("Coeficiente de Determinação Ajustado (R² Ajustado): ", r2_ajustado_sem_outliers, "\n")
cat("Critério de Informação de Akaike (AIC): ", aic_sem_outliers, "\n\n")

# Resumo do Modelo Ajustado
cat("Resumo do Modelo Ajustado (Sem Outliers):\n")
print(summary(modelo_linear_multiplo_ajustado))

# Gráficos Diagnósticos do Modelo Ajustado
par(mfrow = c(2, 2))
plot(modelo_linear_multiplo_ajustado)
par(mfrow = c(1, 1))

# Visualização Comparativa dos Dados com e sem Outliers
dados_floresta$Dataset <- "Com Outliers"
dados_sem_outliers$Dataset <- "Sem Outliers"

grafico_com_outliers <- ggplot(data = dados_floresta, aes(x = Age, y = AGB)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Modelo de Regressão Linear Múltipla com Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)"
  ) +
  theme_minimal()

grafico_sem_outliers <- ggplot(data = dados_sem_outliers, aes(x = Age, y = AGB)) +
  geom_point(color = "green", size = 2) +
  geom_smooth(method = "lm", color = "green", se = FALSE) +
  labs(
    title = "Modelo de Regressão Linear Múltipla sem Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)"
  ) +
  theme_minimal()

grafico_comparativo <- ggplot() +
  geom_point(data = dados_floresta, aes(x = Age, y = AGB, color = "Com Outliers"), size = 2) +
  geom_smooth(data = dados_floresta, aes(x = Age, y = AGB, color = "Com Outliers"),
              method = "lm", se = FALSE) +
  geom_point(data = dados_sem_outliers, aes(x = Age, y = AGB, color = "Sem Outliers"), size = 2) +
  geom_smooth(data = dados_sem_outliers, aes(x = Age, y = AGB, color = "Sem Outliers"),
              method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    title = "Comparação de Modelos de Regressão Linear Múltipla: Com e Sem Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)",
    color = "Conjunto de Dados"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Exibição dos Gráficos
print(grafico_com_outliers)
print(grafico_sem_outliers)
print(grafico_comparativo)

# Salvamento dos Dados Ajustados e do Modelo Final
write.csv(dados_sem_outliers, file.path(derived_dir, "modelo_linear_multiplo_dados_sem_outliers.csv"))



############################
# REGRESSÃO DE SCHNUTE
############################

# Ajuste inicial do modelo Schnute
# Este modelo é utilizado para ajustar a relação não-linear entre AGB e Age.
Schnute <- nls(
  AGB ~ (BETA * (1 - exp(-A * Age))) ^ THETA,
  data = dados_floresta,
  start = list(BETA = 9.2, A = 0.19, THETA = 2.9),
  algorithm = "port"
)

# Cálculo do coeficiente de determinação (R²) para o modelo ajustado
predicoes <- predict(Schnute, newdata = dados_floresta)
residuos <- dados_floresta$AGB - predicoes
soma_quadrados_residuos <- sum(residuos^2, na.rm = TRUE)
soma_quadrados_total <- sum((dados_floresta$AGB - mean(dados_floresta$AGB, na.rm = TRUE))^2, na.rm = TRUE)
r2_original <- 1 - (soma_quadrados_residuos / soma_quadrados_total)

cat("Indicadores do Modelo Schnute (Com Outliers):\n")
cat("Coeficiente de Determinação (R²): ", r2_original, "\n")

# Cálculo do coeficiente de determinação ajustado (R² Ajustado)
n <- nrow(dados_floresta)
p <- length(coef(Schnute))
r2_ajustado_original <- 1 - (1 - r2_original) * (n - 1) / (n - p - 1)
cat("Coeficiente de Determinação Ajustado (R² Ajustado): ", r2_ajustado_original, "\n")

# Cálculo do Critério de Informação de Akaike (AIC) para o modelo
aic_original <- AIC(Schnute)
cat("Critério de Informação de Akaike (AIC): ", aic_original, "\n\n")

# Cálculo do RMSE (Root Mean Square Error)
rmse <- sqrt(mean(residuals(Schnute)^2))
cat("Root Mean Square Error (RMSE): ", rmse, "\n")

# Gráficos diagnósticos do modelo Schnute
par(mfrow = c(2, 2))
plot(Schnute)
par(mfrow = c(1, 1))

# Identificação de outliers baseada nos resíduos padronizados
residuos_padronizados <- residuos / sd(residuos, na.rm = TRUE)
outliers_identificados <- which(abs(residuos_padronizados) > 2)

cat("Índices dos Outliers Identificados:\n")
print(outliers_identificados)

cat("Dados Correspondentes aos Outliers Identificados:\n")
print(dados_floresta[outliers_identificados, ])

# Gráfico dos resíduos padronizados para visualização de outliers
par(mfrow = c(1, 1))
plot(
  residuos_padronizados,
  main = "Resíduos Padronizados",
  xlab = "Índice",
  ylab = "Resíduo Padronizado",
  pch = 1,
  col = "blue"
)
abline(h = c(-2, 2), col = "red", lty = 2)  # Limites de outliers (-2 e 2)


# Remoção dos outliers e ajuste do modelo Schnute
dados_sem_outliers <- dados_floresta[-outliers_identificados, ]

Schnute_ajustado <- nls(
  AGB ~ (BETA * (1 - exp(-A * Age))) ^ THETA,
  data = dados_sem_outliers,
  start = list(BETA = 9.2, A = 0.19, THETA = 2.9),
  algorithm = "port"
)

# Recalcular os indicadores do modelo ajustado (Sem Outliers)
predicoes_sem_outliers <- predict(Schnute_ajustado, newdata = dados_sem_outliers)
residuos_sem_outliers <- dados_sem_outliers$AGB - predicoes_sem_outliers
soma_quadrados_residuos_sem_outliers <- sum(residuos_sem_outliers^2, na.rm = TRUE)
soma_quadrados_total_sem_outliers <- sum((dados_sem_outliers$AGB - mean(dados_sem_outliers$AGB, na.rm = TRUE))^2, na.rm = TRUE)
r2_sem_outliers <- 1 - (soma_quadrados_residuos_sem_outliers / soma_quadrados_total_sem_outliers)

# Cálculo do R² Ajustado para o modelo sem outliers
n_sem_outliers <- nrow(dados_sem_outliers)
p_sem_outliers <- length(coef(Schnute_ajustado))
r2_ajustado_sem_outliers <- 1 - (1 - r2_sem_outliers) * (n_sem_outliers - 1) / (n_sem_outliers - p_sem_outliers - 1)

# Cálculo do AIC para o modelo sem outliers
aic_sem_outliers <- AIC(Schnute_ajustado)

cat("Indicadores do Modelo Schnute (Sem Outliers):\n")
cat("Coeficiente de Determinação (R²): ", r2_sem_outliers, "\n")
cat("Coeficiente de Determinação Ajustado (R² Ajustado): ", r2_ajustado_sem_outliers, "\n")
cat("Critério de Informação de Akaike (AIC): ", aic_sem_outliers, "\n\n")

# Cálculo do RMSE (Root Mean Square Error)
rmse <- sqrt(mean(residuals(Schnute_ajustado)^2))
cat("Root Mean Square Error (RMSE): ", rmse, "\n")

# Adicionando colunas de predições para os conjuntos de dados
dados_floresta$Predicao_Com_Outliers <- predict(Schnute, newdata = dados_floresta)
dados_sem_outliers$Predicao_Sem_Outliers <- predict(Schnute_ajustado, newdata = dados_sem_outliers)

# Visualização gráfica
grafico_com_outliers <- ggplot(data = dados_floresta, aes(x = Age, y = AGB)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(y = Predicao_Com_Outliers), color = "black") +
  labs(
    title = "Modelo Schnute com Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)"
  ) +
  theme_minimal()

grafico_sem_outliers <- ggplot(data = dados_sem_outliers, aes(x = Age, y = AGB)) +
  geom_point(color = "green", size = 2) +
  geom_line(aes(y = Predicao_Sem_Outliers), color = "black") +
  labs(
    title = "Modelo Schnute sem Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)"
  ) +
  theme_minimal()

grafico_comparativo <- ggplot() +
  geom_point(data = dados_floresta, aes(x = Age, y = AGB, color = "Com Outliers"), size = 2) +
  geom_line(data = dados_floresta, aes(x = Age, y = Predicao_Com_Outliers, color = "Com Outliers")) +
  geom_point(data = dados_sem_outliers, aes(x = Age, y = AGB, color = "Sem Outliers"), size = 2) +
  geom_line(data = dados_sem_outliers, aes(x = Age, y = Predicao_Sem_Outliers, color = "Sem Outliers"), linetype = "dashed") +
  labs(
    title = "Modelo Schnute: Comparação Com e Sem Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)",
    color = "Conjunto de Dados"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Exibição dos gráficos
print(grafico_com_outliers)
print(grafico_sem_outliers)
print(grafico_comparativo)

# Salvamento dos dados e modelos
write.csv(dados_sem_outliers, file.path(derived_dir, "dados_sem_outliers_schnute.csv"))


############################
# REGRESSÃO DE GOMPERTZ
############################

# Ajuste inicial do modelo Gompertz
# Modelo não-linear para a relação entre AGB e Age
Gompertz <- nls(
  AGB ~ BETA * exp(-exp(-A * (Age - THETA))),
  data = dados_floresta,
  start = list(BETA = 9.2, A = 0.19, THETA = 2.9),
  algorithm = "port"
)

# Cálculo do coeficiente de determinação (R²) para o modelo inicial
predicoes <- predict(Gompertz, newdata = dados_floresta)
residuos <- dados_floresta$AGB - predicoes
soma_quadrados_residuos <- sum(residuos^2, na.rm = TRUE)
soma_quadrados_total <- sum((dados_floresta$AGB - mean(dados_floresta$AGB, na.rm = TRUE))^2, na.rm = TRUE)
r2_original <- 1 - (soma_quadrados_residuos / soma_quadrados_total)

cat("Indicadores do Modelo Gompertz (Com Outliers):\n")
cat("Coeficiente de Determinação (R²): ", r2_original, "\n")

# Cálculo do coeficiente de determinação ajustado (R² Ajustado)
n <- nrow(dados_floresta)
p <- length(coef(Gompertz))
r2_ajustado_original <- 1 - (1 - r2_original) * (n - 1) / (n - p - 1)
cat("Coeficiente de Determinação Ajustado (R² Ajustado): ", r2_ajustado_original, "\n")

# Cálculo do Critério de Informação de Akaike (AIC)
aic_original <- AIC(Gompertz)
cat("Critério de Informação de Akaike (AIC): ", aic_original, "\n\n")

# Cálculo do RMSE (Root Mean Square Error)
rmse <- sqrt(mean(residuals(Gompertz)^2))
cat("Root Mean Square Error (RMSE): ", rmse, "\n")

# Gráficos diagnósticos do modelo inicial
par(mfrow = c(2, 2))
plot(Gompertz)
par(mfrow = c(1, 1))

# Identificação de outliers com base nos resíduos padronizados
residuos_padronizados <- residuos / sd(residuos, na.rm = TRUE)
outliers_identificados <- which(abs(residuos_padronizados) > 2)

cat("Índices dos Outliers Identificados:\n")
print(outliers_identificados)

cat("Dados Correspondentes aos Outliers:\n")
print(dados_floresta[outliers_identificados, ])

# Gráfico de resíduos padronizados para visualização
plot(
  residuos_padronizados,
  main = "Resíduos Padronizados",
  xlab = "Índice",
  ylab = "Resíduo Padronizado",
  pch = 1,
  col = "blue"
)
abline(h = c(-2, 2), col = "red", lty = 2)  # Limites de identificação de outliers

# Remoção de outliers do conjunto de dados
dados_sem_outliers <- dados_floresta[-outliers_identificados, ]

# Ajuste do modelo Gompertz sem os outliers
# Utilizando valores iniciais ajustados
BETA_inicial <- max(dados_sem_outliers$AGB, na.rm = TRUE)
A_inicial <- 0.1  # Experimente outros valores, se necessário
THETA_inicial <- mean(dados_sem_outliers$Age, na.rm = TRUE)

Gompertz_sem_outliers <- nls(
  AGB ~ BETA * exp(-exp(-A * (Age - THETA))),
  data = dados_sem_outliers,
  start = list(BETA = BETA_inicial, A = A_inicial, THETA = THETA_inicial),
  algorithm = "port"
)

# Recalcular os indicadores do modelo sem outliers
predicoes_sem_outliers <- predict(Gompertz_sem_outliers, newdata = dados_sem_outliers)
residuos_sem_outliers <- dados_sem_outliers$AGB - predicoes_sem_outliers
soma_quadrados_residuos_sem_outliers <- sum(residuos_sem_outliers^2, na.rm = TRUE)
soma_quadrados_total_sem_outliers <- sum((dados_sem_outliers$AGB - mean(dados_sem_outliers$AGB, na.rm = TRUE))^2, na.rm = TRUE)
r2_sem_outliers <- 1 - (soma_quadrados_residuos_sem_outliers / soma_quadrados_total_sem_outliers)

# Cálculo do R² Ajustado para o modelo sem outliers
n_sem_outliers <- nrow(dados_sem_outliers)
p_sem_outliers <- length(coef(Gompertz_sem_outliers))
r2_ajustado_sem_outliers <- 1 - (1 - r2_sem_outliers) * (n_sem_outliers - 1) / (n_sem_outliers - p_sem_outliers - 1)

# Cálculo do AIC para o modelo sem outliers
aic_sem_outliers <- AIC(Gompertz_sem_outliers)

cat("Indicadores do Modelo Gompertz (Sem Outliers):\n")
cat("Coeficiente de Determinação (R²): ", r2_sem_outliers, "\n")
cat("Coeficiente de Determinação Ajustado (R² Ajustado): ", r2_ajustado_sem_outliers, "\n")
cat("Critério de Informação de Akaike (AIC): ", aic_sem_outliers, "\n")

# Cálculo do RMSE (Root Mean Square Error)
rmse <- sqrt(mean(residuals(Gompertz_sem_outliers)^2))
cat("Root Mean Square Error (RMSE): ", rmse, "\n")

# Comparação gráfica: com e sem outliers
dados_floresta$Predicao_Com_Outliers <- predict(Gompertz, newdata = dados_floresta)
dados_sem_outliers$Predicao_Sem_Outliers <- predict(Gompertz_sem_outliers, newdata = dados_sem_outliers)

grafico_comparativo <- ggplot() +
  geom_point(data = dados_floresta, aes(x = Age, y = AGB, color = "Com Outliers"), size = 2) +
  geom_line(data = dados_floresta, aes(x = Age, y = Predicao_Com_Outliers, color = "Com Outliers")) +
  geom_point(data = dados_sem_outliers, aes(x = Age, y = AGB, color = "Sem Outliers"), size = 2) +
  geom_line(data = dados_sem_outliers, aes(x = Age, y = Predicao_Sem_Outliers, color = "Sem Outliers"), linetype = "dashed") +
  labs(
    title = "Modelo Gompertz: Comparação Com e Sem Outliers",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)",
    color = "Conjunto de Dados"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Exibição do gráfico comparativo
print(grafico_comparativo)

# Salvamento dos dados ajustados e dos modelos
write.csv(dados_sem_outliers, file.path(derived_dir, "dados_sem_outliers_gompertz.csv"))

# Sequência de valores para prever
age_grid <- seq(min(dados_floresta$Age, na.rm = TRUE), max(dados_floresta$Age, na.rm = TRUE), length.out = 100)
predictions_df <- data.frame(Age = age_grid)

# Previsões com outliers
predictions_df$Linear_Com_Outliers <- predict(modelo_linear_simples, newdata = predictions_df)
predictions_df$Multiple_Com_Outliers <- predict(
  modelo_linear_multiplo,
  newdata = data.frame(
    Age = age_grid,
    Temperature = mean(dados_floresta$Temperature, na.rm = TRUE),
    Precipitation = mean(dados_floresta$Precipitation, na.rm = TRUE)
  )
)
predictions_df$Schnute_Com_Outliers <- predict(Schnute, newdata = predictions_df)
predictions_df$Gompertz_Com_Outliers <- predict(Gompertz, newdata = predictions_df)

# Previsões sem outliers
predictions_df$Linear_Sem_Outliers <- predict(modelo_linear_simples_ajustado, newdata = predictions_df)
predictions_df$Multiple_Sem_Outliers <- predict(
  modelo_linear_multiplo_ajustado,
  newdata = data.frame(
    Age = age_grid,
    Temperature = mean(dados_sem_outliers$Temperature, na.rm = TRUE),
    Precipitation = mean(dados_sem_outliers$Precipitation, na.rm = TRUE)
  )
)
predictions_df$Schnute_Sem_Outliers <- predict(Schnute_ajustado, newdata = predictions_df)
predictions_df$Gompertz_Sem_Outliers <- predict(Gompertz_sem_outliers, newdata = predictions_df)

# Transformar para formato longo
predictions_long <- predictions_df %>%
  pivot_longer(
    cols = -Age,
    names_to = c("Modelo", "Status"), # Dividindo nomes de colunas em duas categorias
    names_sep = "_", # Separador entre Modelo e Status
    values_to = "AGB_Previsto"
  )

# Criar gráfico combinado
plot_combinado <- ggplot(dados_floresta, aes(x = Age, y = AGB)) +
  geom_point(alpha = 0.6, color = "grey40") + # Dados observados
  geom_line(
    data = predictions_long,
    aes(x = Age, y = AGB_Previsto, color = Modelo, linetype = Status),
    size = 1
  ) + # Curvas de previsão
  labs(
    title = "Comparação de Modelos Calibrados: Ajustado e Não Ajustado",
    x = "Idade do Plantio (Anos)",
    y = "Biomassa Acima do Solo (t MS/ha)",
    color = "Modelo",
    linetype = "Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Exibir o gráfico
print(plot_combinado)

# Definir o diretório de trabalho
setwd("C:/Users/Casa/Desktop/Davi")

# Carregar os arquivos necessários
agb_parcela_results <- read.csv("agb_parcela_results.csv")
database_consolidado_corrigido <- read_excel("Database_Consolidado_Corrigido.xlsx")

# Identificar todas as colunas que devem estar presentes nas tabelas
colunas_totais <- union(names(database_consolidado_corrigido), names(agb_parcela_results))

# Garantir que as tabelas tenham as mesmas colunas
# Para isso, adicionamos colunas ausentes e preenchemos com NA
for (col in setdiff(colunas_totais, names(database_consolidado_corrigido))) {
  database_consolidado_corrigido[[col]] <- NA
}

for (col in setdiff(colunas_totais, names(agb_parcela_results))) {
  agb_parcela_results[[col]] <- NA
}

# Reorganizar as colunas de ambas as tabelas na mesma ordem
database_padronizado <- database_consolidado_corrigido[colunas_totais]
agb_padronizado <- agb_parcela_results[colunas_totais]

# Unir as tabelas (adicionando as linhas de AGB à base consolidada)
tabela_unificada <- bind_rows(database_padronizado, agb_padronizado)
head(tabela_unificada)
write_xlsx(tabela_unificada, "tabela_unificada.xlsx")
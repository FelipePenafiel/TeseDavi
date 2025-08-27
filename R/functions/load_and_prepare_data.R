load_and_prepare_data <- function(file_path = file.path("data", "raw", "tabela_unificada.xlsx"),
                                  numeric_columns = c("AGB", "Age", "Temperature", "Precipitation")) {
  data <- readxl::read_excel(file_path, col_types = "text")
  data[numeric_columns] <- lapply(data[numeric_columns], function(x) as.numeric(gsub(",", ".", x)))
  data[data == -9999] <- NA
  return(data)
}

load_libraries <- function() {
  packages <- c(
    "readxl",
    "dplyr",
    "writexl",
    "MASS",
    "car",
    "ggplot2",
    "tidyr",
    "openxlsx",
    "lmtest"
  )
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      library(pkg, character.only = TRUE)
    }
  }
}

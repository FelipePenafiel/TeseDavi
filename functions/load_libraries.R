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
      install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }
}

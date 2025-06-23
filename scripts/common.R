library(tidyverse)
library(gt)

data <- read_rds("data/bod.rds")
fits <- read_rds("data/fits.rds")
t_tests <- read_rds("data/t-tests.rds")

labels_test <- c(
  paper    = "(+C)",
  plastic  = "Ecovio",
  control  = "(-C)"
)

no_conv <- fits |> unnest(metrics) |> filter(!isConv) |> pull(reactor)

na_min <- function(x) {
  if (all(is.na(x)))
    return(NA)
  
  min(x, na.rm = T)
}

na_max <- function(x) {
  if (all(is.na(x)))
    return(NA)
  
  max(x, na.rm = T)
}

str_range <- function(x) {
  min_val <- na_min(x)
  max_val <- na_max(x)
  
  sprintf("[%.2f, %.2f]\n", min_val, max_val)
}

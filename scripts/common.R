library(tidyverse)
library(gt)

data <- read_rds("data/bod.rds")
fits <- read_rds("data/fits.rds")
t_tests <- read_rds("data/t-tests.rds")

labels_medium <- c(
  lake     = "Lake",
  sea      = "Marine",
  blank    = "Empty reactors"
)

labels_test <- c(
  paper    = "Cellulose (+C)",
  plastic  = "Ecovio®",
  control  = "Control (-C)",
  blank    = "Blank"
)

no_conv <- fits |> unnest(metrics) |> filter(!isConv) |> pull(reactor)

str_range <- function(x) {
  if (all(is.na(x)))
    return(NA)
  
  min_val <- min(x, na.rm = T)
  max_val <- max(x, na.rm = T)
  
  sprintf("[%.2f, %.2f]\n", min_val, max_val)
}
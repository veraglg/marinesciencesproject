library(tidyverse)
library(broom)
library(nls.multstart)
set.seed(124)

data <- read_rds("data/bod.rds")

fits <- data |> 
  filter(medium != "blank") |> 
  group_by(reactor, medium, test) |> 
  nest() |> 
  ungroup() |> 
  mutate(
    linear = map(data, ~ lm(bod ~ time, data = .x)),
    single_exp = map(data, ~ nls_multstart(
      bod ~ l * (1 - exp(-k * time)),
      data = .x,
      iter = 250,
      start_lower = c(l = 0, k = .1),
      start_upper = c(l = 1000, k = 10),
      lower = c(l = 0, k = 0),
      supp_errors = "Y",
      
    )),
    double_exp = map(data, ~ nls_multstart(
      bod ~ l1 * (1 - exp(-k1 * time)) + l2 * (1 - exp(-k2 * time)),
      data = .x,
      iter = 250,
      start_lower = c(l1 = 0, l2 = 0, k1 = 1e-5, k2 = 1e-10),
      start_upper = c(l1 = 500, l2 = 1000, k1 = 10, k2 = 10),
      lower = c(l1 = 0, k1 = 0, l2 = 0, k2 = 0),
      supp_errors = "Y"
    ))
  ) 

fits <- fits |> 
  pivot_longer(
    c(linear, single_exp, double_exp),
    names_to = "model", values_to = "fit"
  ) |> 
  mutate(
    resids = map(fit, residuals),
    metrics = map(fit, glance),
    params = map(fit, tidy),
    preds = map(fit, augment)
  )

fits |> write_rds("data/fits.rds")
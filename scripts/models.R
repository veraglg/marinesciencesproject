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
    SEM = map(data, ~ nls_multstart(
      bod ~ l * (1 - exp(-k * time)),
      data = .x,
      iter = 250,
      start_lower = c(l = 0, k = .1),
      start_upper = c(l = 1000, k = 10),
      lower = c(l = 0, k = 0),
      supp_errors = "Y",
      
    )),
    DEM = map(data, ~ nls_multstart(
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
    c(SEM, DEM),
    names_to = "model", values_to = "fit"
  ) |> 
  mutate(
    resids = map(fit, residuals),
    metrics = map(fit, glance),
    params = map(fit, tidy),
    preds = map(fit, augment)
  )

fig_fits <- fits |> unnest(data) |> 
  ggplot(aes(x = time, y = bod)) +
  geom_point(alpha = .1) +
  geom_line(
    data = fits |> unnest(preds),
    aes(y = .fitted, color = model, linetype = model),
    linewidth = 1
  ) +
  facet_wrap(~ reactor, scales = "free_y") +
  labs(x = "Time (days)", y = "BOD (mg/L)") +
  theme(legend.position = "inside", legend.position.inside = c(.9, .05),
        legend.title = element_blank())

ggsave("supplementary/fig-fits.png", fig_fits)

fits |> write_rds("data/fits.rds")
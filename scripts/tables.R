library(tidyverse)
library(gt)

data <- read_rds("data/bod.rds")
fits <- read_rds("data/fits.rds")

no_conv <- fits |> unnest(metrics) |> filter(!isConv) |> pull(reactor)

tbl_bod30 <- data |> 
  filter(time == 30) |> 
  group_by(medium, test) |> 
  summarize(mean = mean(bod), sd = sd(bod), .groups = "drop") |> 
  pivot_wider(names_from = "medium", values_from = c(mean, sd),
              names_glue = "{medium}_{.value}") |> 
  mutate(across(where(is.numeric), \(x) round(x, 2))) |> 
  mutate(test = fct_recode(test,
                           "Paper" = "paper",
                           "Plastic" = "plastic",
                           "Control" = "control")) |> 
  gt(rowname_col = "test") |> 
  tab_spanner("Lake", starts_with("lake")) |> 
  tab_spanner("Sea", starts_with("sea")) |> 
  tab_spanner(md("BOD~30~ (mg O~2~/L)"), everything()) |> 
  cols_label(
    lake_mean = "mean", sea_mean = "mean",
    lake_sd = "std. dev.", sea_sd = "std. dev."
  )

tbl_bod30 |> gtsave("tbl-bod30.png")


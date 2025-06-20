library(readr)
library(broom)

data <- read_rds("data/bod.rds")

lake_plastic <- t.test(bod ~ test,
       var.equal = TRUE,
       data |> filter(time == 30, medium == "lake",
                      test %in% c("plastic", "control"))) |>
  tidy() |> mutate(medium = "lake", test = "plastic")

lake_paper <- t.test(bod ~ test,
                       var.equal = TRUE,
                       data |> filter(time == 30, medium == "lake",
                                      test %in% c("paper", "control"))) |>
  tidy() |> mutate(medium = "lake", test = "paper")

marine_plastic <- t.test(bod ~ test, 
       var.equal = TRUE,
       data |> filter(time == 30, medium == "marine",
                      test %in% c("plastic", "control"))) |> 
  tidy() |> mutate(medium = "marine", test = "plastic")

marine_paper <- t.test(bod ~ test, 
                      var.equal = TRUE,
                      data |> filter(time == 30, medium == "marine",
                                     test %in% c("paper", "control"))) |> 
  tidy() |> mutate(medium = "marine", test = "paper")

t_tests <- 
  lake_plastic |> bind_rows(lake_paper) |> 
  bind_rows(marine_plastic) |> bind_rows(marine_paper) |>
  select(medium, test, p.value)

t_tests |> write_rds("data/t-tests.rds")

# lm(bod ~ 1, data |> filter(time == 30, medium == "marine", test == "plastic")) |> 
#   confint()

library(tidyverse)

data <- read_csv("data/pressures.csv")

levels_medium <- c("lake", "sea", "blank")
levels_test <- c("paper", "plastic", "control", "blank")

data <- data |> 
  mutate(
    medium = medium |> fct(levels = levels_medium),
    test = test |> fct(levels = levels_test),
    time = time / (60 * 24)
  )

blanks <- data |> 
  filter(medium == "blank", reactor != "B27") |> 
  summarize(
    blk_pressure = mean(pressure),
    .by = time
  )

data <- data |> 
  left_join(blanks, by = join_by(time)) |> 
  mutate(adj_pressure = pressure - blk_pressure)

tm <- 25 + 273.15 # sample temperature in Kelvin
t0 <- 273.15 # reference temperature in Kelvin
a <- 0.03103 # Bunsen absorption coefficient
r <- 83.144 # gas constant in L.hpa/mol.K
mo2 <- 32000 # oxygen gas molecular weight in mg/mol
vt <- 995 # reactor volume in mL
sv <- 260 # sample volume in mL.

data <- data |> 
  mutate(bod = mo2 / (r * tm) * ((vt - sv) / sv + a * tm / t0) * -adj_pressure)

data |> write_rds("data/bod.rds")
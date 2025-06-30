library(tidyverse)

data <- read_csv("data/pressures.csv")

levels_medium <- c("lake", "marine", "blank")
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

fig_pressures <- data |> 
  ggplot(aes(x = time, y = pressure, group = reactor)) +
  geom_hline(yintercept = 0, linewidth = 2, color = "white") +
  geom_line(alpha = .5) +
  annotate(
    geom = "segment",
    arrow = arrow(type = "open", length = unit(.1, "inch")),
    x    = 10, y    = -30,
    xend = 14, yend = -11
  ) +
  annotate(
    geom = "segment",
    arrow = arrow(type = "open", length = unit(.1, "inch")),
    x    = 10, y    = -30,
    xend = 15, yend = -15
  ) +
  annotate(
    geom = "segment",
    arrow = arrow(type = "open", length = unit(.1, "inch")),
    x    = 10, y    = -30,
    xend = 16, yend = -22
  ) +
  annotate(
    geom = "label", label = "Reactors\n(n = 30)", size = 3,
    x = 10, y = -30
  ) +
  labs(x = "Time (days)", y = "Pressure difference (hPa)")

ggsave("supplementary/fig-pressures.png", fig_pressures)

fig_blanks <- data |> 
  filter(test == "blank") |> 
  ggplot(aes(x = time, y = pressure, group = reactor)) +
  geom_hline(yintercept = 0, linewidth = 2, color = "white") +
  geom_line(linewidth = 1, alpha = .5) +
  annotate(
    geom = "segment", arrow = arrow(type = "open", length = unit(.1, "inch")),
    x    = 10, y    = 6,
    xend =  3, yend = 4
  ) +
  annotate(
    geom = "label", label = "Failed reactor", size = 3,
    x = 10, y = 6
  ) +
  labs(x = "Time (days)", y = "Pressure difference (hPa)")

ggsave("supplementary/fig-blanks.png", fig_blanks)

data <- data |> 
  left_join(blanks, by = join_by(time)) |> 
  mutate(adj_pressure = pressure - blk_pressure) |> 
  filter(test != "blank")

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

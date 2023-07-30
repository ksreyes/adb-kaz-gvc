# TRADE ANATOMY

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(arrow)
library(readxl)
library(tidyverse)
library(cowplot)

filename <- "3.2_anatomy"
years <- c(2021, 2022)

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(!(is.na(mrio)))

select <- countries |> filter(name == "Kazakhstan") |> pull(mrio)


# Load and process data ---------------------------------------------------

df <- here("..", "..", "MRIO Processing", "data", "ta.parquet") |> 
  read_parquet() |> 
  filter(breakdown == "es" & s == select & t %in% years) |> 
  summarize(across(exports:pdc2, sum), .by = c(t, s, r)) |> 
  left_join(countries, by = c("r" = "mrio")) |> 
  mutate(
    t = factor(t),
    davax = davax1 + davax2,
    rex = rex1 + rex2 + rex3 + ref1 + ref2,
  )

df_fva <- here("..", "..", "MRIO Processing", "data", "adhoc", "ta-fva.parquet") |> 
  read_parquet() |> 
  filter(s == select & t %in% years) |> 
  left_join(countries, by = c("u" = "mrio")) |> 
  mutate(t = factor(t))

get_top_partners <- function(df, category, year = years[2]) {
  df |> 
    filter(t == year & mrio_code != "ROW") |> 
    arrange(category |> matches() |> pick() |> desc()) |> 
    slice_head(n = 10) |> 
    pull(mrio_code)
}

exports <- df |> 
  mutate(
    share = exports / sum(exports), 
    label = str_c(round(100 * share, 1) |> format(nsmall = 1), "%"),
    .by = t) |> 
  filter(mrio_code %in% get_top_partners(df, "exports")) |>
  select(s, r, mrio_code, t, share, label)

davax <- df |> 
  mutate(
    share = davax / sum(davax), 
    label = str_c(round(100 * share, 1) |> format(nsmall = 1), "%"),
    .by = t) |> 
  filter(mrio_code %in% get_top_partners(df, "davax")) |>
  select(s, r, mrio_code, t, share, label)

rex <- df |> 
  mutate(
    share = rex / sum(rex), 
    label = str_c(round(100 * share, 1) |> format(nsmall = 1), "%"),
    .by = t) |> 
  filter(mrio_code %in% get_top_partners(df, "rex")) |>
  select(s, r, mrio_code, t, share, label)

fva <- df_fva |> 
  mutate(
    share = fva_origin / fva, 
    label = str_c(round(100 * share, 1) |> format(nsmall = 1), "%"),
    .by = t) |> 
  filter(mrio_code %in% get_top_partners(df_fva, "fva")) |>
  select(s, u, mrio_code, t, share, label)

names <- bind_rows(exports, davax, rex, fva) |>
  distinct(mrio_code) |>
  arrange(mrio_code)


# Plot --------------------------------------------------------------------

text <- geom_text(
    aes(label = label, x = share + .01), 
    position = position_dodge(.9), hjust = 0, size = 2, alpha = .5
  )
scale <- scale_x_continuous(limits = c(0, .34))
alpha <- scale_alpha_manual(values = c(.5, 1))
theme <- theme(
    plot.margin = margin(15, 0, 10, 0),
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 7, margin = margin(0, -3, 0, 0)),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank()
  )

plot1 <- ggplot(
    exports, 
    aes(x = share, y = fct_reorder2(mrio_code, t, share, .desc = FALSE), group = t)
  ) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "gray20"
  ) +
  labs(title = "Exports") + alpha + text + scale + theme +
  annotate("text", x = .01, y = 10.2, hjust = 0, label = years[2], size = 1.75, color = "white") +
  annotate("text", x = .01, y = 9.8, hjust = 0, label = years[1], size = 1.75, color = "white")

plot2 <- ggplot(
    davax, 
    aes(x = share, y = fct_reorder2(mrio_code, t, share, .desc = FALSE), group = t)
  ) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "#007db7"
  ) +
  labs(title = "DAVAX") + alpha + text + scale + theme

plot3 <- ggplot(
    rex, 
    aes(x = share, y = fct_reorder2(mrio_code, t, share, .desc = FALSE), group = t)
  ) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "#00A5D2"
  ) +
  labs(title = "REX") + alpha + text + scale + theme

plot4 <- ggplot(
    fva, 
    aes(x = share, y = fct_reorder2(mrio_code, t, share, .desc = FALSE), group = t)
  ) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "#6BB305"
  ) +
  labs(title = "FVA origin") + alpha + text + scale + theme + 
  annotate("text", x = .33, y = 10.225, hjust = 1, label = "33.9%", size = 2, alpha = .75, color = "white")

plot <- plot_grid(
  plot1, plot2, plot3, plot4, 
  ncol = 4, rel_widths = c(1, 1, 1, 1)
)

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  plot,
  device = cairo_pdf,
  width = 16, height = 10, unit = "cm"
)

######### END #########
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

# Data --------------------------------------------------------------------

ta <- here("..", "..", "MRIO Processing", "data", "ta.parquet") |> 
  read_parquet() |> 
  filter(breakdown == "es" & s == select & t %in% years) |> 
  summarize(across(exports:pdc2, sum), .by = c(t, s, r)) |> 
  left_join(countries, by = c("r" = "mrio")) |> 
  mutate(
    t = factor(t),
    davax = davax1 + davax2,
    rex = rex1 + rex2 + rex3 + ref1 + ref2,
  )

ta_fva <- here("data", "interim", "ta-fva.parquet") |> 
  read_parquet() |> 
  filter(s == select & t %in% years) |> 
  left_join(countries, by = c("u" = "mrio")) |> 
  mutate(t = factor(t))

get_top_partners <- function(df, category, year = years[2]) {
  df |> filter(t == year & code != "ROW") |> 
    arrange(category |> matches() |> pick() |> desc()) |> 
    slice_head(n = 10) |> 
    pull(code)
}

ta |> 
  mutate(flow = "Exports", share = exports / sum(exports), .by = t) |> 
  filter(code %in% get_top_partners(ta, "exports")) |>
  pivot_wider(id_cols = c(flow, code, name), names_from = t, values_from = share) |> 
  arrange(-`2022`) |> 
  bind_rows(
    ta |> mutate(flow = "DAVAX", share = davax / sum(davax), .by = t) |> 
      filter(code %in% get_top_partners(ta, "davax")) |>
      pivot_wider(id_cols = c(flow, code, name), names_from = t, values_from = share) |> 
      arrange(-`2022`)
  ) |> 
  bind_rows(
    ta |> mutate(flow = "REX", share = rex / sum(rex), .by = t) |> 
      filter(code %in% get_top_partners(ta, "rex")) |>
      pivot_wider(id_cols = c(flow, code, name), names_from = t, values_from = share) |> 
      arrange(-`2022`)
  ) |> 
  bind_rows(
    ta_fva |> mutate(flow = "FVA origin", share = fva_origin / fva, .by = t) |> 
      filter(code %in% get_top_partners(ta_fva, "fva")) |>
      pivot_wider(id_cols = c(flow, code, name), names_from = t, values_from = share) |> 
      arrange(-`2022`)
  ) |> 
  select(flow, code, name, `2021`, `2022`) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |> 
  pivot_longer(cols = c(`2021`, `2022`), names_to = "t", values_to = "share") |> 
  mutate(label = str_c(round(100 * share, 1) |> format(nsmall = 1), "%"))

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
    df |> filter(flow == "Exports"), 
    aes(x = share, y = fct_reorder2(code, t, share, .desc = FALSE), group = t)
  ) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "gray20"
  ) +
  labs(title = "Exports") + alpha + text + scale + theme +
  annotate("text", x = .01, y = 10.2, hjust = 0, label = years[2], size = 1.75, color = "white") +
  annotate("text", x = .01, y = 9.8, hjust = 0, label = years[1], size = 1.75, color = "white")

plot2 <- ggplot(
    df |> filter(flow == "DAVAX"), 
    aes(x = share, y = fct_reorder2(code, t, share, .desc = FALSE), group = t)
  ) + 
  geom_bar(aes(alpha = t), stat = "identity", position = position_dodge(.85), width = .8, fill = "#007db7") +
  labs(title = "DAVAX") + alpha + text + scale + theme
  
plot3 <- ggplot(
    df |> filter(flow == "REX"), 
    aes(x = share, y = fct_reorder2(code, t, share, .desc = FALSE), group = t)
  ) + 
  geom_bar(aes(alpha = t), stat = "identity", position = position_dodge(.85), width = .8, fill = "#00A5D2") +
  labs(title = "REX") + alpha + text + scale + theme
  
plot4 <- ggplot(
    df |> filter(flow == "FVA origin"), 
    aes(x = share, y = fct_reorder2(code, t, share, .desc = FALSE), group = t)
  ) + 
  geom_bar(aes(alpha = t), stat = "identity", position = position_dodge(.85), width = .8, fill = "#6BB305") +
  labs(title = "FVA origin") + alpha + text + scale + theme + 
  annotate("text", x = .33, y = 10.225, hjust = 1, label = "33.9%", size = 2, alpha = .75, color = "white")

plot <- plot_grid(plot1, plot2, plot3, plot4, nrow = 1)

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 10, unit = "cm"
)

# For notes on chart
unique_names <- df |> distinct(code, name) |> arrange(code)

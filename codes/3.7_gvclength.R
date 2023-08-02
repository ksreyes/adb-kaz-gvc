# GVC LENGTHS BY DOMESTIC AND FOREIGN STAGES

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

filename <- "3.7_gvclength"

years <- c(2000, 2010, 2020, 2022)

highlight <- c("AHF", "MIN", "MFM", "WST")

sectors <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel() |> 
  distinct(ind, abv, name_short) |>
  rename(i = ind, sector = name_short) |> 
  filter(abv %in% highlight) |> 
  add_row(i = 0, sector = "Aggregate", .before = 1) |> 
  mutate(
    sector = replace(sector, sector == "Wholesale trade", "Wholesale\ntrade"),
    face = case_when(sector == "Aggregate" ~ "bold", .default = "plain")
  )

select <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(name == "Kazakhstan") |> 
  pull(mrio)

# Data --------------------------------------------------------------------

here("..", "..", "MRIO Processing", "data", "lengths62.parquet") |> 
  read_parquet() |> 
  filter(s == select & agg %in% c(0, 35) & t %in% years[1:2]) |> 
  bind_rows(
    here("..", "..", "MRIO Processing", "data", "lengths.parquet") |> 
      read_parquet() |> 
      filter(s == select & agg %in% c(0, 35) & t %in% years[3:4])
  ) |> 
  filter(i %in% sectors$i) |> 
  left_join(sectors) |> 
  mutate(
    domestic = PLvd_GVC,
    foreign = CBv_GVC + PLvf_GVC,
    total = domestic + foreign
  ) |> 
  arrange(i, sector) |> 
  select(sector, t, domestic, foreign, total) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |> 
  mutate(
    sector = factor(sector, levels = rev(sectors$sector)),
    t = factor(t, levels = rev(years))
  )

sectors <- sectors |> 
  mutate(face = case_when(sector == "Aggregate" ~ "bold", .default = "plain"))

line <- geom_vline(xintercept = 0, linewidth = .35, color = "gray25")
alpha <- scale_alpha_manual(values = c(1, .75, .5, .25))
theme <- theme(
    plot.margin = margin(15, 10, 0, 0),
    plot.background = element_rect(fill = NA, color = NA),
    axis.title.x.top = element_text(size = 9, face = "bold", margin = margin(b = 8)),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray75", linewidth = .25, linetype = "dashed")
  )

yaxis <- ggplot(df, aes(x = 0, y = sector)) + 
  scale_x_continuous(name = "", position = "top", limits = c(0, 0), breaks = 0, labels = "") + 
  theme + theme(
    plot.margin = margin(15, 5, 0, 5),
    axis.text.y = element_text(
      size = 9, 
      face = rev(sectors$face), 
      color = "black", 
      margin = margin(r = -8))
  )

plot1 <- ggplot(df, aes(x = domestic - 1, y = sector, group = t, alpha = t)) + 
  geom_bar(
    stat = "identity", position = position_dodge(width = .85), 
    width = .7, fill = "#007db7"
  ) +
  scale_x_continuous(
    name = "Domestic stages", position = "top", 
    limits = c(0, 2.7 - 1), 
    breaks = seq(0, 2.5 - 1, .5),
    labels = seq(1, 2.5, .5)
  ) + 
  line + alpha + theme

plot2 <- ggplot(df, aes(x = foreign - 1, y = sector, group = t, alpha = t)) + 
  geom_bar(
    stat = "identity", position = position_dodge(width = .85), 
    width = .7, fill = "#6BB305"
  ) +
  scale_x_continuous(
    name = "Foreign stages", position = "top", 
    limits = c(0, 2.95 - 1),
    breaks = seq(0, 2.5 - 1, .5),
    labels = seq(1, 2.5, .5)
  ) + 
  line + alpha + theme

legend <- get_legend(
  plot1 + 
  guides(alpha = guide_legend(
    reverse = TRUE, 
    override.aes = list(fill = "black")
  )) + 
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 5, 0, 0)),
    legend.position = "bottom"
  ))

plot <- plot_grid(yaxis, plot1, plot2, nrow = 1, rel_widths = c(.4, 2.6 - 1, 2.8 - 1))
plot <- plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .15))

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 10, unit = "cm"
)

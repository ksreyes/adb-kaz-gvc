# REVEALED COMPARATIVE ADVANTAGE INDICES: GROSS VS VALUE ADDED

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

filename <- "4.1_rca"

year <- 2022

select <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(name == "Kazakhstan") |> 
  pull(mrio)

# Data --------------------------------------------------------------------

sectors <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel() |> 
  distinct(ind, name_short) |>
  rename(i = ind, sector = name_short)

here("..", "..", "MRIO Processing", "data", "rca.parquet") |> 
  read_parquet() |> 
  filter(agg == 35 & s == select) |> 
  summarize(across(exports_si:vaxos_all, mean), .by = i) |> 
  mutate(
    rca = (exports_si / exports_s) / (exports_i / exports_all),
    rca_vaxos = (vaxos_si / vaxos_s) / (vaxos_i / vaxos_all),
    exports_sh = exports_si / exports_s,
    exports_wsh = exports_i / exports_all,
    vaxos_sh = vaxos_si / vaxos_s,
    vaxos_wsh = vaxos_i / vaxos_all,
  ) |> 
  left_join(sectors) |> 
  select(sector, exports_si, exports_sh, exports_wsh, rca, vaxos_si, vaxos_sh, vaxos_wsh, rca_vaxos) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |> 
  mutate(
    rca_plot = ifelse(exports_sh > exports_wsh, rca - 1, (-exports_wsh / exports_sh) + 1),
    rca_vaxos_plot = ifelse(vaxos_sh > vaxos_wsh, rca_vaxos - 1, (-vaxos_wsh / vaxos_sh) + 1)
  ) |> 
  filter(exports_si > 0)

# Middle plot: Ranking changes

plot_rank <- df |> 
  arrange(rca_vaxos_plot) |> 
  mutate(vaxos = 1:nrow(df)) |> 
  arrange(rca_plot) |> 
  mutate(exports = 1:nrow(df)) |> 
  pivot_longer(cols = exports:vaxos, names_to = "type", values_to = "rank") |> 
  ggplot(aes(x = type, y = rank, group = sector)) + 
  geom_rect(
    xmin = "exports", xmax = "vaxos", ymin = 1 - .1, ymax = 34 + .1,
    fill = "gray90", color = "gray90", size = 2
  ) + 
  geom_line(linewidth = .35) + 
  scale_y_continuous(limits = c(0, 34.1)) + 
  theme(
    plot.margin = margin(2, -30, 13, -30),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )

# Right plot: RCA by gross exports

plot_exports <- df |> arrange(rca_plot) |>
  mutate(
    rank = 1:nrow(df),
    arrow = ifelse(abs(rca_plot) > 17, -2, NA),
    label = case_when(
      is.na(arrow) ~ NA,
      abs(round(rca_plot)) + 1 > 100 ~ "< 1/100",
      .default = str_c("1/", round(-rca_plot) + 1)
    ),
    rca_plot = ifelse(is.na(arrow), rca_plot, NA)
  ) |> 
  ggplot(aes(x = rca_plot, y = rank)) + 
  geom_segment(
    aes(x = 0, xend = rca_plot, y = rank, yend = rank, color = ifelse(rca_plot > 0, "#007db7", "#63CCEC")),
    linewidth = .75
  ) + 
  geom_point(
    aes(color = ifelse(rca_plot > 0, "#007db7", "#63CCEC")),
    shape = 16, size = 2.5,
  ) +
  scale_x_continuous(breaks = c(-9, 0, 10), labels = c("1/10", "1", "10")) + 
  scale_y_continuous(sec.axis = sec_axis(~., breaks = 1:nrow(df), labels = df |> arrange(rca_plot) |> pull(sector))) + 
  scale_color_identity() +
  theme(
    plot.margin = margin(2, 2, 13, 2),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8, hjust = .5),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(size = 7, hjust = 1, margin = margin(l = -5)),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(color = "gray75", linewidth = .25, linetype = "dashed"),
    panel.grid.major.y = element_blank()
  ) + 
  
  # Off-the-chart sectors
  geom_segment(
    aes(x = arrow, xend = arrow, y = rank, yend = rank, color = ifelse(arrow > 0, "#007db7", "#63CCEC")),
    lineend = "butt", linejoin = "mitre", 
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = arrow, y = rank, label = label),
    hjust = 1, nudge_x = -1, size = 1.75, color = "gray25"
  )

# Left plot: RCA by value-added exports

plot_vaxos <- df |> arrange(rca_vaxos_plot) |> 
  mutate(
    rank = 1:nrow(df),
    arrow = ifelse(abs(rca_vaxos_plot) > 17, -1.5, NA),
    label = case_when(
      is.na(arrow) ~ NA,
      abs(round(rca_vaxos_plot)) + 1 > 100 ~ "< 1/100",
      .default = str_c("1/", round(-rca_vaxos_plot) + 1)
    ),
    rca_vaxos_plot = ifelse(is.na(arrow), rca_vaxos_plot, NA)
  ) |> 
  ggplot(aes(x = rca_vaxos_plot, y = rank)) + 
  geom_segment(
    aes(x = 0, xend = rca_vaxos_plot, y = rank, yend = rank, color = ifelse(rca_vaxos_plot > 0, "#007db7", "#63CCEC")),
    linewidth = .75
  ) + 
  geom_point(
    aes(color = ifelse(rca_vaxos_plot > 0, "#007db7", "#63CCEC")),
    size = 2.5, shape = 16,
  ) +
  scale_x_continuous(breaks = c(-9, 0, 9), labels = c("1/10", "1", "10")) + 
  scale_y_continuous(breaks = 1:nrow(df), labels = df |> arrange(rca_vaxos_plot) |> pull(sector)) + 
  scale_color_identity() +
  theme(
    plot.margin = margin(2, 2, 13, 4),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8, hjust = .5),
    axis.text.y = element_text(size = 7, hjust = 0, margin = margin(r = -5)),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(color = "gray75", linewidth = .25, linetype = "dashed"),
    panel.grid.major.y = element_blank()
  ) +
  
  # Off-the-chart sectors
  geom_segment(
    aes(x = arrow, xend = arrow, y = rank, yend = rank, color = ifelse(arrow > 0, "#007db7", "#63CCEC")),
    lineend = "butt", linejoin = "mitre", 
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = arrow, y = rank, label = label),
    hjust = 1, nudge_x = -.8, size = 1.75, color = "gray25"
  )

# Consolidate and export

title_r <- ggdraw() + 
  draw_label("RCA by Gross Exports", hjust = .5, size = 9, fontface = "bold") + 
  theme(plot.margin = margin(10, 2, 2, 2))
title_m <- ggdraw() + draw_label("M", hjust = .5, size = 9, color = NA) + 
  theme(plot.margin = margin(10, 2, 2, 2))
title_l <- ggdraw() + 
  draw_label("RCA by Value Added Exports", hjust = .5, size = 9, fontface = "bold") + 
  theme(plot.margin = margin(10, 2, 2, 2))

titles <- plot_grid(title_r, title_m, title_l, ncol = 3, rel_widths = c(1, .3, 1))
plots <- plot_grid(plot_exports, plot_rank, plot_vaxos, ncol = 3, rel_widths = c(1, .3, 1))
plot <- plot_grid(titles, plots, ncol = 1, rel_heights = c(.1, 1))

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 13, unit = "cm"
)

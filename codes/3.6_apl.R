# APL

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(ggrepel)
library(cowplot)

filename <- "3.6_apl"

years <- c(2000, 2010, 2022)

highlight <- c("AHF", "MIN", "MFM", "WST")

select <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(name == "Kazakhstan") |> 
  pull(mrio)

# Data --------------------------------------------------------------------

sectors <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel() |>
  distinct(ind, abv, name, ind5, name5) |>
  add_row(ind = 0, abv = "", name = "Aggregate", name5 = "Aggregate")

here("..", "..", "MRIO Processing", "data", "lengths62.parquet") |> 
  read_parquet() |> 
  filter(s == select & agg %in% c(0, 35) & t %in% years[1:2]) |> 
  bind_rows(
    here("..", "..", "MRIO Processing", "data", "lengths.parquet") |> 
      read_parquet() |> 
      filter(s == select & agg %in% c(0, 35) & t == years[3])
  ) |> 
  mutate(length = PLv_GVC + PLy_GVC) |> 
  select(t, i, length) |> 
  pivot_wider(names_from = t, values_from = length) |> 
  left_join(sectors, by = c("i" = "ind")) |> 
  select(abv, sector = name, sector5 = name5, starts_with("20")) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |> 
  mutate(sector5 = factor(sector5, levels = unique(sectors$name5)))

labels <- df |> filter(abv %in% highlight) |> select(abv, `2000`:`2022`)

colors <- scale_color_manual(values = c("#E9532b", "#6DCFF6", "#007db7", "#8dc63f", "#FDB415", "gray20"))

plot1 <- ggplot(df) + 
  geom_abline(size = .25) + 
  geom_point(
    aes(x = `2000`, y = `2010`, color = sector5), 
    size = ifelse(df$sector != "Aggregate", 4, 6),
    alpha = ifelse(df$sector != "Aggregate", .8, 1),
    shape = ifelse(df$sector != "Aggregate", 16, 18)
  ) +
  scale_x_continuous(name = years[1], limits = c(6, 11)) +  
  scale_y_continuous(name = years[2], limits = c(6, 11)) + 
  colors + 
  theme(
    plot.margin = margin(15, 8, 2, 2),
    axis.title = element_text(size = 9),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  # Add labels
  annotate(
    "text", label = "Lengthened", 
    x = 6.5, y = 10.5, angle = 45, hjust = .5, vjust = 0, size = 3, color = "gray50"
  ) + 
  geom_point(aes(x = `2000`, y = `2010`), labels, size = 4, shape = 1) + 
  geom_text_repel(aes(x = `2000`, y = `2010`, label = abv), labels, nudge_y = .15, size = 3)

plot2 <- ggplot(df) + 
  geom_abline(size = .25) + 
  geom_point(
    aes(x = `2010`, y = `2022`, color = sector5), 
    size = ifelse(df$sector != "Aggregate", 4, 6),
    alpha = ifelse(df$sector != "Aggregate", .8, 1),
    shape = ifelse(df$sector != "Aggregate", 16, 18)
  ) +
  scale_x_continuous(name = years[2], limits = c(6, 11)) + 
  scale_y_continuous(limits = c(6, 11), sec.axis = sec_axis(~., name = years[3])) + 
  colors + 
  theme(
    axis.title.x = element_text(size = 9),
    axis.title.y.left = element_blank(),
    axis.title.y.right = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(size = 8),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 2, 2, 8)
  ) +
  
  # Add labels
  annotate(
    "text", label = "Shortened", 
    x = 10.5, y = 6.5, angle = 45, hjust = .5, vjust = 1, size = 3, color = "gray50"
  ) + 
  geom_point(aes(x = `2010`, y = `2022`), labels, size = 4, shape = 1) + 
  geom_text_repel(aes(x = `2010`, y = `2022`, label = abv), labels, nudge_y = .15, size = 3)

legend <- get_legend(
    plot1 + 
    guides(color = guide_legend(
      nrow = 3, 
      override.aes = list(size = c(rep(3, 5), 4), alpha = 1, shape = c(rep(16, 5), 18))
    )) + 
    theme(
      legend.key = element_blank(),
      legend.key.size = unit(.75, "lines"),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(t = -8)
  ))

plot <- plot_grid(plot1, plot2, ncol = 2, align = "h")
plot <- plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .25))

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 10, unit = "cm"
)

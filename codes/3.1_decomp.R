# EXPORTS BY VALUE ADDED CATEGORIES

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(arrow)
library(readxl)
library(tidyverse)

filename <- "3.1_decomp"

select <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(name == "Kazakhstan") |> 
  pull(mrio)

# Data --------------------------------------------------------------------

here("..", "..", "MRIO Processing", "data", "ta62.parquet") |> 
  read_parquet() |> 
  filter(breakdown == "es" & s == select & t < 2017) |>
  bind_rows(
    here("..", "..", "MRIO Processing", "data", "ta.parquet") |> 
      read_parquet() |> 
      filter(breakdown == "es" & s == select)
  ) |> 
  summarize(across(exports:pdc2, sum), .by = t) |>
  mutate(
    davax = davax1 + davax2,
    rex = rex1 + rex2 + rex3,
    ref = ref1 + ref2,
    pdc = pdc1 + pdc2
  ) |> 
  select(t, exports, davax1, davax:ref, fva, pdc) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv()

df_plot <- df |> select(-exports, -davax1) |> 
  add_row(t = 2001) |> 
  pivot_longer(cols = -t, names_to = "category") |>
  mutate(category = factor(category, levels = rev(colnames(df)[-1:-3])))

# Box production-based GVC

box <- df |> select(t, exports, davax1) |>
  mutate(
    x = setdiff(1:(n() + 1), 2),
    max = exports / 1000,
    min = davax1 / 1000
  )

plot <- ggplot() + 
  geom_bar(
    aes(x = factor(t), y = value / 1000, fill = category),
    df_plot, stat = "identity", position = "stack", width = .7
  ) +
  geom_hline(yintercept = 0, size = .25, color = "gray25") + 
  scale_x_discrete(labels = c(2000, "", 2007, "08", "09", 10:22)) +
  scale_y_continuous(name = "$ billion", limits = c(0, 105)) +
  scale_fill_manual(
    labels = colnames(df)[-1:-3] |> rev() |> toupper(),
    values = rev(c("#007db7", "#00A5D2", "#63CCEC", "#8DC63F", "#E9532B"))
  ) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(
    plot.margin = margin(13, 2, 10, 2),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9, angle = 0, hjust = 1, margin = margin(r = -20)),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9, margin = margin(-8, 0, 0, 0)),
    axis.text.y = element_text(size = 9),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 10, 0, 0)),
    legend.position = "bottom",
    legend.box.margin = margin(-5, 15, 0, 0), 
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray75", size = .25, linetype = "dashed")
  ) + 
  
  # Add boxes representing production-based GVCs
  geom_rect(
    aes(ymin = min, ymax = max, xmin = x - .34, xmax = x + .34),
    box, fill = NA, color = "black"
  ) +
  annotate(
    "text", label = "Production-based\nGVCs", 
    x = 13, y = 90, size = 3, hjust = .5, color = "gray25"
  ) + 
  geom_segment(
    data = df_plot |> slice(1),
    x = 11.45, xend = 10.35, y = 90, yend = 83, linewidth = .5, color = "gray25"
  )

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 10, unit = "cm"
)

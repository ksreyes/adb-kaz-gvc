# EXPORTS DECOMPOSITION BY SECTOR

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)

filename <- "3.5_sector_decomp"

year <- 2022

select <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(name == "Kazakhstan") |> 
  pull(mrio)

# Data --------------------------------------------------------------------

sectors <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel() |> 
  distinct(ind, name_short)

ta <- here("..", "..", "MRIO Processing", "data", "ta.parquet") |> 
  read_parquet() |>
  filter(t == year & s == select & breakdown == "es") |> 
  mutate(
    davax = davax1 + davax2,
    rex = rex1 + rex2 + rex3,
    ref = ref1 + ref2,
    pdc = pdc1 + pdc2
  ) |> 
  left_join(sectors, by = c("i" = "ind")) |> 
  select(sector = name_short, exports, davax, rex, ref, fva, pdc) |> 
  summarize(across(exports:pdc, sum), .by = sector)

ta |> add_row(
    ta |> summarize(across(exports:pdc, sum)) |> mutate(sector = "Aggregate"),
    .before = 1
  ) |> 
  arrange(davax / exports) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |> 
  mutate(
    na = ifelse(exports == 0, 1, 0),
    face = case_when(sector == "Aggregate" ~ "bold", .default = "plain")
  ) |> 
  arrange(-davax / exports) |> 
  arrange(desc(na))

df_plot <- df |> 
  pivot_longer(cols = davax:na, names_to = "category")

plot <- ggplot(df_plot, aes(
    x = value, 
    y = factor(sector, levels = df$sector), 
    fill = factor(category, levels = rev(colnames(df)[3:8]))
  )) + 
  geom_bar(stat = "identity", position = "fill", width = .7) +
  scale_fill_manual(
    labels = rev(c("DAVAX", "REX", "REF", "FVA", "PDC", "")),
    values = rev(c("#007db7", "#00A5D2", "#63CCEC", "#8DC63F", "#E9532B", "gray75"))
  ) + 
  scale_x_continuous(breaks = c(.25, .5, .75), labels = function(x) str_c(100 * x, "%")) + 
  guides(fill = guide_legend(
      reverse = TRUE, nrow = 1,
      override.aes = list(fill = c("#007db7", "#00A5D2", "#63CCEC", "#8DC63F", "#E9532B", "white")))
  ) + 
  theme(
    plot.margin = margin(15, 2, 12, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 7.5, margin = margin(r = -12), face = df$face, color = "black"),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 10, 0, 0)),
    legend.position = "bottom",
    legend.box.margin = margin(-5, 40, 0, 0), 
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank()
  ) + 
  
  # Add rectangle indicating Aggregate and grid lines
  geom_rect(
    aes(xmin = 0, xmax = 1, ymin = which(df$sector == "Aggregate") - .34, ymax = which(df$sector == "Aggregate") + .34),
    df_plot |> slice(1), fill = NA, color = "black"
  ) + 
  geom_vline(xintercept = .25, size = .25, color = "gray25", linetype = "dashed") + 
  geom_vline(xintercept = .5, size = .25, color = "gray25", linetype = "dashed") + 
  geom_vline(xintercept = .75, size = .25, color = "gray25", linetype = "dashed")

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 17, unit = "cm"
)

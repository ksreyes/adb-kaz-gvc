# GVC TRADE GROWTH CURRENT VS. CONSTANT

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(arrow)
library(readxl)
library(tidyverse)

filename <- "5.3_constant_growth"

select <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(name == "Kazakhstan") |> 
  pull(mrio62)

sectors <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel() |> 
  distinct(ind, name_short) |>
  add_row(ind = 0, name_short = "Aggregate", .before = 1)


# Data --------------------------------------------------------------------

preprocess <- function (df, select, label) {
  
  df_agg <- df |> 
    filter(t != 2000 & s == select & breakdown == "es") |> 
    summarize(across(exports:pdc2, sum), .by = c(t, s)) |> 
    mutate(
      i = 0,
      level = rex1 + rex2 + rex3 + ref1 + ref2 + fva + pdc1 + pdc2,
      growth = level / lag(level) - 1
    ) |> 
    select(s, i, t, level, growth)
  
  df_sectors <- df |> 
    filter(t != 2000 & s == select & breakdown == "es") |> 
    summarize(across(exports:pdc2, sum), .by = c(t, s, i)) |> 
    mutate(
      level = rex1 + rex2 + rex3 + ref1 + ref2 + fva + pdc1 + pdc2,
      growth = level / lag(level) - 1,
      .by = c(s, i)
    ) |> 
    select(s, i, t, level, growth)
  
  df_all <- df_agg |> bind_rows(df_sectors) |> arrange(s, i)
  
  colnames(df_all)[4] = str_c(label, "_", colnames(df_all)[4])
  colnames(df_all)[5] = str_c(label, "_", colnames(df_all)[5])
  
  return(df_all)
}

df <- here("..", "..", "MRIO Processing", "data", "ta62.parquet") |> 
  read_parquet() |> 
  preprocess(select, "current") |> 
  left_join(
    here("..", "..", "MRIO Processing", "data", "ta62-const.parquet") |> 
      read_parquet() |> 
      preprocess(select, "constant")
  ) |> 
  left_join(sectors, by = c("i" = "ind")) |> 
  mutate(name_short = factor(name_short, levels = sectors |> pull(name_short)))

df |> write_csv(here("data", "final", str_glue("{filename}.csv")))


# Plot --------------------------------------------------------------------

df_plot <- df |>
  select(t, s, i, name_short, ends_with("_growth")) |> 
  pivot_longer(cols = current_growth:constant_growth, names_to = "gvc_trade", values_to = "value")

plot <- ggplot(
    df_plot |> filter(i %in% c(0, 2, 12, 20)), 
    aes(x = t, y = value, color = gvc_trade)
  ) + 
  geom_hline(yintercept = 0, size = .5, color = "gray75") + 
  geom_line(size = .75) +
  facet_wrap(~ name_short) +
  scale_y_continuous(labels = function(x) str_c(100 * x, "%")) +
  scale_color_manual(
    labels = c("Constant price", "Current price"),
    values = c("#007db7", "#e9532b")
  ) + 
  theme(
    plot.margin = margin(15, 2, 10, 2),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 10, 0, 0)),
    legend.box.margin = margin(-10, 0, 0, 0), 
    legend.position = "bottom",
    strip.background = element_rect(fill = "#007db7"),
    strip.text = element_text(face = "bold", color = "white"),
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  plot,
  device = cairo_pdf,
  width = 16, height = 14, unit = "cm"
)

######### END #########
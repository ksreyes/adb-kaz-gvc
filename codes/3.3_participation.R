# GVC PARTICIPATION

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)

filename <- "3.3_participation"

# Data --------------------------------------------------------------------

select <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(name == "Kazakhstan") |> 
  pull(mrio)

here("..", "..", "MRIO Processing", "data", "gvcp62.parquet") |> 
  read_parquet() |> 
  filter(s == select & agg == 0 & t < 2017) |> 
  bind_rows(
    here("..", "..", "MRIO Processing", "data", "gvcp.parquet") |> 
      read_parquet() |> 
      filter(s == select & agg == 0)
  ) |> 
  select(t, gvcp_trade_f, gvcp_trade_b, gvcp_prod) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv()

interpolate <- function(vec) {
  dist <- (vec[2] - vec[1]) / 3
  return(c(vec[1] + dist, vec[1] + 2 * dist))
}

df_plot <- df |>
  bind_rows(tibble(
    t = 2001:2002,
    gvcp_trade_f = interpolate(df$gvcp_trade_f),
    gvcp_trade_b = interpolate(df$gvcp_trade_b),
    gvcp_prod = interpolate(df$gvcp_prod)
  )) |>
  pivot_longer(cols = -t, names_to = "approach", values_to = "rate") |> 
  mutate(approach = factor(approach, levels = colnames(df)))

plot <- ggplot(
    df_plot, 
    aes(x = factor(t), y = rate, color = approach, group = approach)
  ) + 
  geom_line(data = subset(df_plot, t <= 2007), linetype = "dashed", linewidth = 1.25) +
  geom_line(data = subset(df_plot, t >= 2007), size = 1.25) + 
  scale_x_discrete(labels = c(
    2000, "", "", 2007, "", "", 10, "", "", "", "", 15, "", "", "", "", 20, "", ""
  )) + 
  scale_y_continuous(labels = function(x) str_c(100 * x, "%")) +
  scale_color_manual(
    labels = c("Trade-based, forward", "Trade-based, backward", "Production-based"),
    values = c("#007db7", "#63CCEC", "#e9532b")
  ) + 
  theme(
    plot.margin = margin(15, 2, 10, 2),
    axis.title = element_blank(),
    axis.line = element_line(linewidth = .25, color = "black"),
    axis.ticks.x = element_line(
      linewidth = .25, 
      color = c("black", NA, NA, rep("black", 15))
    ),
    axis.ticks.y = element_line(linewidth = .25, color = "black"),
    axis.text.x = element_text(size = 9, color = "black", margin = margin(t = 5)),
    axis.text.y = element_text(size = 9, color = "black"),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank()
  ) + 
  
  # Add labels
  geom_segment(x = 14, xend = 14, y = 0.3647946, yend = 0.3647946 + .011, linewidth = .25, color = "black") + 
  geom_point(x = 14, y = 0.3647946, size = .15, color = "black") +
  annotate(
    "text", label = "Trade-based, forward", 
    x = 14, y = 0.3647946 + .02, vjust = 0, hjust = .5, size = 3.5
  ) + 
  geom_segment(x = 9, xend = 9, y = 0.12346576, yend = 0.12346576 + .011, linewidth = .25, color = "black") + 
  geom_point(x = 9, y = 0.12346576, size = .02, color = "black") +
  annotate(
    "text", label = "Trade-based, backward",
    x = 9, y = 0.12346576 + .02, vjust = 0, hjust = .5, size = 3.5
  ) + 
  geom_segment(x = 12, xend = 12, y = 0.2453017, yend = 0.2453017 - .011, linewidth = .25, color = "black") + 
  geom_point(x = 12, y = 0.2453017, size = .15, color = "black") +
  annotate(
    "text", label = "Production-based",
    x = 12, y = 0.2453017 - .02, vjust = 1, hjust = .5, size = 3.5
  )

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 9, unit = "cm"
)

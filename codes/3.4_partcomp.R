# INTER-COUNTRY COMPARISON OF GVC PARTICIPATION RATES

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

filename <- "3.4_partcomp"

years <- c(2000, 2010, 2022)

select <- c(
  "Kazakhstan", "Russian Federation", "Uzbekistan", "Kyrgyz Republic", 
  "Saudi Arabia", "Brunei Darussalam", "Turkey", "Mongolia", "Georgia"
)

# Data --------------------------------------------------------------------

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(name %in% select) |>
  mutate(s = mrio)

gvcp <- here("..", "..", "MRIO Processing", "data", "gvcp-62.parquet") |> 
  read_parquet() |> 
  filter(t %in% years[1:2] & agg == 0) |> 
  bind_rows(
    here("..", "..", "MRIO Processing", "data", "gvcp-72.parquet") |> 
      read_parquet() |> 
      filter(t == years[3] & agg == 0)
  ) |> 
  select(-ends_with(c("_b", "_f")))

gvcp |> filter(s %in% countries$s) |> 
  bind_rows(
    gvcp |> summarize(across(exports:gvc_prod, sum), .by = t) |> 
      mutate(s = 0, gvcp_trade = gvc_trade / exports, gvcp_prod = gvc_prod / va)
  ) |> 
  left_join(countries) |> 
  arrange(name) |> 
  select(t, name, gvcp_trade, gvcp_prod) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> read_csv()

highlights <- df |> filter(t == years[3]) |> 
  mutate(
    face = case_when(name %in% c("World", select[1]) ~ "bold", .default = "plain"),
    color = case_when(name == "World" ~ "#007db7", .default = "black"),
    size = case_when(name == select[1] ~ 9, .default = 8),
  )

plot1 <- ggplot(df, aes(x = gvcp_trade, y = fct_reorder2(name, t, gvcp_trade, .desc = FALSE))) + 
  geom_hline(yintercept = "World", color = "gray50", size = 5, alpha = .2) + 
  geom_line(color = "#63CCEC", size = 1.5) + 
  geom_point(aes(color = factor(t)), size = 2.5) + 
  scale_x_continuous(breaks = c(.4, .5), labels = function(x) str_c(100 * x, "%")) + 
  scale_color_manual("Year", values = c("#007db7", "#00A5D2", "#E9532B")) + 
  labs(title = "Trade-based") + 
  theme(
    plot.margin = margin(10, 6, 13, 2),
    plot.title = element_text(hjust = .5, size = 9, face = "bold", margin = margin(5, 0, 5, 0)),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(
      size = highlights |> arrange(gvcp_trade) |> pull(size),
      face = highlights |> arrange(gvcp_trade) |> pull(face),
      color = highlights |> arrange(gvcp_trade) |> pull(color),
    ),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray20", size = .5),
    panel.grid.major.x = element_line(color = "gray75", size = .25, linetype = "dashed"),
    panel.grid.major.y = element_blank()
  )

plot2 <- ggplot(df, aes(x = gvcp_prod, fct_reorder2(name, t, gvcp_prod, .desc = FALSE))) + 
  geom_hline(yintercept = "World", color = "gray50", size = 5, alpha = .2) + 
  geom_line(color = "#63CCEC", size = 1.5) + 
  geom_point(aes(color = factor(t)), size = 2.5) + 
  scale_x_continuous(breaks = c(.2, .4), labels = function(x) str_c(100 * x, "%")) + 
  scale_color_manual("Year", values = c("#007db7", "#00A5D2", "#E9532B")) + 
  guides(color = guide_legend(title = NULL, title.position = "right")) +
  labs(title = "Production-based") + 
  theme(
    plot.margin = margin(10, 2, 13, 6),
    plot.title = element_text(hjust = .5, size = 9, face = "bold", margin = margin(5, 0, 5, 0)),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(
      size = highlights |> arrange(gvcp_prod) |> pull(size),
      face = highlights |> arrange(gvcp_prod) |> pull(face),
      color = highlights |> arrange(gvcp_prod) |> pull(color),
    ),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_rect(
      fill = "white", color = "gray75", linewidth = .25
    ),
    legend.spacing = unit(0, "lines"),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9),
    legend.position = c(.78, .34),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray20", size = .5),
    panel.grid.major.x = element_line(color = "gray75", size = .25, linetype = "dashed"),
    panel.grid.major.y = element_blank()
  )

plot <- plot_grid(plot1, plot2, ncol = 2, align = "h")

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 6.5, unit = "cm"
)

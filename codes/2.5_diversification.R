# TRADE DIVERSIFICATION

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(tidyverse)
library(DBI)
library(duckdb)

filename <- "2.5_diversification"

years <- c(2005, 2010, 2015, 2021)

include <- c(
    "Saudi Arabia",
    "Kyrgyz Republic",
    "Kazakhstan",
    "Uzbekistan",
    "Russian Federation"
  )

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel(sheet = "BACI") |>
  drop_na(iso_num) |> 
  left_join(
    here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
      read_excel(sheet = "Consolidated")
  ) |> 
  select(baci_num, name) |> 
  filter(name %in% include)


# Data --------------------------------------------------------------------

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

baci <- con |> 
  dbGetQuery(sprintf(
    "SELECT t, i, substring(k, -6, 4) AS k4, sum(v) AS v
    FROM '../../../Repos/baci/final/BACI_HS02_V202301.parquet'
    WHERE t in (%s) AND i IN (%s)
    GROUP BY t, i, k4",
    str_flatten_comma(years),
    str_flatten_comma(countries$baci_num)
  ))

dbDisconnect(con, shutdown=TRUE)

baci |> 
  mutate(share2 = (v / sum(v)) ^ 2, .by = c(t, i)) |> 
  summarize(hhi = sum(share2), .by = c(t, i)) |> 
  left_join(countries, by = c("i" = "baci_num")) |> 
  select(-i) |> 
  arrange(name, t) |> 
  pivot_wider(names_from = name, values_from = hhi) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))


# Plot --------------------------------------------------------------------

plot <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |> 
  pivot_longer(cols = -t, names_to = "name", values_to = "hhi") |> 
  
  ggplot(aes(x = fct_reorder2(name, t, hhi), y = hhi, fill = factor(t))) +
  geom_bar(stat = "identity", position = position_dodge(width = .8), width = .7) +
  geom_hline(yintercept = 0, linewidth = .25, color = "gray25") +
  scale_fill_manual(values = rev(c(
    "#007db7", "#00A5D2", "#63CCEC", "#9EE4FF"
  ))) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    plot.margin = margin(8, 2, 13, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      size = c(9, 9, 10, 9, 9),
      color = "black",
      face = c("plain", "plain", "bold", "plain", "plain"),
      margin = margin(-5, 0, 0, 0)
    ),
    axis.text.y = element_text(size = 9),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 5, 0, 0)),
    legend.position = c(.75, .90),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray75", linewidth = .25, linetype = "dashed")
  )

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  plot,
  device = cairo_pdf,
  width = 16, height = 10, unit = "cm"
)

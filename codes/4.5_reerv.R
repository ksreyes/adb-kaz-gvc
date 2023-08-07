# REER APPRECIATION: MEAN VERSUS VOLATILITY

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)

filename <- "4.5_reerv"

# Highlighted countries
select <- "Kazakhstan"
hl1 <- c("Mongolia", "Kyrgyz Republic", "Turkey")
hl2 <- c("People's Republic of China", "Italy", "Brunei Darussalam", "United States")
hl3 <- c("Singapore", "Germany")
hl4 <- "Russian Federation"

# Data --------------------------------------------------------------------

here("..", "..", "MRIO Processing", "data", "reer", "reer62.parquet") |> 
  read_parquet() |> 
  filter(method == "pww" & s != 63) |> 
  summarize(mean = mean(reer), sd = sd(reer), .by = s) |> 
  left_join(
    here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> read_excel(), 
    by = c("s" = "mrio62")
  ) |> 
  select(name, mean, sd) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> read_csv()

plot <- ggplot(mapping = aes(x = 100 * sd, y = 100 * mean, label = name)) + 
  geom_point(
    data = subset(df, !(name %in% c(hl1, hl2))),
    shape = 16, color = "gray75", size = 3, alpha = .7
  ) + 
  geom_point(
    data = subset(df, name %in% c(hl1, hl2, hl3, hl4)), 
    shape = 16, color = "#007db7", size = 4, alpha = .8
  ) + 
  geom_text(
    data = subset(df, name %in% hl1), 
    nudge_y = .35, vjust = 0, size = 3
  ) + 
  geom_text(
    data = subset(df, name %in% hl2), 
    nudge_y = -.35, vjust = 1, size = 3
  ) + 
  geom_text(
    data = subset(df, name %in% hl3), 
    nudge_x = .2, nudge_y = .35, vjust = 0, size = 3
  ) + 
  geom_text(
    data = subset(df, name %in% hl4), 
    nudge_x = -.3, hjust = 1, vjust = .5, size = 3
  ) + 
  geom_point(
    data = subset(df, name == select), 
    shape = 21, fill = "#007db7", color = "black", size = 5
  ) + 
  geom_text(
    data = subset(df, name == select), 
    nudge_y = -.4, vjust = 1, size = 4, fontface = "bold"
  ) + 
  scale_x_continuous(name = "Standard deviation") +
  scale_y_continuous(name = "Mean appreciation", label = function(x) str_c(x, "%")) +
  theme(
    axis.title.x = element_text(size = 9, margin = margin(t = 5)),
    axis.title.y = element_text(size = 9, margin = margin(r = 5)),
    axis.text = element_text(size = 8),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray20", linewidth = .5),
    panel.grid.major = element_blank(),
    plot.margin = margin(15, 7, 15, 2)
  )

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 9, unit = "cm"
)

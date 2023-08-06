# REER

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

filename <- "4.4_reer"

select <- c("Russian Federation", "Kazakhstan")
display <- c("Mining", "Metals", "Aggregate")

sectors <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel() |> 
  distinct(ind, name_short) |>
  rename(i = ind, sector = name_short) |> 
  add_row(i = 0, sector = "Aggregate")

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  rename(s = mrio62)

select_ind <- filter(countries, name %in% select)$s
display_ind <- filter(sectors, sector %in% display)$i

# Data --------------------------------------------------------------------

here("..", "..", "MRIO Processing", "data", "reer", "reer62.parquet") |> 
  read_parquet() |> 
  filter(method %in% c("pww", "pww-sector") & s %in% select_ind & i %in% display_ind) |> 
  left_join(sectors) |> 
  left_join(countries) |> 
  arrange(name) |>
  select(name, sector, t, reer) |> 
  pivot_wider(names_from = sector, values_from = reer) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |> 
  pivot_longer(cols = -c(name, t), names_to = "sector", values_to = "reer") |> 
  mutate(sector = factor(sector, levels = display))

plot1 <- ggplot(
    filter(df, name == select[2]), 
    aes(x = factor(t), y = reer, color = sector, group = sector)
  ) + 
  geom_hline(yintercept = 0, size = .5, color = "gray75") + 
  geom_line(size = 1) + 
  labs(title = select[2]) + 
  scale_color_manual(values = c("#007DB7", "#00A5D2", "black")) + 
  scale_x_discrete(labels = c(2007, "", "09", "", 11, "", 13, "", 15, "", 17, "", 19, "", 21)) + 
  scale_y_continuous(
    limits = c(-.35, .35), labels = function(x) str_c(100 * x, "%")
  ) + 
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 8, hjust = .5, color = "black"),
    axis.ticks.x = element_line(size = .25, color = "black"),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 9, face = "bold", hjust = .5),
    plot.margin = margin(15, 2, 2, 2)
  )

plot2 <- ggplot(
    subset(df, name == select[1]), 
    aes(x = factor(t), y = reer, color = sector, group = sector)
  ) + 
  geom_hline(yintercept = 0, size = .5, color = "gray75") + 
  geom_line(size = 1) + 
  labs(title = select[1]) + 
  scale_color_manual(values = c("#007DB7", "#00A5D2", "black")) + 
  scale_x_discrete(labels = c(2007, "", "09", "", 11, "", 13, "", 15, "", 17, "", 19, "", 21)) + 
  scale_y_continuous(
    limits = c(-.35, .35),
    sec.axis = sec_axis(~., labels = function(x) str_c(100 * x, "%"))
  ) + 
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", margin = margin(t = 3)),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(size = 8, hjust = .5, color = "black"),
    axis.ticks.x = element_line(size = .25, color = "black"),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 9, face = "bold", hjust = .5),
    plot.margin = margin(15, 2, 2, 2)
  )

legend <- get_legend(plot1 + 
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(r = 5)),
    legend.position = "bottom",
    plot.margin = margin(2, 2, 15, 2)
  ))

plot <- plot_grid(plot1, plot2)
plot <- plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .2))

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 7.5, unit = "cm"
)

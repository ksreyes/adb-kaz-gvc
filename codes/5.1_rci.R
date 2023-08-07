# REGIONAL CONCENTRATION INDEX

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

filename <- "5.1_rci"

rtas <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |>
  read_excel() |>
  select(starts_with("rta_")) |>
  colnames() |>
  str_sub(5, -1)

# Data --------------------------------------------------------------------

here("..", "..", "MRIO Processing", "data", "rci62.parquet") |> 
  read_parquet() |> 
  filter(t < 2017) |> 
  bind_rows(
    here("..", "..", "Mrio Processing", "data", "rci.parquet") |> 
      read_parquet()
  ) |> 
  select(method, t, rta, rci) |> 
  pivot_wider(names_from = rta, values_from = rci) |> 
  arrange(method) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |> 
  pivot_longer(cols = -c(method, t), names_to = "rta", values_to = "rci") |> 
  pivot_wider(id_cols = c(method, rta), names_from = t, values_from = rci) |> 
  mutate(
    `2001` = `2000` + (`2007` - `2000`) / 3,
    `2002` = `2000` + 2 * (`2007` - `2000`) / 3
  ) |> 
  pivot_longer(cols = -c(method, rta), names_to = "t", values_to = "rci") |> 
  pivot_wider(id_cols = c(rta, t), names_from = method, values_from = rci)

colors <- c("#007DB7", "#8DC63F", "#E9532B", "#FDB415", "black")
xlabs <- c(2000, "", "", 2007, "", "", 10, "", "", "", "", 15, "", "", "", "", 20, "", "")
xticks <- case_when(
    df |> arrange(t) |> pull(t) |> unique() %in% c(2001, 2002) ~ NA, 
    .default = "black"
  )

theme <- theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.ticks.x = element_line(size = .25, color = xticks),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 9, face = "bold", hjust = .5)
  )

plot1 <- ggplot(df, aes(x = t, y = `gross exports`, color = rta, group = rta)) + 
  geom_line(data = subset(df, t <= 2007), size = .75, linetype = "dashed") + 
  geom_line(data = subset(df, t >= 2007), size = .75) +
  labs(title = "Gross exports") + 
  scale_x_discrete(labels = xlabs) +
  scale_y_continuous(limits = c(0, 5.5), breaks = c(1, 3, 5)) +
  scale_color_manual(labels = toupper(rtas), values = colors) + 
  theme + theme(plot.margin = margin(15, 4, 2, 2))

plot2 <- ggplot(df, aes(x = t, y = `end-to-end`, color = rta, group = rta)) + 
  geom_line(data = subset(df, t <= 2007), size = .75, linetype = "dashed") + 
  geom_line(data = subset(df, t >= 2007), size = .75) +
  labs(title = "End-to-end") + 
  scale_x_discrete(labels = xlabs) +
  scale_y_continuous(
    limits = c(0, 5.5), breaks = c(1, 3, 5), 
    sec.axis = sec_axis(~., breaks = c(1, 3, 5))
  ) +
  scale_color_manual(labels = toupper(rtas), values = colors) + 
  theme + theme(
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(size = 8, hjust = .5),
    plot.margin = margin(15, 2, 2, 4)
  )

legend <- get_legend(plot1 + 
  theme(
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(r = 8)),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box.margin = margin(t = -10), 
    plot.margin = margin(2, 2, 15, 2)
  ))

plot <- plot_grid(plot1, plot2)
plot <- plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .2))

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 8, unit = "cm"
)

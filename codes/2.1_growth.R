# GDP GROWTH

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(tidyverse)

filename <- "2.1_growth"
forecast <- 2023:2028

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |>
  mutate(forecast = ifelse(t %in% forecast, TRUE, FALSE))

plot <- ggplot(df) +
  geom_rect(
    aes(xmin = 2022.5, xmax = 2028.5, ymin = -Inf, ymax = Inf),
    slice(df, 1), fill = "gray75", alpha = .3
  ) +
  geom_bar(
    aes(x = t, y = g, alpha = "GDP growth"),
    fill = ifelse(df |> drop_na(g) |> pull(forecast), "#6DCFF6", "#00A5D2"),
    width = .7, stat = "identity"
  ) +
  geom_line(
    aes(x = t, y = oil / 10, color = "Brent crude price (rhs)"),
    linewidth = 1
  ) +
  scale_alpha_manual(values = 1) +
  scale_y_continuous(
    labels = function(x) str_c(x, "%"),
    limits = c(-12.58, 13.5),
    sec.axis = sec_axis(
      ~ . * 10,
      labels = function(x) ifelse(x >= 0, str_c("$", x), "")
    )
  ) +
  geom_hline(yintercept = 0, size = .25, color = "gray25") +
  annotate(
    "text", label = "Forecasts",
    x = 2025.5, y = 13.5, size = 3, color = "gray25"
  ) +
  guides(
    alpha = guide_legend(
      order = 1, title = NULL,
      title.position = "right", override.aes = list(fill = "#00A5D2")
    ),
    color = guide_legend(title = NULL, title.position = "right")
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 9),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.spacing = unit(0, "lines"),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9),
    legend.position = c(.85, .15),
    legend.box.background = element_rect(fill = "white", color = "gray75"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray75", linewidth = .25, linetype = "dashed"),
    plot.margin = margin(10, 2, 15, 2)
  )

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 10, unit = "cm"
)

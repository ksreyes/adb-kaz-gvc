# GDP GROWTH

rm(list = ls())
library(here)
library(tidyverse)

df <- here("data", "final", "2.1_growth.csv") %>% 
  read_csv() %>%
  mutate(
    decade = floor(t / 10) * 10,
    forecast = ifelse(t %in% 2023:2028, TRUE, FALSE)
  )

plot <- ggplot(df) +
  geom_rect(
    data = df[1, ],
    mapping = aes(xmin = 2022.5, xmax = 2028.5, ymin = -Inf, ymax = Inf),
    fill = "gray75",
    alpha = .3
  ) +
  geom_bar(
    mapping = aes(x = t, y = g, alpha = "GDP growth"),
    fill = ifelse(df %>% drop_na(g) %>% pull(forecast), "#6DCFF6", "#00A5D2"),
    width = .7,
    stat = "identity"
  ) +
  geom_line(
    mapping = aes(x = t, y = oil / 10, color = "Brent crude price (rhs)"),
    linewidth = 1
  ) +
  scale_alpha_manual(values = 1) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(-12.58, 13.5),
    sec.axis = sec_axis(
      ~ . * 10,
      labels = function(x) ifelse(x >= 0, paste0("$", x), "")
    )
  ) +
  geom_hline(yintercept = 0, size = .25, color = "gray25") +
  annotate(
    "text",
    x = 2025.5,
    y = 13.5,
    label = "Forecasts",
    size = 3,
    color = "gray25"
  ) +
  guides(
    alpha = guide_legend(
      order = 1,
      title = NULL,
      title.position = "right",
      override.aes = list(fill = "#00A5D2")
    ),
    color = guide_legend(
      title = NULL,
      title.position = "right"
    )
  ) +
  theme(
    plot.margin = margin(10, 2, 15, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 9),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill = "white", color = "gray75"),
    legend.spacing = unit(0, "lines"),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9),
    legend.position = c(.85, .15),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray75",
      linewidth = .25,
      linetype = "dashed"
    )
  )

ggsave(
  "figures/2.1_growth.pdf",
  plot,
  device = cairo_pdf,
  width = 16,
  height = 10,
  unit = "cm"
)

######### END #########
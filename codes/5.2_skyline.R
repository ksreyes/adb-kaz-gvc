# SKYLINE CHART

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(tidyverse)

filename <- "5.2_skyline"


# Data --------------------------------------------------------------------

df <- here("data", "interim", "skyline.csv") |> 
  read_csv()

N <- nrow(df)
x <- df |> pull(x)
sf <- df |> pull(sf)
se <- df |> pull(se)
sm <- df |> pull(sm)

# Geometry of skylines

width <- x / sum(x)
right <- cumsum(width)
left <- c(0, right[-N])
height1 <- x / sf
height2 <- (sm + x) / sf
mid <- (left + right) / 2
label <- df |> pull(abv)
label[which(width < .015)] <- ""

df <- tibble(i = df |> pull(abv), width, left, right, height1, height2, mid, label)

pos_primary <- right[2] / 2
pos_manuf <- (right[2] + right[18]) / 2
pos_services <- (right[18] + right[35]) / 2


# Plot --------------------------------------------------------------------

plot <- ggplot(df) + 
  
  # Shading to indicator broad sectors
  geom_rect(
    aes(ymin = -Inf, ymax = Inf, xmin = 0, xmax = right[2]), 
    data = df[1, 1],
    fill = "gray90", alpha = .5
  ) + 
  geom_rect(
    aes(ymin = -Inf, ymax = Inf, xmin = right[18], xmax = right[35]), 
    data = df[1, 1],
    fill = "gray90", alpha = .5
  ) + 
  
  # Rectangles
  geom_rect(
    aes(ymin = 0, ymax = height1, xmin = left, xmax = right, fill = "Regional Self-Sufficiency Ratio"), 
    color = "#007DB7", size = .25
  ) + 
  geom_rect(
    aes(ymin = height1, ymax = height2, xmin = left, xmax = right, fill = "Imports from Outside Region"), 
    color = "#E9532B", size = .25
  ) + 
  
  # 100% line
  geom_hline(yintercept = 1, color = "black", size = .5) + 
  
  # Labels
  geom_text(
    mapping = aes(x = mid, y = -.03, label = label), 
    size = 2, angle = 90, hjust = 1, vjust = .5
  ) +
  geom_text(
    x = pos_primary, y = -.35, size = 2.5, hjust = .5, vjust = 1,
    label = "Primary"
  ) +
  geom_text(
    x = pos_manuf, y = -.35, size = 2.5, hjust = .5, vjust = 1,
    label = "Manufacturing"
  ) +
  geom_text(
    x = pos_services, y = -.35, size = 2.5, hjust = .5, vjust = 1,
    label = "Services"
  ) +
  
  # Style
  scale_y_continuous(
    limits = c(-.3, 4),
    breaks = seq(0, 5, .5),
    labels = function(x) paste0(100 * x, "%")
  ) + 
  scale_fill_manual(
    values = c("#00A5D2", "#F57F29"),
    breaks = c("Regional Self-Sufficiency Ratio", "Imports from Outside Region")
  ) + 
  guides(fill = guide_legend(override.aes = list(linetype = "blank"))) + 
  theme(
    plot.margin = margin(15, 2, 10, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 10, 0, 0)),
    legend.box.margin = margin(-10, 0, 0, 0), 
    legend.position = "bottom",
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

######### END #########
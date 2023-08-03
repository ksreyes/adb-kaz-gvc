# POSITION OF SELECTED SECTORS AND SELECTED COUNTRIES

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

filename <- "4.2_position"

years <- c(2010, 2022)
display <- c("Mining", "Metals")
select1 <- c("Russian Federation", "Brunei Darussalam", "Norway", "United States", "Kazakhstan")
select2 <- c("PRC", "Japan", "United States", "Germany", "Kazakhstan")

sectors <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel() |> 
  distinct(ind, name_short) |>
  rename(i = ind, sector = name_short)

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  filter(!(is.na(mrio))) |> 
  rename(s = mrio) |> 
  mutate(name = replace(name, name == "People's Republic of China", "PRC"))

# Data --------------------------------------------------------------------

lengths <- here("..", "..", "MRIO Processing", "data", "lengths62.parquet") |> 
  read_parquet() |> 
  filter(t == years[1]) |> 
  bind_rows(
    here("..", "..", "MRIO Processing", "data", "lengths.parquet") |> 
      read_parquet() |> 
      filter(t == years[2])
  ) |> 
  filter(agg == 35)

lengths |> 
  filter(
    s %in% subset(countries, name %in% select1)$s & 
      i == subset(sectors, sector == display[1])$i
  ) |> 
  bind_rows(
    lengths |> 
      filter(
        s %in% subset(countries, name %in% select2)$s & 
          i == subset(sectors, sector == display[2])$i
  )) |> 
  left_join(countries) |> 
  left_join(sectors) |> 
  select(sector, name, t, PLv_GVC, PLy_GVC) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> read_csv()

points <- function(df, display, country, u) {
  df <- subset(df, sector == display & name == country)
  x <- df$PLy_GVC[1] + u
  y <- df$PLv_GVC[1] + u
  xend <- df$PLy_GVC[2] + u
  yend <- df$PLv_GVC[2] + u
  return(c(x, y, xend, yend))
}

# Display sector 1

plot1 <- df |> filter(sector == display[1]) |> 
  ggplot(aes(x = PLy_GVC, y = PLv_GVC, color = factor(t))) + 
  geom_abline(size = .35, color = "gray60") + 
  annotate(
    "text", label = "Upstream", x = 3.62, y = 3.66, 
    angle = 45, hjust = .5, vjust = 0, size = 2.5, color = "gray50"
  ) + 
  annotate(
    "text", label = "Downstream", x = 3.66, y = 3.62, 
    angle = 45, hjust = .5, vjust = 1, size = 2.5, color = "gray50"
  ) + 
  geom_point(shape = 16, size = 4, alpha = .8) + 
  labs(title = display[1]) + 
  scale_color_manual(values = c("#007DB7", "#e9532b")) + 
  scale_x_continuous(limits = c(3.5, 4.75)) +
  scale_y_continuous(limits = c(3.5, 4.75)) +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 3, alpha = 1))) + 
  theme(
    plot.margin = margin(14, 2, 2, 5),
    plot.title = element_text(size = 9, hjust = .5, face = "bold"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  # Russian Federation
  geom_segment(
    x = points(df, display[1], select1[1], .01)[1], 
    y = points(df, display[1], select1[1], .055)[2], 
    xend = points(df, display[1], select1[1], -.01)[3], 
    yend = points(df, display[1], select1[1], -.055)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[1] & name == select1[1]),
    nudge_x = .16, nudge_y = -.06, hjust = 1, vjust = 1, size = 2.5, color = "black"
  ) +
  
  # Brunei Darussalam
  geom_segment(
    x = points(df, display[1], select1[2], .05)[1], 
    y = points(df, display[1], select1[2], -.015)[2], 
    xend = points(df, display[1], select1[2], -.05)[3], 
    yend = points(df, display[1], select1[2], .015)[4],
    lineend = "butt", linewidth = .5, linejoin = "mitre", colour = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[1] & name == select1[2]), 
    nudge_x = .04, nudge_y = -.04, hjust = 1, vjust = 1, size = 2.5, color = "black",
  ) +

  # Norway
  geom_segment(
    x = points(df, display[1], select1[3], -.02)[1], 
    y = points(df, display[1], select1[3], .02)[2], 
    xend = points(df, display[1], select1[3], .02)[3], 
    yend = points(df, display[1], select1[3], -.02)[4] ,
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[1] & name == select1[3]),
    nudge_x = .06, nudge_y = 0, hjust = 0, vjust = .5, size = 2.5, color = "black"
  ) +
  
  # United States
  geom_curve(
    x = points(df, display[1], select1[4], .035)[1], 
    y = points(df, display[1], select1[4], .045)[2], 
    xend = points(df, display[1], select1[4], -.01)[3], 
    yend = points(df, display[1], select1[4], .05)[4],
    lineend = "butt", linewidth = .5, curvature = -.5, colour = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[2] & sector == display[1] & name == select1[4]),
    nudge_x = .04, nudge_y = .04, hjust = 0, vjust = 0, size = 2.5, color = "black"
  ) +
  
  # Kazakhstan
  geom_segment(
    x = points(df, display[1], select1[5], .05)[1], 
    y = points(df, display[1], select1[5], -.005)[2], 
    xend = points(df, display[1], select1[5], -.05)[3], 
    yend = points(df, display[1], select1[5], .005)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, color = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[1] & name == select1[5]),
    nudge_x = .1, nudge_y = -.06, hjust = 1, vjust = 1, size = 2.5, color = "black", fontface = "bold",
  )
  
# Display sector 2

plot2 <- df |> filter(sector == display[2]) |> 
  ggplot(aes(x = PLy_GVC, y = PLv_GVC, color = factor(t))) + 
  geom_abline(size = .35, color = "gray60") + 
  annotate(
    "text", label = "Upstream", x = 4.68, y = 4.72, 
    angle = 45, hjust = .5, vjust = 0, size = 2.5, color = "gray50"
  ) + 
  annotate(
    "text", label = "Downstream", x = 4.72, y = 4.68, 
    angle = 45, hjust = .5, vjust = 1, size = 2.5, color = "gray50"
  ) + 
  geom_point(shape = 16, size = 4, alpha = .8) + 
  labs(title = display[2]) + 
  scale_color_manual(values = c("#007DB7", "#e9532b")) + 
  scale_x_continuous(limits = c(3.75, 5)) +
  scale_y_continuous(limits = c(3.75, 5)) +
  guides(color = guide_legend(
    title = NULL, title.position = "right", nrow = 1, 
    override.aes = list(size = 3, alpha = 1))
  ) + 
  theme(
    plot.margin = margin(14, 2, 2, 5),
    plot.title = element_text(size = 9, hjust = .5, face = "bold"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8),
    legend.box.background = element_rect(
      fill = "white", color = "gray75", linewidth = .25
    ),
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 8, color = "gray50", margin = margin(0, 5, 0, 3)),
    legend.key = element_blank(),
    legend.key.size = unit(.65, "lines"),
    legend.background = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(.05, .85),
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  # PRC
  geom_segment(
    x = points(df, display[2], select2[1], -.004)[1], 
    y = points(df, display[2], select2[1], -.028)[2], 
    xend = points(df, display[2], select2[1], .004)[3], 
    yend = points(df, display[2], select2[1], .028)[4] ,
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[2] & name == select2[1]),
    nudge_x = -.06, nudge_y = 0, hjust = 1, vjust = .5, size = 2.5, color = "black"
  ) +
  
  # Japan
  geom_segment(
    x = points(df, display[2], select2[2], -.015)[1], 
    y = points(df, display[2], select2[2], -.05)[2], 
    xend = points(df, display[2], select2[2], .015)[3], 
    yend = points(df, display[2], select2[2], .05)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, color = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[2] & name == select2[2]),
    nudge_x = -.06, nudge_y = 0, hjust = 1, vjust = .5, size = 2.5, color = "black"
  ) +
  
  # United States
  geom_segment(
    x = points(df, display[2], select2[3], .03)[1], 
    y = points(df, display[2], select2[3], -.01)[2], 
    xend = points(df, display[2], select2[3], -.03)[3], 
    yend = points(df, display[2], select2[3], .01)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[2] & name == select2[3]),
    nudge_x = -.06, nudge_y = .06, hjust = .5, vjust = 0, size = 2.5, color = "black"
  ) +
  
  # Germany
  geom_segment(
    x = points(df, display[2], select2[4], -.03)[1], 
    y = points(df, display[2], select2[4], -.022)[2], 
    xend = points(df, display[2], select2[4], .026)[3], 
    yend = points(df, display[2], select2[4], .022)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, color = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[2] & name == select2[4]),
    nudge_x = .04, nudge_y = -.04, hjust = 0, vjust = 1, size = 2.5, color = "black"
  ) +
  
  # Kazakhstan
  geom_segment(
    x = points(df, display[2], select2[5], -.045)[1], 
    y = points(df, display[2], select2[5], -.015)[2], 
    xend = points(df, display[2], select2[5], .05)[3], 
    yend = points(df, display[2], select2[5], .015)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, color = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  geom_text(
    aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    subset(df, t == years[1] & sector == display[2] & name == select2[5]),
    nudge_x = -.02, nudge_y = -.06, hjust = 0, vjust = 1, size = 2.5, color = "black", fontface = "bold",
  )

# Consolidate

xlab <- ggdraw() + draw_label("Forward GVC length", angle = 90, size = 9)
ylab <- ggdraw() + draw_label("Backward GVC length", size = 9) + theme(plot.margin = margin(b = 10))

plot <- plot_grid(xlab, plot1, plot2, nrow = 1, rel_widths = c(.075, 1, 1))
plot <- plot_grid(plot, ylab, ncol = 1, rel_heights = c(1, .15))

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 9, unit = "cm"
)

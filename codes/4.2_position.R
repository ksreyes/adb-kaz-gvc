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
  distinct(ind, name_short)

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
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
    s %in% subset(countries, name %in% select1)$mrio & 
      i == subset(sectors, name_short == display[1])$ind
  ) |> 
  bind_rows(
    lengths |> 
      filter(
        s %in% subset(countries, name %in% select2)$mrio & 
          i == subset(sectors, name_short == display[2])$ind
  )) |> 
  left_join(countries, by = c("s" = "mrio")) |> 
  left_join(sectors, by = c("i" = "ind")) |> 
  select(sector = name_short, name, t, PLv_GVC, PLy_GVC) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> read_csv()

# Helper functions

points <- function(df, display, country, nudge) {
  df <- subset(df, sector == display & name == country)
  x <- df$PLy_GVC[1] + nudge
  y <- df$PLv_GVC[1] + nudge
  xend <- df$PLy_GVC[2] + nudge
  yend <- df$PLv_GVC[2] + nudge
  return(c(x, y, xend, yend))
}

draw_arrow <- function(display, country, x_nudge, y_nudge, xend_nudge, yend_nudge) {
  geom_segment(
    x = points(df, display, country, x_nudge)[1], 
    y = points(df, display, country, y_nudge)[2], 
    xend = points(df, display, country, xend_nudge)[3], 
    yend = points(df, display, country, yend_nudge)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  )
}

add_label <- function(
    year, display, country, nudge_x, nudge_y, 
    hjust = .5, vjust = .5, size = 2.5, color = "black", face = "plain"
  ) {
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df, t == year & sector == display & name == country),
    nudge_x = nudge_x, nudge_y = nudge_y, hjust = hjust, vjust = vjust, 
    size = size, color = color, fontface = face
  )
}

# Display sector 1

plot1 <- filter(df, sector == display[1]) |> 
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
    plot.title = element_text(size = 9, hjust = .5, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(14, 2, 2, 5)
  ) +
  
  # Russian Federation
  draw_arrow(display[1], select1[1], .01, .055, -.01, -.055) +
  add_label(years[1], display[1], select1[1], .16, -.06, 1, 1) +
  
  # Brunei Darussalam
  draw_arrow(display[1], select1[2], .05, -.015, -.05, .015) +
  add_label(years[1], display[1], select1[2], .04, -.04, 1, 1) +

  # Norway
  draw_arrow(display[1], select1[3], -.02, .02, .02, -.02) +
  add_label(years[1], display[1], select1[3], .06, 0, 0, .5) +
  
  # United States
  geom_curve(
    x = points(df, display[1], select1[4], .035)[1], 
    y = points(df, display[1], select1[4], .045)[2], 
    xend = points(df, display[1], select1[4], -.01)[3], 
    yend = points(df, display[1], select1[4], .05)[4],
    lineend = "butt", linewidth = .5, curvature = -.5, colour = "black",
    arrow = arrow(length = unit(.1, "cm"), type = "closed")
  ) + 
  add_label(years[2], display[1], select1[4], .04, .04, 0, 0) +
  
  # Kazakhstan
  draw_arrow(display[1], select1[5], .05, -.005, -.05, .005) +
  add_label(years[1], display[1], select1[5], .1, -.06, 1, 1, face = "bold")
  
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
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    legend.box.background = element_rect(
      fill = "white", color = "gray75", linewidth = .25
    ),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.65, "lines"),
    legend.text = element_text(size = 8, color = "gray50", margin = margin(0, 5, 0, 3)),
    legend.title = element_text(size = 0),
    legend.position = c(.05, .85),
    legend.justification = c(0, 0),
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 9, hjust = .5, face = "bold"),
    plot.margin = margin(14, 2, 2, 5)
  ) +
  
  # PRC
  draw_arrow(display[2], select2[1], -.004, -.028, .004, .028) +
  add_label(years[1], display[2], select2[1], -.06, 0, 1, .5) +
  
  # Japan
  draw_arrow(display[2], select2[2], -.015, -.05, .015, .05) +
  add_label(years[1], display[2], select2[2], -.06, 0, 1, .5) +
  
  # United States
  draw_arrow(display[2], select2[3], .03, -.01, -.03, .01) +
  add_label(years[1], display[2], select2[3], -.06, .06, .5, 0) +
  
  # Germany
  draw_arrow(display[2], select2[4], -.03, -.022, .026, .022) +
  add_label(years[1], display[2], select2[4], .04, -.04, 0, 1) +
  
  # Kazakhstan
  draw_arrow(display[2], select2[5], -.045, -.015, .05, .015) +
  add_label(years[1], display[2], select2[5], -.02, -.06, 0, 1, face = "bold")
  
# Consolidate

xlab <- ggdraw() + draw_label("Forward GVC length", angle = 90, size = 9)
ylab <- ggdraw() + draw_label("Backward GVC length", size = 9) + theme(plot.margin = margin(b = 10))

plot <- plot_grid(xlab, plot1, plot2, nrow = 1, rel_widths = c(.075, 1, 1))
plot <- plot_grid(plot, ylab, ncol = 1, rel_heights = c(1, .15))

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 9, unit = "cm"
)

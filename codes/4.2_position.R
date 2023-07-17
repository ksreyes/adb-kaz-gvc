# POSITION

# SET UP ----

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

sectors <- here("..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel() %>% 
  group_by(ind, name_short) %>%
  distinct(ind) %>%
  ungroup() %>% 
  bind_rows(tibble(ind = 0, name_short = "Aggregate"))

focusnames <- c("Mining", "Metals")
focus <- sectors %>% filter(name_short %in% focusnames) %>% pull(ind)

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio))) %>% 
  mutate(s = mrio) %>% 
  arrange(s) %>%
  select(s, name) %>% 
  mutate(name = replace(name, name == "People's Republic of China", "PRC"))

years <- c(2010, 2022)

# Check which countries are significant in chosen sectors

exports_A <- here("..", "mrio-processing", "data", "trade-accounting", "ta-es.parquet") %>% 
  read_parquet() %>% 
  filter(t == years[2] & i == focus[1]) %>% 
  group_by(s) %>% 
  summarize(Exports = sum(Exports)) %>%
  arrange(-Exports) %>% 
  left_join(countries) %>% 
  select(s, name, Exports)

exports_B <- here("..", "mrio-processing", "data", "trade-accounting", "ta-es.parquet") %>% 
  read_parquet() %>% 
  filter(t == years[2] & i == focus[2]) %>% 
  group_by(s) %>% 
  summarize(Exports = sum(Exports)) %>%
  arrange(-Exports) %>% 
  left_join(countries) %>% 
  select(s, name, Exports)

select_A <- c("Russian Federation", "Brunei Darussalam", "Norway", "United States", "Kazakhstan")
select_B <- c("PRC", "Japan", "United States", "Germany", "Kazakhstan")

s_A <- countries %>% filter(name %in% select_A) %>% pull(s)
s_B <- countries %>% filter(name %in% select_B) %>% pull(s)


# DATA ----

df_A_t2 <- here("..", "mrio-processing", "data", "lengths.parquet") %>% 
  read_parquet() %>% 
  filter(t == years[2])

df_A_t1 <- here("..", "mrio-processing", "data", "lengths62.parquet") %>% 
  read_parquet() %>% 
  filter(t == years[1])

df_A <- bind_rows(df_A_t2, df_A_t1) %>% 
  filter(s %in% s_A & agg == 35 & i == focus[1]) %>% 
  left_join(countries) %>% 
  mutate(t = factor(t, levels = years)) %>% 
  select(i, s, name, t, PLv_GVC, PLy_GVC)

df_B_t2 <- here("..", "mrio-processing", "data", "lengths.parquet") %>% 
  read_parquet() %>% 
  filter(t == years[2])

df_B_t1 <- here("..", "mrio-processing", "data", "lengths62.parquet") %>% 
  read_parquet() %>% 
  filter(t == years[1])

df_B <- bind_rows(df_B_t2, df_B_t1) %>% 
  filter(s %in% s_B & agg == 35 & i == focus[2]) %>% 
  left_join(countries) %>% 
  mutate(t = factor(t, levels = years)) %>% 
  select(i, s, name, t, PLv_GVC, PLy_GVC)


# PLOT ----

points <- function(df, country, u) {
  df <- subset(df, s == country)
  x <- df$PLy_GVC[2] + u
  y <- df$PLv_GVC[2] + u
  xend <- df$PLy_GVC[1] + u
  yend <- df$PLv_GVC[1] + u
  return(c(x, xend, y, yend))
}

# Sector A ----

plot_A <- ggplot(df_A, aes(x = PLy_GVC, y = PLv_GVC, color = t)) + 
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
  
  # Style
  labs(title = focusnames[1]) + 
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

  # Norway
  geom_segment(
    x = points(df_A, 33, -.02)[1], xend = points(df_A, 33, .02)[2], 
    y = points(df_A, 33, .02)[3], yend = points(df_A, 33, -.02)[4] ,
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_A, t == years[1] & s == 33),
    nudge_x = .06, nudge_y = 0, hjust = 0, vjust = .5, size = 2.5, color = "black"
  ) +
  
  # Russian Federation
  geom_segment(
    x = points(df_A, 37, .01)[1], xend = points(df_A, 37, -.01)[2], 
    y = points(df_A, 37, .055)[3], yend = points(df_A, 37, -.055)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_A, t == years[1] & s == 37),
    nudge_x = .16, nudge_y = -.06, hjust = 1, vjust = 1, size = 2.5, color = "black"
  ) +
  
  # United States
  geom_curve(
    x = points(df_A, 43, .035)[1], xend = points(df_A, 43, -.01)[2], 
    y = points(df_A, 43, .045)[3], yend = points(df_A, 43, .05)[4],
    lineend = "butt", linewidth = .5, curvature = -.5, colour = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_A, t == years[2] & s == 43),
    nudge_x = .04, nudge_y = .04, hjust = 0, vjust = 0, size = 2.5, color = "black"
  ) +
  
  # Brunei Darussalam
  geom_segment(
    x = points(df_A, 55, .05)[1], xend = points(df_A, 55, -.05)[2], 
    y = points(df_A, 55, -.015)[3], yend = points(df_A, 55, .015)[4],
    lineend = "butt", linewidth = .5, linejoin = "mitre", colour = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_A, t == years[1] & s == 55), size = 2.5, color = "black",
    nudge_x = .04, nudge_y = -.04, hjust = 1, vjust = 1
  ) +
  
  # Kazakhstan
  geom_segment(
    x = points(df_A, 49, .05)[1], xend = points(df_A, 49, -.05)[2], 
    y = points(df_A, 49, -.005)[3], yend = points(df_A, 49, .005)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, color = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_A, t == years[1] & s == 49),
    nudge_x = .1, nudge_y = -.06, hjust = 1, vjust = 1, size = 2.5, color = "black", fontface = "bold",
  )
  
# Sector B ----

plot_B <- ggplot(df_B, aes(x = PLy_GVC, y = PLv_GVC, color = t)) + 
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
  
  # Style
  labs(title = focusnames[2]) + 
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
    x = points(df_B, s_B[1], -.004)[1], xend = points(df_B, s_B[1], .004)[2], 
    y = points(df_B, s_B[1], -.028)[3], yend = points(df_B, s_B[1], .028)[4] ,
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_B, t == years[1] & s == s_B[1]),
    nudge_x = -.06, nudge_y = 0, hjust = 1, vjust = .5, size = 2.5, color = "black"
  ) +
  
  # Germany
  geom_segment(
    x = points(df_B, s_B[2], -.03)[1], xend = points(df_B, s_B[2], .026)[2], 
    y = points(df_B, s_B[2], -.022)[3], yend = points(df_B, s_B[2], .022)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, color = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_B, t == years[1] & s == s_B[2]),
    nudge_x = .04, nudge_y = -.04, hjust = 0, vjust = 1, size = 2.5, color = "black"
  ) +
  
  # Japan
  geom_segment(
    x = points(df_B, s_B[3], -.015)[1], xend = points(df_B, s_B[3], .015)[2], 
    y = points(df_B, s_B[3], -.05)[3], yend = points(df_B, s_B[3], .05)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, color = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_B, t == years[1] & s == s_B[3]),
    nudge_x = -.06, nudge_y = 0, hjust = 1, vjust = .5, size = 2.5, color = "black"
  ) +
  
  # United States
  geom_segment(
    x = points(df_B, s_B[4], .03)[1], xend = points(df_B, s_B[4], -.03)[2], 
    y = points(df_B, s_B[4], -.01)[3], yend = points(df_B, s_B[4], .01)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, colour = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_B, t == years[1] & s == s_B[4]), size = 2.5, color = "black",
    nudge_x = -.06, nudge_y = .06, hjust = .5, vjust = 0
  ) +
  
  # Kazakhstan
  geom_segment(
    x = points(df_B, s_B[5], -.045)[1], xend = points(df_B, s_B[5], .05)[2], 
    y = points(df_B, s_B[5], -.015)[3], yend = points(df_B, s_B[5], .015)[4],
    lineend = "butt", linejoin = "mitre", linewidth = .5, color = "black",
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) + 
  geom_text(
    mapping = aes(x = PLy_GVC, y = PLv_GVC, label = name), 
    data = subset(df_B, t == years[1] & s == s_B[5]),
    nudge_x = -.02, nudge_y = -.06, hjust = 0, vjust = 1, size = 2.5, color = "black", fontface = "bold",
  )

# .... Consolidate ----

xtitle <- ggdraw() + 
  draw_label("Forward GVC length", angle = 90, size = 9)

ytitle <- ggdraw() +
  draw_label("Backward GVC length", size = 9) + 
  theme(plot.margin = margin(0, 0, 10, 0))

plot <- plot_grid(xtitle, plot_A, plot_B, nrow = 1, rel_widths = c(.075, 1, 1))
plot <- plot_grid(plot, ytitle, ncol = 1, rel_heights = c(1, .15))

ggsave(
  here("figures", "4.2_position.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 9, unit = "cm"
)

######### END #########
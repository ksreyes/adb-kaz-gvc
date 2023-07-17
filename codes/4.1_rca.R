# RCA: GROSS EXPORTS VS VALUE-ADDED EXPORTS

# SET UP ----

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

year <- 2022

sectors <- here("..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel() %>% 
  group_by(ind, name_short) %>%
  distinct(ind) %>%
  ungroup() 

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio)))

select <- countries$mrio[which(countries$name == "Kazakhstan")]

# DATA ----

df <- here("..", "mrio-processing", "data", "rca.parquet") %>% 
  read_parquet() %>% 
  filter(t == year & agg == 35 & s == select) %>% 
  mutate(
    exports_sh = exports_si / exports_s,
    exports_wsh = exports_i / exports_all,
    vaxos_sh = vaxos_si / vaxos_s,
    vaxos_wsh = vaxos_i / vaxos_all,
    rca_plot = ifelse(exports_sh > exports_wsh, rca - 1, (-exports_wsh / exports_sh) + 1),
    rca_vaxos_plot = ifelse(vaxos_sh > vaxos_wsh, rca_vaxos - 1, (-vaxos_wsh / vaxos_sh) + 1)
  ) %>% 
  left_join(sectors, by = c("i" = "ind")) %>% 
  select(
    t, s, i, name_short, 
    exports_si, exports_sh, exports_wsh, rca, rca_plot,
    vaxos_si, vaxos_sh, vaxos_wsh, rca_vaxos, rca_vaxos_plot
  )

# Export dataset

dfout <- df %>% 
  select(
    t, s, i, name_short, exports_si, exports_sh, exports_wsh, rca,
    vaxos_si, vaxos_sh, vaxos_wsh, rca_vaxos
  )

write_csv(dfout, here("data", "final", "rca.csv"))

# PLOT ----

df <- df %>% filter(i != 35)

# .... Middle plot: Ranking changes ----

rank <- df %>% 
  arrange(rca_vaxos_plot) %>% 
  mutate(vaxos = 1:nrow(df)) %>% 
  arrange(rca_plot) %>% 
  mutate(exports = 1:nrow(df)) %>% 
  select(i, exports, vaxos) %>% 
  pivot_longer(cols = exports:vaxos, names_to = "type", values_to = "rank")

plot_rank <- ggplot(rank, aes(x = type, y = rank, group = i)) + 
  geom_rect(
    xmin = "exports", xmax = "vaxos", ymin = 1 - .1, ymax = 34 + .1,
    fill = "gray90", color = "gray90", size = 2
  ) + 
  geom_line(linewidth = .35) + 
  scale_y_continuous(limits = c(0, 34.1)) + 
  theme(
    plot.margin = margin(2, -30, 13, -30),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )

# .... Right plot: RCA by gross exports ----

df_exports <- df %>% 
  arrange(rca_plot) %>% 
  mutate(
    rank = 1:nrow(df),
    arrow = ifelse(abs(rca_plot) > 17, -2, NA),
    label = ifelse(is.na(arrow), NA, paste0("1/", round(-rca_plot) + 1)),
    label = ifelse(abs(round(rca_plot)) + 1 > 100, "< 1/100", label),
    rca_plot = ifelse(is.na(arrow), rca_plot, NA)
  ) %>% 
  select(i, name_short, rank, rca_plot, arrow, label)

plot_exports <- ggplot(df_exports, aes(x = rca_plot, y = rank)) + 
  geom_segment(
    mapping = aes(x = 0, xend = rca_plot, y = rank, yend = rank),
    linewidth = .75, 
    color = ifelse(df_exports$rca_plot > 0, "#007db7", "#63CCEC")
  ) + 
  geom_point(
    shape = 16, size = 2.5,
    color = ifelse(df_exports$rca_plot > 0, "#007db7", "#63CCEC")
  ) +
  
  # Off-the-chart sectors
  geom_segment(
    mapping = aes(x = arrow, xend = arrow, y = rank, yend = rank),
    lineend = "butt", linejoin = "mitre", 
    arrow = arrow(length = unit(.1, "cm"), type = "closed"),
    color = ifelse(df_exports$arrow > 0, "#007db7", "#63CCEC")
  ) + 
  geom_text(
    mapping = aes(x = arrow, y = rank, label = label),
    hjust = 1, nudge_x = -1, size = 1.75, color = "gray25"
  ) + 
  
  # Style
  scale_x_continuous(breaks = c(-9, 0, 10), labels = c("1/10", "1", "10")) + 
  scale_y_continuous(
    sec.axis = sec_axis(~., breaks = 1:nrow(df), labels = df_exports$name_short)
  ) + 
  theme(
    plot.margin = margin(2, 2, 13, 2),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8, hjust = .5),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(size = 7, hjust = 1, margin = margin(0, 0, 0, -5)),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(color = "gray75", linewidth = .25, linetype = "dashed"),
    panel.grid.major.y = element_blank()
  )

# .... Left plot: RCA by value-added exports ----

df_vaxos <- df %>% 
  arrange(rca_vaxos_plot) %>% 
  mutate(
    rank = 1:nrow(df),
    arrow = ifelse(abs(rca_vaxos_plot) > 17, -1.5, NA),
    label = ifelse(is.na(arrow), NA, paste0("1/", round(-rca_vaxos_plot) + 1)),
    label = ifelse(abs(round(rca_vaxos_plot)) + 1 > 100, "< 1/100", label),
    rca_vaxos_plot = ifelse(is.na(arrow), rca_vaxos_plot, NA)
  ) %>% 
  select(i, name_short, rank, rca_vaxos_plot, arrow, label)

plot_vaxos <- ggplot(df_vaxos, aes(x = rca_vaxos_plot, y = rank)) + 
  geom_segment(
    mapping = aes(x = 0, xend = rca_vaxos_plot, y = rank, yend = rank),
    linewidth = .75, 
    color = ifelse(df_vaxos$rca_vaxos_plot > 0, "#007db7", "#63CCEC")
  ) + 
  geom_point(
    size = 2.5, shape = 16,
    color = ifelse(df_vaxos$rca_vaxos_plot > 0, "#007db7", "#63CCEC")
  ) +
  
  # Off-the-chart sectors
  geom_segment(
    mapping = aes(x = arrow, xend = arrow, y = rank, yend = rank),
    lineend = "butt", linejoin = "mitre", 
    arrow = arrow(length = unit(.1, "cm"), type = "closed"),
    color = ifelse(df_vaxos$arrow > 0, "#007db7", "#63CCEC")
  ) + 
  geom_text(
    mapping = aes(x = arrow, y = rank, label = label),
    hjust = 1, nudge_x = -.8, size = 1.75, color = "gray25"
  ) + 
  
  # Style
  scale_x_continuous(breaks = c(-9, 0, 9), labels = c("1/10", "1", "10")) + 
  scale_y_continuous(breaks = 1:nrow(df), labels = df_vaxos$name_short) + 
  theme(
    plot.margin = margin(2, 2, 13, 4),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8, hjust = .5),
    axis.text.y = element_text(size = 7, hjust = 0, margin = margin(0, -5, 0, 0)),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(color = "gray75", linewidth = .25, linetype = "dashed"),
    panel.grid.major.y = element_blank()
  )

# .... Consolidate and export ----

title_r <- ggdraw() + 
  draw_label("RCA by Gross Exports", hjust = .5, size = 9, fontface = "bold") + 
  theme(plot.margin = margin(10, 2, 2, 2))
title_m <- ggdraw() + draw_label("M", hjust = .5, size = 9, color = NA) + 
  theme(plot.margin = margin(10, 2, 2, 2))
title_l <- ggdraw() + 
  draw_label("RCA by Value Added Exports", hjust = .5, size = 9, fontface = "bold") + 
  theme(plot.margin = margin(10, 2, 2, 2))

titles <- plot_grid(
  title_r, title_m, title_l, ncol = 3, rel_widths = c(1, .3, 1)
)
plots <- plot_grid(
  plot_exports, plot_rank, plot_vaxos, ncol = 3, rel_widths = c(1, .3, 1)
)
plot <- plot_grid(titles, plots, ncol = 1, rel_heights = c(.1, 1))

ggsave(
  here("figures", "4.1_rca.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 13, unit = "cm"
)

######### END #########
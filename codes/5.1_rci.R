# REGIONAL CONCENTRATION INDEX

# SET UP ----

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

df72 <- here("..", "..", "mrio-processing", "data", "rci.parquet") %>% 
  read_parquet()

df62 <- here("..", "..", "mrio-processing", "data", "rci62.parquet") %>% 
  read_parquet() %>% 
  filter(t < 2017)

df <- bind_rows(df62, df72) %>% 
  select(t, rta, method, rci) %>% 
  pivot_wider(id_cols = c(t, rta), names_from = method, values_from = rci)

q <- length(unique(df$rta))

# Add 2001-2002 dummy

step <- function(vec) {
  tab <- cbind(vec[1:q], vec[q + 1:q])
  dist <- (tab[, 2] - tab[, 1]) / 3
  return(c(tab[, 1] + dist, tab[, 1] + 2 * dist))
}

df_plot <- df %>% 
  bind_rows(
    tibble(
      t = rep(2001:2002, each = q) %>% as.character(),
      rta = rep(df %>% pull(rta) %>% unique(), 2),
      `gross exports` = step(df %>% pull(`gross exports`)),
      `end-to-end` = step(df %>% pull(`end-to-end`)),
    )) %>% 
  mutate(t = factor(t, levels = c(2000:2002, 2007:2022)))

# Plot elements

line_dashed <- geom_line(
    data = subset(df_plot, t %in% c(2000:2002, 2007)), 
    size = .75, linetype = "dashed"
  )
line <- geom_line(data = subset(df_plot, t %in% 2007:2022), size = .75)
scale_x <- scale_x_discrete(
    labels = c(2000, "", "", 2007, "", "", 10, "", "", "", "", 15, "", "", "", "", 20, "", "")
  )
scale_color <- scale_color_manual(
    labels = c("ASEAN", "EAEU", "European Union", "NAFTA", "SAARC"),
    values = c("#007DB7", "#8DC63F", "#E9532B", "#FDB415", "black")
  )
theme <- theme(
    plot.title = element_text(size = 9, face = "bold", hjust = .5),
    axis.title = element_blank(),
    axis.ticks.x = element_line(size = .25, color = c("black", NA, NA, rep("black", 15))),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", margin = margin(3, 0, 0, 0)),
    axis.text.y = element_text(size = 8, color = "black"),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

plot1 <- ggplot(
    df_plot, 
    aes(x = t, y = `gross exports`, color = rta, group = rta)
  ) + 
  line + line_dashed + 
  labs(title = "Gross exports") + 
  scale_y_continuous(limits = c(0, 5.5), breaks = c(1, 3, 5)) +
  scale_x + scale_color + theme + 
  theme(plot.margin = margin(15, 4, 2, 2))

plot2 <- ggplot(
    df_plot, 
    aes(x = t, y = `end-to-end`, color = rta, group = rta)
  ) + 
  line + line_dashed + 
  labs(title = "End-to-end") + 
  scale_y_continuous(
    limits = c(0, 5.5), 
    breaks = c(1, 3, 5), sec.axis = sec_axis(~., breaks = c(1, 3, 5))
  ) +
  scale_x + scale_color + theme + 
  theme(
    plot.margin = margin(15, 2, 2, 4),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(size = 8, hjust = .5)
  )

legend <- get_legend(
  plot1 + 
  theme(
    plot.margin = margin(2, 2, 15, 2),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 8, 0, 0)),
    legend.box.margin = margin(-10, 0, 0, 0), 
    legend.position = "bottom"
  ))

plot <- plot_grid(plot1, plot2)
plot <- plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .2))

ggsave(
  here("figures", "5.1_rci.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 8, unit = "cm"
)

######### END #########
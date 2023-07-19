# REER

# SET UP ----

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

sectors <- here("..", "..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel() %>% 
  group_by(ind, name_short) %>%
  distinct(ind) %>%
  ungroup() %>% 
  bind_rows(tibble(ind = 0, name_short = "Aggregate"))

focus <- c("Aggregate", "Mining", "Metals")
ind <- sectors %>% filter(name_short %in% focus) %>% pull(ind)

countries <- here("..", "..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio62))) %>% 
  mutate(s = mrio62) %>% 
  select(s, name)

selectnames <- c("Kazakhstan", "Russian Federation")
select <- countries %>% filter(name %in% selectnames) %>% pull(s)

years <- 2007:2021
G <- 63
N <- 35

# DATA ----

df <- here("..", "..", "mrio-processing", "data", "reer", "reer62.parquet") %>% 
  read_parquet() %>% 
  filter(method %in% c("pww", "pww-sector") & s %in% select & i %in% ind) %>% 
  left_join(sectors, by = c("i" = "ind")) %>% 
  left_join(countries) %>% 
  mutate(
    t = factor(t, levels = years),
    name_short = factor(
      name_short, 
      levels = c("Mining", "Metals", "Aggregate")
    ))

df_out <- df %>% 
  select(s, country = name, i, sector = name_short, t, reer) %>% 
  pivot_wider(names_from = t, values_from = reer) %>% 
  arrange(s, i)
  
write_csv(df_out, here("data/final/4.4_reer.csv"))

# PLOT ----

plot1 <- ggplot(
    subset(df, s == select[1]), 
    aes(x = t, y = reer, color = name_short, group = name_short)
  ) + 
  geom_hline(yintercept = 0, size = .5, color = "gray75") + 
  geom_line(size = 1) + 
  labs(title = selectnames[1]) + 
  scale_color_manual(values = c("#007DB7", "#00A5D2", "black")) + 
  scale_x_discrete(labels = c(2007, "", "09", "", 11, "", 13, "", 15, "", 17, "", 19, "", 21)) + 
  scale_y_continuous(
    limits = c(-.35, .35), labels = function(x) paste0(100 * x, "%")
  ) + 
  theme(
    plot.margin = margin(15, 2, 2, 2),
    plot.title = element_text(size = 9, face = "bold", hjust = .5),
    axis.title = element_blank(),
    axis.ticks.x = element_line(size = .25, color = "black"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", margin = margin(3, 0, 0, 0)),
    axis.text.y = element_text(size = 8, hjust = .5, color = "black"),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

plot2 <- ggplot(
    subset(df, s == select[2]), 
    aes(x = t, y = reer, color = name_short, group = name_short)
  ) + 
  geom_hline(yintercept = 0, size = .5, color = "gray75") + 
  geom_line(size = 1) + 
  labs(title = selectnames[2]) + 
  scale_color_manual(values = c("#007DB7", "#00A5D2", "black")) + 
  scale_x_discrete(labels = c(2007, "", "09", "", 11, "", 13, "", 15, "", 17, "", 19, "", 21)) + 
  scale_y_continuous(
    limits = c(-.35, .35),
    sec.axis = sec_axis(~., labels = function(x) paste0(100 * x, "%"))
  ) + 
  theme(
    plot.margin = margin(15, 2, 2, 2),
    plot.title = element_text(size = 9, face = "bold", hjust = .5),
    axis.title = element_blank(),
    axis.ticks.x = element_line(size = .25, color = "black"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", margin = margin(3, 0, 0, 0)),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(size = 8, hjust = .5, color = "black"),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

legend <- get_legend(
  plot1 + 
  theme(
    plot.margin = margin(2, 2, 15, 2),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 5, 0, 0)),
    legend.position = "bottom"
  ))

plot <- plot_grid(plot1, plot2)
plot <- plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .2))

ggsave(
  here("figures", "4.4_reer.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 7.5, unit = "cm"
)

######### END #########
# REER VOLATILITY

# SET UP ----

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)

countries <- here("..", "..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio62))) %>% 
  mutate(s = mrio62) %>% 
  select(s, name)

# DATA ----

df <- here("..", "..", "mrio-processing", "data", "reer", "reer62.parquet") %>% 
  read_parquet() %>% 
  filter(method == "pww" & s!= 63) %>% 
  group_by(s) %>% 
  summarize(mean = mean(reer), sd = sd(reer)) %>% 
  ungroup() %>% 
  left_join(countries) %>% 
  select(s, name, mean, sd)

write_csv(df, here("data", "final", "4.5_reerv.csv"))

# PLOT ----

hl1 <- c("Mongolia", "Kyrgyz Republic", "Turkey")
hl2 <- c("People's Republic of China", "Italy", "Brunei Darussalam", "United States")
hl3 <- c("Singapore", "Germany")
hl4 <- "Russian Federation"

plot <- ggplot(df, aes(x = 100 * sd, y = 100 * mean, label = name)) + 
  geom_point(data = subset(df, !(name %in% c(hl1, hl2))), 
             shape = 16, color = "gray75", size = 3, alpha = .7) + 
  
  # Highlighted countries
  geom_point(
    data = subset(df, name %in% c(hl1, hl2, hl3, hl4)), 
    shape = 16, color = "#007db7", size = 4, alpha = .8
  ) + 
  geom_text(
    data = subset(df, name %in% hl1), 
    nudge_y = .35, vjust = 0, size = 3
  ) + 
  geom_text(
    data = subset(df, name %in% hl2), 
    nudge_y = -.35, vjust = 1, size = 3
  ) + 
  geom_text(
    data = subset(df, name %in% hl3), 
    nudge_x = .2, nudge_y = .35, vjust = 0, size = 3
  ) + 
  geom_text(
    data = subset(df, name %in% hl4), 
    nudge_x = -.3, hjust = 1, vjust = .5, size = 3
  ) + 
  
  # Kazakhstan
  geom_point(
    data = subset(df, name == "Kazakhstan"), 
    shape = 21, fill = "#007db7", color = "black", size = 5
  ) + 
  geom_text(
    data = subset(df, name == "Kazakhstan"), 
    nudge_y = -.4, vjust = 1, size = 4, fontface = "bold"
  ) + 
  
  # Style
  scale_x_continuous(name = "Standard deviation") +
  scale_y_continuous(
    name = "Mean appreciation",
    label = function(x) paste0(x, "%")
  ) +
  theme(
    plot.margin = margin(15, 7, 15, 2),
    axis.title.x = element_text(size = 9, margin = margin(5, 0, 0, 0)),
    axis.title.y = element_text(size = 9, margin = margin(0, 5, 0, 0)),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray20", linewidth = .5),
    panel.grid.major = element_blank()
  )

ggsave(
  here("figures", "4.5_reerv.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 9, unit = "cm"
)

######### END #########
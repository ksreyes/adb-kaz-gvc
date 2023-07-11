# GVC PARTICIPATION

# PRELIMINARIES ----

rm(list = ls())
library(here)
library(readxl)
library(tidyverse)
library(ggplot2)

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio)))

select <- countries$mrio[which(countries$name == "Kazakhstan")]

# DATA ----

df62 <- here("..", "mrio-processing", "data", "final", "gvcp62.csv") %>% 
  read_csv() %>% 
  filter(s == select & agg == 0 & t < 2017) %>% 
  select(t, GVCP_trade_f, GVCP_trade_b, GVCP_prod)

df72 <- here("..", "mrio-processing", "data", "final", "gvcp.csv") %>% 
  read_csv() %>% 
  filter(s == select & agg == 0) %>% 
  select(t, GVCP_trade_f, GVCP_trade_b, GVCP_prod)

df <- df62 %>% 
  bind_rows(df72)

write_csv(df, here("data", "final", "3.3_participation.csv"))

# PLOT ----

# Add 2001-2002 dummy

step <- function(vec) {
  dist <- (vec[2] - vec[1]) / 3
  return(c(vec[1] + dist, vec[1] + 2 * dist))
}

df_plot <- df %>%
  bind_rows(tibble(
    t = 2001:2002,
    GVCP_trade_f = step(df$GVCP_trade_f),
    GVCP_trade_b = step(df$GVCP_trade_b),
    GVCP_prod = step(df$GVCP_prod)
  )) %>%
  arrange(t) %>%
  pivot_longer(cols = GVCP_trade_f:GVCP_prod,
               names_to = "approach",
               values_to = "rate") %>%
  mutate(
    t = factor(t, levels = c(2000:2002, 2007:2022)),
    approach = factor(
      approach,
      levels = c("GVCP_trade_f", "GVCP_trade_b", "GVCP_prod")
    ))

plot <- ggplot(df_plot, aes(x = t, y = rate, color = approach, group = approach)) + 
  geom_line(
    data = subset(df_plot, t %in% c(2000:2002, 2007)),
    linetype = "dashed", linewidth = 1.25
  ) +
  geom_line(data = subset(df_plot, t %in% 2007:2022), size = 1.25) + 
  
  geom_segment(
    x = 14, xend = 14, y = 0.3647946, yend = 0.3647946 + .011, linewidth = .25, 
    color = "black"
  ) + 
  geom_point(x = 14, y = 0.3647946, size = .15, color = "black") +
  annotate(
    "text", x = 14, y = 0.3647946 + .02, vjust = 0, hjust = .5, size = 3.5, 
    label = "Trade-based, forward"
  ) + 
  
  geom_segment(
    x = 9, xend = 9, y = 0.12346576, yend = 0.12346576 + .011, linewidth = .25, 
    color = "black"
  ) + 
  geom_point(x = 9, y = 0.12346576, size = .02, color = "black") +
  annotate(
    "text", x = 9, y = 0.12346576 + .02, vjust = 0, hjust = .5, size = 3.5, 
    label = "Trade-based, backward"
  ) + 
  
  geom_segment(
    x = 12, xend = 12, y = 0.2453017, yend = 0.2453017 - .011, linewidth = .25, 
    color = "black"
  ) + 
  geom_point(x = 12, y = 0.2453017, size = .15, color = "black") +
  annotate(
    "text", x = 12, y = 0.2453017 - .02, vjust = 1, hjust = .5, size = 3.5, 
    label = "Production-based"
  ) + 
  
  scale_x_discrete(
    labels = c(2000, "", "", 2007, "", "", 10, "", "", "", "", 15, "", "", "", "", 20, "", "")
  ) + 
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  scale_color_manual(
    labels = c("Trade-based, forward", "Trade-based, backward", "Production-based"),
    values = c("#007db7", "#63CCEC", "#e9532b")
  ) + 
  
  theme(
    plot.margin = margin(15, 2, 10, 2),
    axis.title = element_blank(),
    axis.line = element_line(linewidth = .25, color = "black"),
    axis.ticks.x = element_line(
      linewidth = .25, 
      color = c("black", NA, NA, rep("black", 15))
    ),
    axis.ticks.y = element_line(linewidth = .25, color = "black"),
    axis.text.x = element_text(
      size = 9, color = "black", margin = margin(5, 0, 0, 0)
    ),
    axis.text.y = element_text(size = 9, color = "black"),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  here("figures", "3.3_participation.pdf"),
  plot,
  device = cairo_pdf,
  width = 16,
  height = 9,
  unit = "cm"
)

######### END #########
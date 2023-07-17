# GVC PARTICIPATION

# PRELIMINARIES ----

rm(list = ls())
library(here)
library(readxl)
library(tidyverse)
library(cowplot)

year <- 2022

party <- c(
  "Kazakhstan", "Russian Federation", "Uzberkistan", "Kyrgyz Republic",
  "Saudi Arabia", "Brunei Darussalam", "Turkey", "Mongolia", "Georgia"
)

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(name %in% party) %>%
  mutate(s = mrio) %>% 
  bind_rows(tibble(s = 0, name = "World")) %>% 
  arrange(s) %>%
  select(s, name)

# DATA ----

gvcp_world <- here("..", "mrio-processing", "data", "gvcp.csv") %>% 
  read_csv() %>% 
  filter(t == year & agg == 0) %>% 
  group_by(t) %>% 
  summarize(across(Exports:GVC_prod, sum)) %>%
  mutate(
    s = 0,
    name = "World",
    GVCP_trade = GVC_trade / Exports,
    GVCP_prod = GVC_prod / va
  ) %>% 
  select(s, name, t, GVCP_trade, GVCP_prod)

gvcp62_world <- here("..", "mrio-processing", "data", "gvcp62.csv") %>% 
  read_csv() %>% 
  filter(t %in% c(2000, 2010) & agg == 0) %>% 
  group_by(t) %>% 
  summarize(across(Exports:GVC_prod, sum)) %>%
  mutate(
    s = 0,
    name = "World",
    GVCP_trade = GVC_trade / Exports,
    GVCP_prod = GVC_prod / va
  ) %>% 
  select(s, name, t, GVCP_trade, GVCP_prod)

gvcp <- here("..", "mrio-processing", "data", "gvcp.csv") %>% 
  read_csv() %>% 
  filter(t == year & agg == 0 & s %in% (countries %>% pull(s))) %>% 
  left_join(countries) %>% 
  select(s, name, t, GVCP_trade, GVCP_prod) 

gvcp62 <- here("..", "mrio-processing", "data", "gvcp62.csv") %>% 
  read_csv() %>% 
  filter(t %in% c(2000, 2010) & agg == 0 & s %in% (countries %>% pull(s))) %>% 
  left_join(countries) %>% 
  select(s, name, t, GVCP_trade, GVCP_prod)

df <- bind_rows(gvcp62_world, gvcp62, gvcp_world, gvcp) %>% 
  arrange(s) %>% 
  pivot_wider(
    names_from = t,
    values_from = c(GVCP_trade, GVCP_prod)
  )

# Trade-based

df_trade1 <- df %>% 
  select(s, name, starts_with("GVCP")) %>% 
  arrange(GVCP_trade_2022)

df_trade <- df_trade1 %>% 
  pivot_longer(
    cols = starts_with("GVCP"),
    names_to = c(".value", "t"),
    names_pattern = "(.+)_(.+)"
  ) %>% 
  mutate(name = factor(name, levels = df_trade1$name))

# Production-based

df_prod1 <- df %>% 
  select(s, name, starts_with("GVCP")) %>% 
  arrange(GVCP_prod_2022)

df_prod <- df_prod1 %>% 
  pivot_longer(
    cols = GVCP_prod_2000:GVCP_prod_2022,
    names_to = c(".value", "t"),
    names_pattern = "(.+)_(.+)"
  ) %>% 
  mutate(name = factor(name, levels = df_prod1$name))

dfout <- df_trade1 %>% 
  left_join(df_prod1)

write_csv(dfout, here("data", "final", "3.4_partcomp.csv"))

# PLOT ----

# Highlight World and Pakistan

bold_trade <- bold_prod <- rep("plain", nrow(countries))
bold_trade[c(6, 3)] = bold_prod[c(2, 6)] = "bold"

color_trade <- color_prod <- rep("black", nrow(countries))
color_trade[6] <- color_prod[2] <- "#007db7"

size_trade <- size_prod <- rep(8, nrow(countries))
size_trade[3] <- size_prod[6] <- 9

hltheme_trade <- theme(
  axis.text.y = element_text(
    face = bold_trade, 
    color = color_trade,
    size = size_trade
  ))

hltheme_prod <- theme(
  axis.text.y = element_text(
    face = bold_prod,
    color = color_prod,
    size = size_prod
  ))

# Plots

plot1 <- ggplot(df_trade, aes(x = 100 * GVCP_trade, y = name,  group = name)) + 
  geom_hline(yintercept = "World", color = "gray50", size = 5, alpha = .2) + 
  geom_line(color = "#63CCEC", size = 1.5) + 
  geom_point(aes(color = t), size = 2.5) + 
  scale_x_continuous(breaks = c(40, 50),
                     labels = function(x) paste0(x, "%")) + 
  scale_color_manual("Year", values = c("#007db7", "#00A5D2", "#E9532B")) + 
  labs(title = "Trade-based") + 
  theme(
    plot.margin = margin(10, 6, 13, 2),
    plot.title = element_text(hjust = .5, size = 9, face = "bold", margin = margin(5, 0, 5, 0)),
    axis.title = element_blank(),
    axis.text = element_text(size = 9),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray20", size = .5),
    panel.grid.major.x = element_line(color = "gray75", size = .25, linetype = "dashed"),
    panel.grid.major.y = element_blank()
  ) + 
  hltheme_trade

plot2 <- ggplot(df_prod, aes(x = 100 * GVCP_prod, y = name,  group = name)) + 
  geom_hline(yintercept = "World", color = "gray50", size = 5, alpha = .2) + 
  geom_line(color = "#63CCEC", size = 1.5) + 
  geom_point(aes(color = t), size = 2.5) + 
  scale_x_continuous(
    breaks = c(20, 40, 60),
    labels = function(x) paste0(x, "%")
  ) + 
  scale_color_manual("Year", values = c("#007db7", "#00A5D2", "#E9532B")) + 
  guides(color = guide_legend(title = NULL, title.position = "right")) +
  labs(title = "Production-based") + 
  theme(
    plot.margin = margin(10, 2, 13, 6),
    plot.title = element_text(hjust = .5, size = 9, face = "bold", margin = margin(5, 0, 5, 0)),
    axis.title = element_blank(),
    axis.text = element_text(size = 9),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_rect(
      fill = "white", color = "gray75", linewidth = .25
    ),
    legend.spacing = unit(0, "lines"),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9),
    legend.position = c(.78, .34),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray20", size = .5),
    panel.grid.major.x = element_line(color = "gray75", size = .25, linetype = "dashed"),
    panel.grid.major.y = element_blank()
  ) + 
  hltheme_prod

plot <- plot_grid(plot1, plot2, ncol = 2, align = "h")

ggsave(
  here("figures", "3.4_partcomp.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 6.5, unit = "cm"
)

######### END #########
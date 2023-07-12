# TRADE ANATOMY

# PRELIMINARIES ----

rm(list = ls())
library(here)
library(arrow)
library(readxl)
library(tidyverse)
library(cowplot)

show <- 10
# year <- 2022
years <- 2021:2022

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio)))

select <- countries$mrio[which(countries$name == "Kazakhstan")]

df <- here("..", "mrio-processing", "data", "interim", "trade-accounting", "ta.parquet") %>% 
  read_parquet() %>% 
  filter(s == select & t %in% years) %>% 
  left_join(countries, by = c("r" = "mrio"))

exports22 <- df %>% 
  filter(t == years[2]) %>% 
  mutate(share = Exports / sum(Exports)) %>% 
  select(s, r, code, share_2022 = share)

exports <- df %>% 
  filter(t == years[1]) %>% 
  mutate(share = Exports / sum(Exports)) %>% 
  filter(code != "ROW") %>% 
  arrange(-share) %>% 
  slice_head(n = 10) %>% 
  select(s, r, code, share_2021 = share) %>% 
  left_join(exports22, by = c("s", "r", "code")) %>% 
  mutate(code = factor(code, levels = rev(code))) %>% 
  pivot_longer(
    cols = starts_with("share_"),
    names_to = c(".value", "t"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  mutate(
    t = factor(t, levels = rev(years)),
    label = paste0(format(round(100 * share, 1), nsmall = 1), "%")
  )

davax22 <- df %>% 
  filter(t == years[2]) %>% 
  mutate(
    davax = DAVAX1 + DAVAX2,
    share = davax / sum(davax)
  ) %>% 
  select(s, r, code, share_2022 = share)

davax <- df %>% 
  filter(t == years[1]) %>% 
  mutate(
    davax = DAVAX1 + DAVAX2,
    share = davax / sum(davax)
  ) %>% 
  filter(code != "ROW") %>% 
  arrange(-share) %>% 
  slice_head(n = 10) %>% 
  select(s, r, code, share_2021 = share) %>% 
  left_join(davax22, by = c("s", "r", "code")) %>% 
  mutate(code = factor(code, levels = rev(code))) %>% 
  pivot_longer(
    cols = starts_with("share_"),
    names_to = c(".value", "t"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  mutate(
    t = factor(t, levels = rev(years)),
    label = paste0(format(round(100 * share, 1), nsmall = 1), "%")
  )


rex22 <- df %>% 
  filter(t == years[2]) %>% 
  mutate(
    rex = REX1 + REX2 + REX3 + REF1 + REF2,
    share = rex / sum(rex)
  ) %>% 
  select(s, r, code, share_2022 = share)

rex <- df %>% 
  filter(t == years[1]) %>% 
  mutate(
    rex = REX1 + REX2 + REX3 + REF1 + REF2,
    share = rex / sum(rex)
  ) %>% 
  filter(code != "ROW") %>% 
  arrange(-share) %>% 
  slice_head(n = 10) %>% 
  select(s, r, code, share_2021 = share) %>% 
  left_join(rex22, by = c("s", "r", "code")) %>% 
  mutate(code = factor(code, levels = rev(code))) %>% 
  pivot_longer(
    cols = starts_with("share_"),
    names_to = c(".value", "t"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  mutate(
    t = factor(t, levels = rev(years)),
    label = paste0(format(round(100 * share, 1), nsmall = 1), "%")
  )


df_fva <- here("..", "mrio-processing", "data", "interim", "trade-accounting", "ta-fva.parquet") %>% 
  read_parquet() %>% 
  filter(s == select & t %in% years) %>% 
  left_join(countries, by = c("u" = "mrio"))

fva22 <- df_fva %>% 
  filter(t == years[2]) %>% 
  mutate(share = FVAorigin / FVA) %>% 
  select(s, u, code, share_2022 = share)

fva <- df_fva %>% 
  filter(t == years[1]) %>% 
  mutate(share = FVAorigin / FVA) %>% 
  filter(code != "ROW") %>% 
  arrange(-share) %>% 
  slice_head(n = 10) %>% 
  select(s, u, code, share_2021 = share) %>% 
  left_join(fva22, by = c("s", "u", "code")) %>% 
  mutate(code = factor(code, levels = rev(code))) %>% 
  pivot_longer(
    cols = starts_with("share_"),
    names_to = c(".value", "t"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  mutate(
    t = factor(t, levels = rev(years)),
    label = paste0(format(round(100 * share, 1), nsmall = 1), "%")
  )

names <- bind_rows(exports, davax, rex, fva) %>%
  distinct(code) %>%
  mutate(code = as.character(code)) %>%
  arrange(code)

# PLOT ----

text <- geom_text(
  aes(label = label, x = share + .01), 
  position = position_dodge(.9), hjust = 0, size = 2, alpha = .5
)
scale <- scale_x_continuous(limits = c(0, .34))
alpha <- scale_alpha_manual(values = c(.5, 1))
theme <- theme(
  plot.margin = margin(15, 0, 10, 0),
  plot.title = element_text(size = 10, face = "bold"),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 7, margin = margin(0, -3, 0, 0)),
  legend.position = "none",
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank()
)

plot1 <- ggplot(exports, aes(x = share, y = code, group = t)) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "gray20"
  ) +
  labs(title = "Exports") + alpha + text + scale + theme +
  annotate("text", x = .01, y = 10.2, hjust = 0, label = 2021, size = 1.75, color = "white") +
  annotate("text", x = .01, y = 9.8, hjust = 0, label = 2022, size = 1.75, color = "white")

plot2 <- ggplot(davax, aes(x = share, y = code, group = t)) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "#007db7"
  ) +
  labs(title = "DAVAX") + alpha + text + scale + theme

plot3 <- ggplot(rex, aes(x = share, y = code, group = t)) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "#00A5D2"
  ) +
  labs(title = "REX") + alpha + text + scale + theme

plot4 <- ggplot(fva, aes(x = share, y = code, group = t)) + 
  geom_bar(
    aes(alpha = t), 
    stat = "identity", position = position_dodge(.85), width = .8, fill = "#6BB305"
  ) +
  labs(title = "FVA origin") + alpha + text + scale + theme +
  annotate("text", x = .33, y = 8.8, hjust = 1, label = "33.9%", size = 2, alpha = .5)


plot <- plot_grid(
  plot1, plot2, plot3, plot4, 
  ncol = 4, rel_widths = c(1, 1, 1, 1)
)

ggsave(
  here("figures", "3.2_anatomy.pdf"),
  plot,
  device = cairo_pdf,
  width = 16,
  height = 10,
  unit = "cm"
)

######### END #########
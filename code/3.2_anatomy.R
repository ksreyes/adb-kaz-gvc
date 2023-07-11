# TRADE ANATOMY

# PRELIMINARIES ----

rm(list = ls())
library(here)
library(arrow)
library(readxl)
library(tidyverse)
library(cowplot)

show <- 10
year <- 2021

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio)))

select <- countries$mrio[which(countries$name == "Kazakhstan")]

df <- here("..", "mrio-processing", "data", "interim", "trade-accounting", "ta.parquet") %>% 
  read_parquet() %>% 
  filter(s == select & t == year) %>% 
  left_join(countries, by = c("r" = "mrio"))

exports <- df %>% 
  mutate(share = Exports / sum(Exports)) %>% 
  filter(code != "ROW") %>% 
  arrange(-share) %>% 
  slice_head(n = 10) %>% 
  select(t, s, r, code, share) %>% 
  mutate(
    code = factor(code, levels = rev(code)),
    label = paste0(format(round(100 * share, 1), nsmall = 1), "%")
  )

davax <- df %>% 
  mutate(
    davax = DAVAX1 + DAVAX2,
    share = davax / sum(davax)
  ) %>% 
  filter(code != "ROW") %>% 
  arrange(-share) %>% 
  slice_head(n = 10) %>% 
  select(t, s, r, code, share) %>% 
  mutate(
    code = factor(code, levels = rev(code)),
    label = paste0(format(round(100 * share, 1), nsmall = 1), "%")
  )

rex <- df %>% 
  mutate(
    rex = REX1 + REX2 + REX3 + REF1 + REF2,
    share = rex / sum(rex)
  ) %>% 
  filter(code != "ROW") %>% 
  arrange(-share) %>% 
  slice_head(n = 10) %>% 
  select(t, s, r, code, share) %>% 
  mutate(
    code = factor(code, levels = rev(code)),
    label = paste0(format(round(100 * share, 1), nsmall = 1), "%")
  )

fva <- here("..", "mrio-processing", "data", "interim", "trade-accounting", "ta-fva.parquet") %>% 
  read_parquet() %>% 
  filter(s == select & t == year) %>% 
  left_join(countries, by = c("u" = "mrio")) %>% 
  mutate(
    share = FVAorigin / FVA
  ) %>% 
  filter(code != "ROW") %>% 
  arrange(-share) %>% 
  slice_head(n = 10) %>% 
  select(t, s, u, code, share) %>% 
  mutate(
    code = factor(code, levels = rev(code)),
    label = paste0(format(round(100 * share, 1), nsmall = 1), "%")
  )

# names <- bind_rows(davax, rex, fva) %>% 
#   select(code, name) %>% 
#   distinct(code, name) %>% 
#   mutate(code = as.character(code)) %>% 
#   arrange(code)

# PLOT ----

text <- geom_text(hjust = 0, nudge_x = .02, size = 2.5)
scale <- scale_x_continuous(limits = c(0, .5))
theme <- theme(
  plot.margin = margin(15, 2, 10, 2),
  plot.title = element_text(size = 10, face = "bold"),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 8),
  legend.position = "none",
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank()
)

plot1 <- ggplot(exports, aes(x = share, y = code, label = label)) + 
  geom_bar(stat = "identity", width = .8, fill = "gray20") +
  labs(title = "Exports") + 
  text + scale + theme

plot2 <- ggplot(davax, aes(x = share, y = code, label = label)) + 
  geom_bar(stat = "identity", width = .8, fill = "#007db7") +
  labs(title = "DAVAX") + 
  text + scale + theme

plot3 <- ggplot(rex, aes(x = share, y = code, label = label)) + 
  geom_bar(stat = "identity", width = .8, fill = "#00A5D2") +
  labs(title = "REX") + 
  text + scale + theme

plot4 <- ggplot(fva, aes(x = share, y = code, label = label)) + 
  geom_bar(stat = "identity", width = .8, fill = "#8DC63F") +
  labs(title = "FVA") + 
  text + scale + theme

plot <- plot_grid(
  plot1, plot2, plot3, plot4, 
  ncol = 4, rel_widths = c(1, 1, 1, 1)
)

ggsave(
  here("figures", "3.2_anatomy.pdf"),
  plot,
  device = cairo_pdf,
  width = 16,
  height = 7,
  unit = "cm"
)

######### END #########
# GVC LENGTH

# SET UP ----

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)
library(cowplot)

club <- c(1, 2, 12, 20)

sectors <- here("..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel() %>% 
  group_by(ind, name_short) %>%
  distinct(ind) %>%
  ungroup() %>% 
  arrange(ind) %>% 
  rename(i = ind, sector = name_short) %>% 
  filter(i %in% club) %>% 
  mutate(sector = replace(sector, sector == "Wholesale trade", "Wholesale\ntrade")) %>% 
  bind_rows(tibble(i = 0, sector = "Aggregate")) %>% 
  arrange(i)

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio)))

select <- countries$mrio[which(countries$name == "Kazakhstan")]

# DATA ----

apl62 <- here("..", "mrio-processing", "data", "lengths62.parquet") %>% 
  read_parquet() %>% 
  filter(s == select & agg %in% c(0, 35) & t %in% c(2000, 2010))

apl72 <- here("..", "mrio-processing", "data", "lengths.parquet") %>% 
  read_parquet() %>% 
  filter(s == select & agg %in% c(0, 35) & t %in% c(2020, 2022))

df <- apl62 %>% 
  bind_rows(apl72) %>% 
  left_join(sectors) %>% 
  filter(i %in% c(0, club)) %>% 
  select(t, i, sector, PLvd_GVC, CBv_GVC, PLvf_GVC) %>%
  mutate(
    dummy = 0,
    t = factor(t, levels = rev(c(2000, 2010, 2020, 2022))),
    domestic = PLvd_GVC,
    foreign = CBv_GVC + PLvf_GVC,
    sector = factor(sector, levels = rev(sectors$sector))
  )

dfout <- df %>% 
  select(i, sector, t, domestic, foreign)

write_csv(dfout, here("data", "final", "3.7_gvclength.csv"))

# PLOT ----

barcolors <- c("#007db7", "#6BB305", "#e9532b")

line <- geom_vline(xintercept = 0, size = .35, color = "gray25")

alpha <- scale_alpha_manual(values = c(1, .75, .5, .25))

theme <- theme(
  plot.margin = margin(15, 10, 0, 0),
  plot.background = element_rect(fill = NA, color = NA),
  axis.title.x.top = element_text(
    size = 9, face = "bold", margin = margin(0, 0, 8, 0)
  ),
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = 9),
  axis.text.y = element_blank(),
  legend.position = "none",
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_line(color = "gray75", size = .25, linetype = "dashed")
)

plot0 <- ggplot(df, aes(x = dummy, y = sector, group = t)) + 
  geom_bar(
    width = .7, position = position_dodge(width = .85), stat = "identity"
  ) + 
  scale_x_continuous(
    name = "", position = "top", limits = c(0, 0), breaks = 0, labels = ""
  ) + 
  theme + 
  theme(
    plot.margin = margin(15, 5, 0, 5),
    axis.text.y = element_text(
      size = 9, face = c(rep("plain", 4), "bold"), color = "black", margin = margin(0, -8, 0, 0)
    ),
    panel.grid.major.x = element_blank()
  )

plot1 <- ggplot(df, aes(x = domestic - 1, y = sector, group = t, alpha = t)) + 
  geom_bar(
    width = .7, position = position_dodge(width = .85), stat = "identity", fill = barcolors[1]
  ) +
  scale_x_continuous(
    name = "Domestic stages", position = "top", 
    limits = c(0, 2.7 - 1), 
    breaks = seq(0, 2.5 - 1, .5),
    labels = seq(1, 2.5, .5)
  ) + 
  line + alpha + theme

plot2 <- ggplot(df, aes(x = foreign - 1, y = sector, group = t, alpha = t)) + 
  geom_bar(
    width = .7, position = position_dodge(width = .85), stat = "identity", fill = barcolors[2]
  ) +
  scale_x_continuous(
    name = "Foreign stages", position = "top", 
    limits = c(0, 2.95 - 1),
    breaks = seq(0, 2.5 - 1, .5),
    labels = seq(1, 2.5, .5)
  ) + 
  line + alpha + theme

guides <- guides(
  alpha = guide_legend(reverse = TRUE, override.aes = list(fill = "black"))
)

legend <- get_legend(
  plot1 + guides + 
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 5, 0, 0)),
    legend.position = "bottom")
  )

plot <- plot_grid(
  plot0, plot1, plot2, nrow = 1, rel_widths = c(.4, 2.6 - 1, 2.8 - 1)
)

plot <- plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .15))

ggsave(
  here("figures", "3.7_gvclengths.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 10, unit = "cm"
)

######### END #########
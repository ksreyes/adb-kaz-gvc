# APL

# PRELIMINARIES ----

rm(list = ls())
library(here)
library(readxl)
library(tidyverse)
library(ggrepel)
library(cowplot)

sectors <- here("..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel() %>%
  group_by(ind, abv, name, ind5, name5) %>%
  distinct(ind) %>%
  ungroup() %>% 
  bind_rows(
    tibble(
      ind = 0, ind5 = 0, abv = "AGG", name = "Aggregate", name5 = "Aggregate")
  ) %>% 
  arrange(ind5)

order <- c(unique(sectors$name5)[-1], unique(sectors$name5)[1])

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio)))

select <- countries$mrio[which(countries$name == "Kazakhstan")]

# DATA ----

apl62 <- here("..", "mrio-processing", "data", "lengths62.csv") %>% 
  read_csv() %>% 
  filter(s == select & agg %in% c(0, 35) & t %in% c(2000, 2010))

apl72 <- here("..", "mrio-processing", "data", "lengths.csv") %>% 
  read_csv() %>% 
  filter(s == select & agg %in% c(0, 35) & t == 2022)
  
df <- apl62 %>% 
  bind_rows(apl72) %>% 
  mutate(length = PLv_GVC + PLy_GVC) %>% 
  select(t, i, length) %>% 
  pivot_wider(names_from = t, values_from = length) %>% 
  left_join(sectors, by = c("i" = "ind")) %>% 
  mutate(
    name5 = factor(name5, levels = order),
    diff_10_00 = `2010` - `2000`,
    diff_22_10 = `2022` - `2010`,
    diff_22_00 = `2022` - `2000`
  ) %>% 
  select(i, abv, sector = name, sector5 = name5, `2000`:`2022`, diff_10_00:diff_22_00)

write_csv(df, here("data", "final", "3.6_apl.csv"))

# PLOT ----

labels <- df %>% 
  filter(i %in% c(1, 2, 12, 20)) %>% 
  select(abv, `2000`:`2022`)

plot1 <- ggplot(df) + 
  geom_abline(size = .25) + 
  annotate(
    "text", label = "Lengthened", x = 6.5, y = 10.5, angle = 45, 
    hjust = .5, vjust = 0, size = 3, color = "gray50"
  ) + 
  geom_point(
    aes(x = `2000`, y = `2010`, color = sector5), 
    size = ifelse(df$sector != "Aggregate", 4, 6),
    alpha = ifelse(df$sector != "Aggregate", .8, 1),
    shape = ifelse(df$sector != "Aggregate", 16, 18)
  ) +
  geom_point(
    mapping = aes(x = `2000`, y = `2010`), 
    data = labels, size = 4, shape = 1
  ) + 
  geom_text_repel(
    mapping = aes(x = `2000`, y = `2010`, label = abv),
    data = labels, 
    nudge_y = .15, 
    size = 3
  ) + 
  scale_x_continuous(name = 2000, limits = c(6, 11)) +  
  scale_y_continuous(name = 2010, limits = c(6, 11)) + 
  scale_color_manual(
    values = c("#E9532b", "#6DCFF6", "#007db7", "#8dc63f", "#FDB415", "gray20")
  ) + 
  theme(
    plot.margin = margin(15, 8, 2, 2),
    axis.title = element_text(size = 9),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

plot2 <- ggplot(df) + 
  geom_abline(size = .25) + 
  annotate(
    "text", label = "Shortened", x = 10.5, y = 6.5, angle = 45,
    hjust = .5, vjust = 1, size = 3, color = "gray50"
  ) + 
  geom_point(
    aes(x = `2010`, y = `2022`, color = sector5), 
    size = ifelse(df$sector != "Aggregate", 4, 6),
    alpha = ifelse(df$sector != "Aggregate", .8, 1),
    shape = ifelse(df$sector != "Aggregate", 16, 18)
  ) +
  geom_point(
    mapping = aes(x = `2010`, y = `2022`), 
    data = labels, size = 4, shape = 1
  ) + 
  geom_text_repel(
    mapping = aes(x = `2010`, y = `2022`, label = abv),
    data = labels, 
    nudge_y = .15,
    size = 3
  ) + 
  scale_x_continuous(name = 2000, limits = c(6, 11)) + 
  scale_y_continuous(
    limits = c(6, 11),
    sec.axis = sec_axis(~., name = 2022)
  ) + 
  scale_color_manual(
    values = c("#E9532b", "#6DCFF6", "#007db7", "#8dc63f", "#FDB415", "gray20")
  ) + 
  theme(
    plot.margin = margin(15, 2, 2, 8),
    axis.title.x = element_text(size = 9),
    axis.title.y.left = element_blank(),
    axis.title.y.right = element_text(size = 9),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(size = 8),
    legend.position = "none",
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

guides <- guides(
  color = guide_legend(
    nrow = 3, 
    override.aes = list(size = c(rep(3, 5), 4), alpha = 1, shape = c(rep(16, 5), 18))
  ))

legend <- get_legend(
  plot1 + guides + 
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9),
    legend.box.margin = margin(-8, 0, 0, 0),
    legend.position = "bottom")
  )

plot <- plot_grid(plot1, plot2, ncol = 2, align = "h")
plot <- plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .25))

ggsave(
  here("figures", "3.6_apl.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 10, unit = "cm"
)

######### END #########
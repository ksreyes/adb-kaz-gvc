# EXPORTS DECOMPOSITION BY SECTOR

# PRELIMINARIES ----

rm(list = ls())
library(here)
library(readxl)
library(arrow)
library(tidyverse)

sectors <- here("..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel() %>%
  group_by(ind, name_short) %>%
  distinct(ind) %>%
  arrange(ind) %>%
  rename(i = ind, sector = name_short) %>% 
  ungroup() %>% 
  bind_rows(tibble(i = 0, sector = "Aggregate"))

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio)))

select <- countries$mrio[which(countries$name == "Kazakhstan")]
year <- 2022

# DATA ----

df <- here("..", "mrio-processing", "data", "interim", "trade-accounting", "ta-es.parquet") %>% 
  read_parquet() %>%
  filter(t == year & s == select) %>% 
  mutate(
    DAVAX = DAVAX1 + DAVAX2,
    REX = REX1 + REX2 + REX3,
    REF = REF1 + REF2,
    PDC = PDC1 + PDC2
  ) %>% 
  left_join(sectors) %>% 
  select(s, i, sector, Exports, DAVAX, REX, REF, FVA, PDC) %>% 
  group_by(s, i, sector) %>% 
  summarize(across(Exports:PDC, sum)) %>% 
  ungroup()

df_agg <- df %>% 
  summarize(across(Exports:PDC, sum)) %>% 
  mutate(s = select, i = 0, sector = "Aggregate")

df <- df %>% 
  bind_rows(df_agg)

write_csv(df, here("data", "final", "3.5_sector_decomp.csv"))

# PLOT ----

dfplot <- df %>% 
  mutate(na = ifelse(Exports == 0, 1, 0)) %>% 
  arrange(desc(DAVAX / Exports)) %>% 
  arrange(desc(na)) %>% 
  mutate(sector = factor(sector, levels = sector))

aggpos <- which(dfplot$sector == "Aggregate")

bold <- rep("plain", nrow(dfplot))
bold[aggpos] <- "bold"

dfplot <- dfplot %>% 
  pivot_longer(cols = DAVAX:na, names_to = "category") %>% 
  mutate(category = factor(
    category, 
    levels = rev(c("DAVAX", "REX", "REF", "FVA", "PDC",  "na"))
  ))

e <- .34

plot <- ggplot(dfplot, aes(x = value, y = sector, fill = category)) + 
  geom_bar(stat = "identity", position = "fill", width = .7) +
  geom_rect(
    aes(xmin = 0, xmax = 1, ymin = aggpos - e, ymax = aggpos + e),
    data = dfplot[1, ], 
    fill = NA, 
    color = "black"
  ) + 
  
  geom_vline(xintercept = .25, size = .25, color = "gray25", linetype = "dashed") + 
  geom_vline(xintercept = .5, size = .25, color = "gray25", linetype = "dashed") + 
  geom_vline(xintercept = .75, size = .25, color = "gray25", linetype = "dashed") + 
  
  scale_fill_manual(
    labels = rev(c("DAVAX", "REX", "REF", "FVA", "PDC", "")),
    values = rev(c("#007db7", "#00A5D2", "#63CCEC", "#8DC63F", "#E9532B", "gray75"))
  ) + 
  scale_x_continuous(
    breaks = c(.25, .5, .75),
    labels = function(x) paste0(100 * x, "%")
  ) + 
  guides(
    fill = guide_legend(
      reverse = TRUE, nrow = 1,
      override.aes = list(fill = c("#007db7", "#00A5D2", "#63CCEC", "#8DC63F", "#E9532B", "white")))
  ) + 
  
  theme(
    plot.margin = margin(15, 2, 12, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(
      size = 7.5, margin = margin(0, -12, 0, 0), face = bold, color = "black"
    ),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 10, 0, 0)),
    legend.position = "bottom",
    legend.box.margin = margin(-5, 40, 0, 0), 
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank()
  )

ggsave(
  here("figures", "3.5_sector_decomp.pdf"),
  plot,
  device = cairo_pdf,
  width = 16,
  height = 17,
  unit = "cm"
)

######### END #########
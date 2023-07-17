# EXPORTS DECOMPOSITION

# PRELIMINARIES ----

rm(list = ls())
library(here)
library(arrow)
library(readxl)
library(tidyverse)

countries <- here("..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio)))

select <- countries$mrio[which(countries$name == "Kazakhstan")]

# LOAD DATA ----

df72 <- here("..", "mrio-processing", "data", "trade-accounting", "ta.parquet") %>% 
  read_parquet() %>% 
  filter(s == select) %>% 
  group_by(t) %>% 
  summarize(across(Exports:PDC2, sum)) %>%
  mutate(DAVAX = DAVAX1 + DAVAX2,
         REX = REX1 + REX2 + REX3,
         REF = REF1 + REF2,
         PDC = PDC1 + PDC2) %>% 
  select(t, Exports, DAVAX1, DAVAX:REF, FVA, PDC)

df62 <- here("..", "mrio-processing", "data", "trade-accounting", "ta62.parquet") %>% 
  read_parquet() %>% 
  filter(s == select & t < 2017) %>%
  group_by(t) %>% 
  summarize(across(Exports:PDC2, sum)) %>%
  mutate(DAVAX = DAVAX1 + DAVAX2,
         REX = REX1 + REX2 + REX3,
         REF = REF1 + REF2,
         PDC = PDC1 + PDC2) %>% 
  select(t, Exports, DAVAX1, DAVAX:REF, FVA, PDC)

decomp <- df62 %>% 
  bind_rows(df72) %>% 
  mutate(t = as.numeric(t))

write_csv(decomp, here("data", "final", "3.1_decomp.csv"))

df <- decomp %>% 
  select(t, DAVAX:REF, FVA, PDC)

# PLOT ----

# Add empty 2001

df01 <- df[1, ] %>%
  mutate(t = 2001)
df01[1, -1] <- 0

df <- df %>%
  bind_rows(df01) %>%
  pivot_longer(cols = DAVAX:PDC,
               names_to = "category") %>%
  mutate(category = factor(category,
                           levels = rev(c("DAVAX", "REX", "REF", "FVA", "PDC"))),
         t = factor(t, levels = c(2000:2001, 2007:2022)))

# Box production-based GVC

box <- decomp %>%
  select(t, Exports, DAVAX1) %>%
  mutate(x = setdiff(1:(n() + 1), 2),
         max = Exports / 1000,
         min = DAVAX1 / 1000)

e <- .34

# Plot and export

plot <- ggplot() + 
  
  geom_bar(
    data = df, 
    aes(x = t, y = value / 1000, fill = category),
    stat = "identity", position = "stack", width = .7
  ) +
  geom_rect(
    data = box,
    aes(ymin = min, ymax = max, xmin = x - e, xmax = x + e),
    fill = NA, color = "black"
  ) +
  geom_hline(yintercept = 0, size = .25, color = "gray25") + 
  
  annotate(
    "text", label = "Production-based\nGVCs", 
    x = 13, y = 90, size = 3, hjust = .5, color = "gray25"
  ) + 
  geom_segment(
    data = df[1, ],
    x = 11.45, xend = 10.35, y = 90, yend = 83, linewidth = .5, color = "gray25"
  ) + 
  
  scale_x_discrete(labels = c(2000, "", 2007, "08", "09", 10:22)) +
  scale_y_continuous(name = "$ billion", limits = c(0, 105)) +
  scale_fill_manual(
    values = rev(c("#007db7", "#00A5D2", "#63CCEC", "#8DC63F", "#E9532B"))
  ) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  
  theme(
    plot.margin = margin(13, 2, 10, 2),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 9, angle = 0, hjust = 1, margin = margin(0, -20, 0, 0)
    ),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9, margin = margin(-8, 0, 0, 0)),
    axis.text.y = element_text(size = 9),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 10, 0, 0)),
    legend.position = "bottom",
    legend.box.margin = margin(-5, 15, 0, 0), 
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray75", size = .25, linetype = "dashed"
    ))

ggsave(
  here("figures", "3.1_decomp.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 10, unit = "cm"
)

######### END #########
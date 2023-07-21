# EXPORTS OF GOODS BY BROAD PRODUCT CATEGORIES

rm(list = ls())
library(here)
library(readxl)
library(tidyverse)
library(arrow)
library(DBI)
library(duckdb)

countries <- here("..", "..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel()

select <- countries %>% filter(name == "Kazakhstan") %>% pull(iso_num)

# Choose products to highlight

products <- here("..", "..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel(sheet = "hs02_mrio") %>%
  select(hs02:section_name) %>% 
  mutate(hs02 = as.numeric(hs02))

group <- rep("Others", nrow(products))
group[which(products$section %in% 1:4)] <- "Food"
group[which(products$section == 5)] <- "Minerals: others"
group[which(products$hs02 == 270900)] <- "Minerals: crude oil"
group[which(products$hs02 %in% c(271111, 271121))] <- "Minerals: natural gas"
group[between(products$hs02, 260111, 261790)] <- "Minerals: ores"
group[which(products$section == 6)] <- "Chemicals"
group[which(products$section == 15)] <- "Metals: others"
group[between(products$hs02, 720211, 720299)] <- "Metals: ferroalloys"
group[between(products$hs02, 740110, 741999)] <- "Metals: copper"

products <- products %>% 
  mutate(group = group)

groups <- tibble(
  name = c(
    "Food",
    "blank1",
    "Minerals: crude oil",
    "Minerals: natural gas",
    "Minerals: ores",
    "Minerals: others",
    "blank2",
    "Chemicals",
    "Metals: ferroalloys",
    "Metals: copper",
    "Metals: others",
    "Others"
  ),
  labs = c(
    "1    Food",
    " ",
    "     Minerals: crude oil",
    "     Minerals: natural gas",
    "     Minerals: ores",
    "     Minerals: others",
    " ",
    "2    Chemicals",
    "3    Metals: ferroalloys",
    "4    Metals: copper",
    "5    Metals: others",
    "6    Others"
  ),
  color = c(
    "#007DB7",
    NA,
    "#E9532B", 
    "#F57F29", 
    "#FDB415",
    "#f2e600",
    NA,
    "#0099d8", 
    "#0088c7", 
    "#00A5D2", 
    "#6dbce3", 
    "#6DCFF6"
  ),
  color_key = c(
    NA,
    NA,
    "#E9532B", 
    "#F57F29", 
    "#FDB415",
    "#f2e600",
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
  ))

# DATA ----

df <- dbConnect(duckdb::duckdb(), dbdir = ":memory:") %>% 
  dbGetQuery(sprintf(
    "SELECT * 
    FROM read_parquet('../../mrio-processing/data/external/BACI_HS02_V202301.parquet') 
    WHERE i = %s",
    select
  )) %>% 
  mutate(k = as.numeric(k)) %>% 
  left_join(products, by = c("k" = "hs02")) %>%
  group_by(t, i, group) %>%
  summarize(v = sum(v)) %>%
  ungroup() %>%
  mutate(share = v / sum(v)) %>%
  select(t, group, v) %>%
  bind_rows(tibble(
    t = 2010,
    group = c("blank1", "blank2"),
    v = c(0, 0)
  )) %>% 
  mutate(
    t = factor(t, levels = 2002:2021),
    group = factor(group, levels = groups %>% pull(name) %>% rev())
  )

dfout <- df %>%
  filter(!(group %in% c("blank1", "blank2"))) %>%
  pivot_wider(names_from = group, values_from = v) %>%
  select(
    t,
    Food,
    `Minerals: crude oil`,
    `Minerals: natural gas`,
    `Minerals: ores`,
    `Minerals: others`,
    Chemicals,
    `Metals: ferroalloys`,
    `Metals: copper`,
    `Metals: others`,
    Others
  ) %>% 
  replace_na(list(`Minerals: natural gas` = 0)) %>% 
  rowwise(t) %>%
  mutate(Total = sum(c_across(Food:Others))) %>%
  arrange(desc(t))

write_csv(dfout, here("data", "final", "2.4_products.csv"))

# PLOT ----

pos <- groups %>%
  left_join(subset(df, t == 2021), by = c("name" = "group")) %>%
  mutate(x = cumsum(v / sum(v)))

plot <- ggplot(df, aes(x = v, y = t, fill = group)) +
  geom_bar(stat = "identity", position = "fill", width = .7) +
  geom_vline(
    xintercept = .75,
    linewidth = .25,
    linetype = "dashed",
    color = "gray25"
  ) +
  geom_vline(
    xintercept = .25,
    linewidth = .25,
    linetype = "dashed",
    color = "gray25"
  ) +
  scale_x_continuous(breaks = c(.25, .75), labels = c("25%", "75%")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(labels = rev(groups$labs), values = rev(groups$color)) +
  annotate(
    "text",
    x = pos$x[1] / 2,
    y = 20,
    label = "1",
    size = 2.5
  ) +
  annotate(
    "text",
    x = (pos$x[8] + pos$x[7]) / 2,
    y = 20,
    label = "2",
    size = 2.5
  ) +
  annotate(
    "text",
    x = (pos$x[9] + pos$x[8]) / 2,
    y = 20,
    label = "3",
    size = 2.5
  ) +
  annotate(
    "text",
    x = (pos$x[10] + pos$x[9]) / 2 - .01,
    y = 20,
    label = "4",
    size = 2.5
  ) +
  annotate(
    "text",
    x = (pos$x[11] + pos$x[10]) / 2,
    y = 20,
    label = "5",
    size = 2.5
  ) +
  annotate(
    "text",
    x = (pos$x[12] + pos$x[11]) / 2,
    y = 20,
    label = "6",
    size = 2.5
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,
    override.aes = list(fill = groups$color_key)
  )) +
  theme(
    plot.margin = margin(15, 2, 12, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9, margin = margin(0, -10, 0, 0)),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 8, margin = margin(0, 0, 0, -13)),
    legend.position = "right",
    legend.box.margin = margin(0, 0, 0, -25),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank()
  )

ggsave(
  here("figures", "2.4_products.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 12, unit = "cm"
)

# APPENDIX ----

products <- here("..", "..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel(sheet = "hs02_mrio") %>%
  mutate(hs02 = as.numeric(hs02)) %>% 
  select(hs02:section_name)

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Check which exports are largest

baci21 <- con %>% 
  dbGetQuery(sprintf(
    "SELECT * 
    FROM read_parquet('../../mrio-processing/data/external/BACI_HS02_V202301.parquet') 
    WHERE i = %s AND t = 2021",
    select
  )) %>% 
  left_join(products, by = c("k" = "hs02")) %>% 
  group_by(t, i, k, hs02_desc, section, section_name) %>% 
  summarize(v = sum(v)) %>% 
  ungroup() %>% 
  mutate(share = v / sum(v))

write_csv(baci21, here("data", "interim", "kaz_exports_2021.csv"))

# Check who receives exports

baci21_product <- con %>% 
  dbGetQuery(sprintf(
    "SELECT * 
    FROM read_parquet('../../mrio-processing/data/external/BACI_HS02_V202301.parquet') 
    WHERE i = %s AND t = 2021 AND k = 270900",
    select
  )) %>% 
  left_join(countries, by = c("j" = "iso_num")) %>% 
  select(t, i, j, name, v, q)

write_csv(baci21_product, here("data", "interim", "kaz_exports_oil_2021.csv"))

# Check what is exported to top partners

baci21_partners <- con %>% 
  dbGetQuery(sprintf(
    "SELECT * 
    FROM read_parquet('../../mrio-processing/data/external/BACI_HS02_V202301.parquet') 
    WHERE i = %s AND t = 2021",
    select
  )) %>% 
  filter(j %in% c(156, 643, 300, 826, 276)) %>% 
  left_join(countries, by = c("j" = "iso_num")) %>% 
  left_join(products, by = c("k" = "hs02")) %>% 
  select(t, i, j, name, k, hs02_desc, v, q) %>% 
  group_by(t, i, j, name) %>% 
  arrange(-v, .by_group = TRUE) %>% 
  ungroup()

write_csv(baci21_partners, here("data", "interim", "kaz_partners_2021.csv"))

# Metals

baci21_products_partners <- dbConnect(duckdb::duckdb(), dbdir = ":memory:") %>% 
  dbGetQuery(sprintf(
    "SELECT * 
    FROM read_parquet('../../mrio-processing/data/external/BACI_HS02_V202301.parquet') 
    WHERE i = %s AND t = 2021",
    select
  )) %>% 
  mutate(k = as.numeric(k)) %>% 
  left_join(products, by = c("k" = "hs02")) %>%
  filter(section == 15) %>% 
  group_by(t, i, j) %>% 
  summarize(v = sum(v)) %>% 
  ungroup() %>% 
  left_join(countries, by = c("j" = "iso_num")) %>% 
  select(t, i, j, name, v)

write_csv(baci21_products_partners, here("data", "interim", "kaz_products_partners_2021.csv"))

######### END #########
# EXPORTS OF GOODS BY BROAD PRODUCT CATEGORIES

rm(list = ls())
library(readxl)
library(tidyverse)

years <- 2002:2021
country <- 398

# Choose products to highlight

products <-
  read_excel("data/interim/sectors.xlsx", sheet = "hs02_mrio") %>%
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
  )
)

# DATA ----

df1 <- tibble()

for (t in 1:length(years)) {
  
  baci <-
    read_csv(paste0(
      "data/raw/BACI_HS02_V202301/BACI_HS02_Y",
      years[t],
      "_V202301.csv"
    )) %>%
    mutate(k = as.numeric(k))
  
  df_t <- baci %>%
    filter(i == country) %>%
    left_join(products, by = c("k" = "hs02")) %>%
    group_by(t, i, group) %>%
    summarize(v = sum(v)) %>%
    ungroup() %>%
    mutate(share = v / sum(v)) %>%
    select(t, group, v) %>%
    bind_rows(tibble(
      t = years[t],
      group = c("blank1", "blank2"),
      v = c(0, 0)
    ))

  df1 <- rbind(df1, df_t)
  
}

df <-
  df1 %>% mutate(t = factor(t, levels = years),
                 group = factor(group, levels = rev(groups$name)))

dfout <- df %>%
  filter(!(group %in% c("blank1", "blank2"))) %>%
  pivot_wider(names_from = group,
              values_from = v) %>%
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

write_csv(dfout, "data/final/2.4_products.csv")

# PLOT ----

pos <- groups %>%
  left_join(subset(df, t == 2021), by = c("name" = "group")) %>%
  mutate(x = cumsum(v / sum(v)))

plot <- ggplot(df, aes(x = v, y = t, fill = group)) +
  geom_bar(stat = "identity",
           position = "fill",
           width = .7) +
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
  scale_fill_manual(labels = rev(groups$labs),
                    values = rev(groups$color)) +
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
    axis.text.y = element_text(size = 9, margin = margin(0,-10, 0, 0)),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 8, margin = margin(0, 0, 0,-13)),
    legend.position = "right",
    legend.box.margin = margin(0, 0, 0,-25),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank()
  )

ggsave(
  "figures/2.4_products.pdf",
  plot,
  device = cairo_pdf,
  width = 16,
  height = 12,
  unit = "cm"
)

# ggsave(
#   "figures/2.4_products.png",
#   plot,
#   width = 16,
#   height = 12,
#   unit = "cm"
# )

# APPENDIX ----

# Check which exports are largest

products <-
  read_excel("data/interim/sectors.xlsx", sheet = "hs02_mrio") %>%
  select(hs02:section_name)

baci19 <- read_csv("data/raw/BACI_HS02_V202301/BACI_HS02_Y2019_V202301.csv")
baci02 <- read_csv("data/raw/BACI_HS02_V202301/BACI_HS02_Y2002_V202301.csv")

df19 <- baci19 %>%
  filter(i == country) %>%
  left_join(products, by = c("k" = "hs02")) %>% 
  group_by(t, i, k, hs02_desc, section, section_name) %>% 
  summarize(v = sum(v)) %>% 
  ungroup() %>% 
  mutate(share = v / sum(v))

df02 <- baci02 %>%
  filter(i == country) %>%
  left_join(products, by = c("k" = "hs02")) %>% 
  group_by(t, i, k, hs02_desc, section, section_name) %>% 
  summarize(v = sum(v)) %>% 
  ungroup() %>% 
  mutate(share = v / sum(v))

write_csv(df19, "data/interim/kaz_exports_2019.csv")
write_csv(df02, "data/interim/kaz_exports_2002.csv")

######### END #########
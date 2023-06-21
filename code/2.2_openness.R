# OPENNESS & DEVELOPMENT

rm(list = ls())
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)

highlight <-
  c("Saudi Arabia",
    "Turkey",
    "PRC",
    "Kyrgyz Republic",
    "Uzbekistan",
    "Russian Federation",
    "Brunei Darussalam")

# DATA ----

wb_groups <- read_excel("data/interim/countries.xlsx",
                        sheet = "World Bank") %>%
  filter(is.na(iso_num) & !(name %in% c("Channel Islands",
                                        "Monaco",
                                        "Kosovo"))) %>%
  select(wb_code, name)

countries <- read_excel("data/interim/countries.xlsx") %>%
  select(name_wb, name) %>%
  mutate_if(is.character,
            str_replace_all,
            pattern = "People's Republic of China",
            replacement = "PRC")

wb1 <-
  read_excel(
    "data/raw/API_NE.TRD.GNFS.ZS_DS2_en_excel_v2_5552026.xls",
    sheet = "Data",
    skip = 3
  ) %>%
  pivot_longer(cols = matches("[0-9]{4}"),
               names_to = "t",
               values_to = "openness") %>%
  select(`Country Name`, `Country Code`, t, openness)

wb2 <-
  read_excel(
    "data/raw/API_NY.GDP.MKTP.PP.CD_DS2_en_excel_v2_5552699.xls",
    sheet = "Data",
    skip = 3
  ) %>%
  pivot_longer(cols = matches("[0-9]{4}"),
               names_to = "t",
               values_to = "gdp") %>%
  select(`Country Name`, `Country Code`, t, gdp)

df <- wb1 %>%
  left_join(wb2) %>%
  mutate(wb_code = `Country Code`,
         name_wb = `Country Name`) %>%
  left_join(countries) %>%
  select(wb_code, name_wb, name, t, openness, gdp) %>%
  filter(!(wb_code %in% wb_groups$wb_code) & t == 2021)

cat <- df %>%
  filter(name == "Kazakhstan")

dfout <- df %>%
  drop_na %>%
  select(t, name, openness, gdp)

write_csv(dfout, "data/final/2.2_openness.csv")

# PLOT ----

df_hl <- df %>%
  filter(name %in% highlight)

df_pak <- df %>%
  filter(name == "Kazakhstan")

plot <- ggplot() +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(openness)),
    data = subset(df, !(name %in% highlight)),
    shape = 16,
    color = "gray75",
    size = 2,
    alpha = .6
  ) +
  geom_smooth(
    mapping = aes(x = log10(gdp), y = log2(openness)),
    data = df,
    method = "glm",
    formula = y ~ x,
    se = FALSE,
    show.legend = FALSE,
    color = "#E88468",
    size = .5
  ) +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(openness)),
    data = df_hl,
    shape = 16,
    color = "#007db7",
    size = 3,
    alpha = .6
  ) +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(openness)),
    data = df_pak,
    shape = 21,
    fill = "#007db7",
    color = "black",
    size = 4
  ) +
  geom_text_repel(
    mapping = aes(
      x = log10(gdp),
      y = log2(openness),
      label = name
    ),
    data = df_hl,
    point.padding = 5,
    size = 2.5
  ) +
  geom_text(
    mapping = aes(
      x = log10(gdp),
      y = log2(openness),
      label = name
    ),
    data = df_pak,
    nudge_x = .16,
    nudge_y = -.2,
    size = 3,
    fontface = "bold"
  ) +
  scale_x_continuous(
    name = "GDP ($ PPP)",
    breaks = c(log10(10 ^ 8), log10(10 ^ 10), log10(10 ^ 12)),
    label = c("$100 million", "$10 billion", "$1 trillion")
  ) +
  scale_y_continuous(
    name = "Trade (% of GDP)",
    limits = c(log2(20), log2(250)),
    breaks = c(log2(25), log2(50), log2(100), log2(200)),
    label = function(x) paste0(2 ^ x, "%")
  ) +
  theme(
    plot.margin = margin(10, 2, 15, 2),
    axis.title.x = element_text(size = 9, margin = margin(5, 0, 0, 0)),
    axis.title.y = element_text(size = 9, margin = margin(0, 5, 0, 0)),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "gray20",
      linewidth = .5
    ),
    panel.grid.major = element_line(
      color = "gray75",
      linewidth = .25,
      linetype = "dashed"
    )
  )

ggsave("figures/2.2_openness.png", plot, 
       dpi = 300, width = 16, height = 10, unit = "cm")

ggsave("figures/2.2_openness.pdf", plot, device = cairo_pdf,
       dpi = 300, width = 16, height = 10, unit = "cm")

######### END #########
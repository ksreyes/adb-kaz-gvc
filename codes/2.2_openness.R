# OPENNESS & DEVELOPMENT

rm(list = ls())
library(here)
library(readxl)
library(tidyverse)
library(ggrepel)

highlight <-
  c(
    "Saudi Arabia",
    "Turkey",
    "PRC",
    "Kyrgyz Republic",
    "Uzbekistan",
    "Russian Federation",
    "Norway",
    "United Kingdom",
    "New Zealand"
  )

# DATA ----

wb_groups <- here("..", "..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel(sheet = "World Bank") %>%
  filter(
    is.na(iso_num) & !(wb_name %in% c("Channel Islands", "Monaco", "Kosovo"))
  ) %>%
  select(wb_code, wb_name)

countries <- here("..", "..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  select(wb_name, name) %>% 
  mutate(name = replace(name, name == "People's Republic of China", "PRC"))

wb1 <- here("data", "raw", "API_NE.TRD.GNFS.ZS_DS2_en_excel_v2_5552026.xls") %>% 
  read_excel(sheet = "Data", skip = 3) %>%
  pivot_longer(
    cols = matches("[0-9]{4}"),
    names_to = "t",
    values_to = "openness"
  ) %>%
  select(`Country Name`, `Country Code`, t, openness)

wb2 <- here("data", "raw", "API_NY.GDP.MKTP.PP.CD_DS2_en_excel_v2_5552699.xls") %>% 
  read_excel(sheet = "Data", skip = 3) %>%
  pivot_longer(
    cols = matches("[0-9]{4}"),
    names_to = "t",
    values_to = "gdp"
  ) %>%
  select(`Country Name`, `Country Code`, t, gdp)

df <- wb1 %>%
  left_join(wb2) %>%
  mutate(wb_code = `Country Code`, wb_name = `Country Name`) %>%
  left_join(countries) %>%
  select(wb_code, wb_name, name, t, openness, gdp) %>%
  filter(!(wb_code %in% wb_groups %>% pull(wb_code)) & t == 2021)

# Add Kazakhstan excluding oil trade

merch_trade <- here("data", "raw", "API_TG.VAL.TOTL.GD.ZS_DS2_en_excel_v2_5556032.xls") %>% 
  read_excel(sheet = "Data", skip = 3) %>%
  pivot_longer(
    cols = matches("[0-9]{4}"),
    names_to = "t",
    values_to = "merch_trade"
  ) %>%
  filter(`Country Name` == "Kazakhstan" & t == 2021) %>% 
  pull(merch_trade)

fuel_exports <- here("data", "raw", "API_TX.VAL.FUEL.ZS.UN_DS2_en_excel_v2_5555455.xls") %>% 
  read_excel(sheet = "Data", skip = 3) %>%
  pivot_longer(
    cols = matches("[0-9]{4}"),
    names_to = "t",
    values_to = "fuel_exports"
  ) %>%
  select(`Country Name`, t, fuel_exports) %>% 
  filter(`Country Name` == "Kazakhstan" & t >= 2010 & t <= 2020)

fuel_imports <- here("data", "raw", "API_TM.VAL.FUEL.ZS.UN_DS2_en_excel_v2_5555456.xls") %>% 
  read_excel(sheet = "Data", skip = 3) %>%
  pivot_longer(
    cols = matches("[0-9]{4}"),
    names_to = "t",
    values_to = "fuel_imports"
  ) %>%
  select(`Country Name`, t, fuel_imports) %>% 
  filter(`Country Name` == "Kazakhstan" & t >= 2010 & t <= 2020)

fuel_trade <- fuel_exports %>% 
  left_join(fuel_imports) %>% 
  mutate(fuel_trade = fuel_exports + fuel_imports) %>% 
  summarize(fuel_trade = mean(fuel_trade)) %>% 
  pull(fuel_trade)

adjustment <- merch_trade * fuel_trade / 100
openness <- df %>% 
  filter(name_wb == "Kazakhstan") %>% 
  pull(openness)
openness_adj <- openness - adjustment

df <- df %>%
  bind_rows(
    tibble(
      wb_code = "KAZ",
      name_wb = "Kazakhstan",
      name = "Kazakhstan (exc. oil)",
      t = "2021",
      openness = openness_adj,
      gdp = 545041403820
    )
  ) %>% 
  arrange(wb_code)

# Export dataset

dfout <- df %>%
  drop_na %>%
  select(t, name, openness, gdp)

write_csv(dfout, here("data", "final", "2.2_openness.csv"))

# PLOT ----

df_highlight <- df %>%
  filter(name %in% highlight)

df_select_1 <- df %>%
  filter(name == "Kazakhstan")

df_select_2 <- df %>%
  filter(name == "Kazakhstan (exc. oil)")

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
    linewidth = .5
  ) +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(openness)),
    data = df_highlight,
    shape = 16,
    color = "#007db7",
    size = 3,
    alpha = .6
  ) +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(openness)),
    data = df_select_1,
    shape = 21,
    fill = "#007db7",
    color = "black",
    size = 4
  ) +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(openness)),
    data = df_select_2,
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
    data = df_highlight,
    point.padding = 5,
    size = 2.5
  ) +
  geom_text(
    mapping = aes(
      x = log10(gdp),
      y = log2(openness),
      label = name
    ),
    data = df_select_1,
    nudge_x = .05,
    nudge_y = -.21,
    size = 3,
    fontface = "bold"
  ) +
  geom_text(
    mapping = aes(
      x = log10(gdp),
      y = log2(openness),
      label = name
    ),
    data = df_select_2,
    nudge_x = .1,
    hjust = 0,
    size = 3,
    fontface = "bold"
  ) +
  scale_x_continuous(
    name = "GDP ($ PPP)",
    limits = c(log10(9 ^ 10), log10(range(df %>% drop_na(gdp) %>% pull(gdp))[2])),
    breaks = c(log10(10 ^ 10), log10(10 ^ 11), log10(10 ^ 12), log10(10 ^ 13)),
    label = c("$10 billion", "$100 billion", "$1 trillion", "$10 trillion")
  ) +
  scale_y_continuous(
    name = "Trade (% of GDP)",
    limits = c(log2(15), log2(250)),
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
    panel.border = element_rect(fill = NA, color = "gray20", linewidth = .5),
    panel.grid.major = element_line(color = "gray75", linewidth = .25, linetype = "dashed")
  )

ggsave(
  here("figures", "2.2_openness.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 10, unit = "cm"
)

######### END #########
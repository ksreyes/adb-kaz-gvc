# OPENNESS VERSUS ECONOMIC SIZE

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(WDI)
library(tidyverse)
library(ggrepel)

filename <- "2.2_openness"
year <- 2021

highlight <- c(
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


# Data --------------------------------------------------------------------

not_countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel(sheet = "World Bank") |>
  filter(is.na(wb_region)) |> 
  pull(wb_name)

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  select(iso_a3, name) |> 
  mutate(name = replace(name, name == "People's Republic of China", "PRC"))

wb <- WDI(
    indicator = c("trade_sh" = "NE.TRD.GNFS.ZS", "gdp" = "NY.GDP.MKTP.PP.CD"), 
    country = "all", 
    start = year, end = year
  ) |> 
  filter(!(country %in% not_countries)) |> 
  left_join(countries, by = c("iso3c" = "iso_a3")) |> 
  select(year, iso3c, name, trade_sh, gdp)

# Add a Kazakhstan node that excludes its oil trade. The relevant WDI variables
# are the share of merchandise trade in GDP (merch_sh) and the shares of fuel
# exports and imports in merchandise trade (fuelexp_sh and fuelimp_sh). The
# new Kazakhstan node is computed as 

#   trade_sh - merch_sh * (fuelexp_sh + fuelimp_sh)

kaz_adj <- WDI(
    indicator = c(
      "merch_sh" = "TG.VAL.TOTL.GD.ZS", 
      "fuelexp_sh" = "TX.VAL.FUEL.ZS.UN",
      "fuelimp_sh" = "TM.VAL.FUEL.ZS.UN",
      "trade_sh_orig" = "NE.TRD.GNFS.ZS",
      "gdp" = "NY.GDP.MKTP.PP.CD"
    ), 
    country = "KZ", 
    start = year - 10, end = year
  ) |> 
  summarise(across(where(is.numeric), mean)) |> 
  mutate(
    name = "Kazakhstan (exc. oil)",
    trade_sh = trade_sh_orig - merch_sh * (fuelexp_sh + fuelimp_sh) / 100
  ) |> 
  select(name, trade_sh, gdp)

df <- wb |> bind_rows(kaz_adj) |> arrange(name) |> drop_na(c(trade_sh, gdp))

df |> write_csv(here("data", "final", str_glue("{filename}.csv")))


# Plot --------------------------------------------------------------------

df_highlight <- df |> filter(name %in% highlight)
df_select_1 <- df |> filter(name == "Kazakhstan")
df_select_2 <- df |> filter(name == "Kazakhstan (exc. oil)")

plot <- ggplot() +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(trade_sh)),
    data = subset(df, !(name %in% highlight)),
    shape = 16, color = "gray75", size = 2, alpha = .6
  ) +
  geom_smooth(
    mapping = aes(x = log10(gdp), y = log2(trade_sh)),
    data = df,
    method = "glm", formula = y ~ x, se = FALSE,
    show.legend = FALSE, color = "#E88468", linewidth = .5
  ) +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(trade_sh)),
    data = df_highlight,
    shape = 16, color = "#007db7", size = 3, alpha = .6
  ) +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(trade_sh)),
    data = df_select_1,
    shape = 21, fill = "#007db7", color = "black", size = 4
  ) +
  geom_point(
    mapping = aes(x = log10(gdp), y = log2(trade_sh)),
    data = df_select_2,
    shape = 21, fill = "#007db7", color = "black", size = 4
  ) +
  geom_text_repel(
    mapping = aes(x = log10(gdp), y = log2(trade_sh), label = name),
    data = df_highlight,
    point.padding = 5, size = 2.5
  ) +
  geom_text(
    mapping = aes( x = log10(gdp), y = log2(trade_sh), label = name),
    data = df_select_1,
    nudge_x = .05, nudge_y = -.21, size = 3, fontface = "bold"
  ) +
  geom_text(
    mapping = aes(x = log10(gdp), y = log2(trade_sh), label = name),
    data = df_select_2,
    nudge_x = .1, hjust = 0, size = 3, fontface = "bold"
  ) +
  scale_x_continuous(
    name = "GDP ($ PPP)",
    limits = c(log10(9 ^ 10), log10(max(df$gdp))),
    breaks = c(log10(10 ^ 10), log10(10 ^ 11), log10(10 ^ 12), log10(10 ^ 13)),
    label = c("$10 billion", "$100 billion", "$1 trillion", "$10 trillion")
  ) +
  scale_y_continuous(
    name = "Trade (% of GDP)",
    limits = c(log2(15), log2(250)),
    breaks = c(log2(25), log2(50), log2(100), log2(200)),
    label = function(x) str_c(2 ^ x, "%")
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
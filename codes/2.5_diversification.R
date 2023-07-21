# TRADE DIVERSIFICATION

rm(list = ls())
library(here)
library(tidyverse)
library(DBI)
library(duckdb)

party <-
  c(
    "Saudi Arabia",
    "Kyrgyz Republic",
    "Kazakhstan",
    "Uzbekistan",
    "Russian Federation"
  )

countries <- here("..", "..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  select(iso_num, code, name, region) %>% 
  filter(name %in% party)

products <- here("..", "..", "mrio-processing", "data", "external", "product_codes_HS02_V202301.csv") %>% 
  read_csv() %>%
  mutate(hs02_2 = substr(code, 1, 2),
         hs02_4 = substr(code, 1, 4)) %>% 
  select(hs02_2, hs02_4, hs02 = code, hs02_name = description) 

years <- c(2005, 2010, 2015, 2021)

# DATA ----

get_hhi <- function(baci, country) {
  baci %>%
    filter(i == countries %>% slice(country) %>% pull(iso_num)) %>%
    left_join(products, by = c("k" = "hs02")) %>%
    left_join(countries, by = c("i" = "iso_num")) %>%
    group_by(t, i, name, hs02_4) %>%
    summarize(v = sum(v)) %>%
    ungroup() %>%
    mutate(
      share = v / sum(v),
      share2 = share ^ 2
    ) %>%
    group_by(name, t) %>%
    summarize(hhi = sum(share2)) %>%
    ungroup()
}

df <- tibble()

for (t in 1:length(years)) {
  
  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  
  baci <- con %>% 
    dbGetQuery(sprintf(
      "SELECT * 
      FROM read_parquet('../../mrio-processing/data/external/BACI_HS02_V202301.parquet') 
      WHERE t = %d",
      years[t]
    )) %>% 
    mutate(k = as.character(k))
  
  dbDisconnect(con, shutdown=TRUE)
  
  df <-
    rbind(
      df,
      get_hhi(baci, 1),
      get_hhi(baci, 2),
      get_hhi(baci, 3),
      get_hhi(baci, 4),
      get_hhi(baci, 5)
    )
}

dfout <- df %>% 
  pivot_wider(names_from = name, values_from = hhi)

write_csv(dfout, here("data", "final", "2.5_diversification.csv"))

# PLOT ----

df <- df %>% 
  mutate(
    t = factor(t, levels = years),
    name = factor(name, levels = party)
  )

plot <- ggplot(df, aes(x = name, y = hhi, fill = t)) +
  geom_bar(
    stat = "identity", position = position_dodge(width = .8), width = .7
  ) +
  geom_hline(yintercept = 0, linewidth = .25, color = "gray25") +
  scale_fill_manual(values = rev(c(
    "#007db7", "#00A5D2", "#63CCEC", "#9EE4FF"
  ))) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    plot.margin = margin(8, 2, 13, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      size = c(9, 9, 10, 9, 9),
      color = "black",
      face = c("plain", "plain", "bold", "plain", "plain"),
      margin = margin(-5, 0, 0, 0)
    ),
    axis.text.y = element_text(size = 9),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 9, margin = margin(0, 5, 0, 0)),
    legend.position = c(.75, .90),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray75", linewidth = .25, linetype = "dashed")
  )

ggsave(
  here("figures", "2.5_diversification.pdf"),
  plot,
  device = cairo_pdf,
  width = 16, height = 10, unit = "cm"
)

######### END #########
# EXPORTS OF GOODS BY BROAD PRODUCT CATEGORIES

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(readxl)
library(tidyverse)
library(arrow)
library(DBI)
library(duckdb)

filename <- "2.4_products"

select <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |> 
  filter(name == "Kazakhstan") |> 
  pull(iso_num)

# Choose products to highlight

products <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel(sheet = "hs02_mrio") |>
  select(hs02:section_name) |> 
  mutate(
    hs02 = as.numeric(hs02),
    group = case_when(
      section %in% 1:4              ~ "Food",
      hs02 == 270900                ~ "Minerals: crude oil",
      hs02 %in% c(271111, 271121)   ~ "Minerals: natural gas",
      between(hs02, 260111, 261790) ~ "Minerals: ores",
      section == 5                  ~ "Minerals: others",
      section == 6                  ~ "Chemicals",
      between(hs02, 720211, 720299) ~ "Metals: ferroalloys",
      between(hs02, 740110, 741999) ~ "Metals: copper",
      section == 15                 ~ "Metals: others",
      .default                      = "Others"
  ))

groups <- tribble(
  ~name,                    ~labs,                         ~color,     ~color_key,
  "Food",                   "1    Food",                   "#007DB7",  NA,
  "blank1",                 " ",                           NA,         NA,
  "Minerals: crude oil",    "     Minerals: crude oil",    "#E9532B",  "#E9532B",
  "Minerals: natural gas",  "     Minerals: natural gas",  "#F57F29",  "#F57F29",
  "Minerals: ores",         "     Minerals: ores",         "#FDB415",  "#FDB415",
  "Minerals: others",       "     Minerals: others",       "#F2E600",  "#F2E600",
  "blank2",                 " ",                           NA,         NA,
  "Chemicals",              "2    Chemicals",              "#0099D8",  NA,
  "Metals: ferroalloys",    "3    Metals: ferroalloys",    "#0088C7",  NA,
  "Metals: copper",         "4    Metals: copper",         "#00A5D2",  NA,
  "Metals: others",         "5    Metals: others",         "#6DBCE3",  NA,
  "Others",                 "6    Others",                 "#6DCFF6",  NA
)

# Data ------------------------------------------------------------

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

baci <- dbGetQuery(con, sprintf(
    "SELECT * 
    FROM '../../../Repos/baci/final/BACI_HS02_V202301.parquet'
    WHERE i = %s",
    select
  ))

dbDisconnect(con, shutdown = TRUE)
  
baci |> mutate(k = as.numeric(k)) |> 
  left_join(products, by = c("k" = "hs02")) |>
  summarize(v = sum(v), .by = c(t, i, group)) |>
  mutate(share = v / sum(v)) |>
  select(t, group, v) |> 
  pivot_wider(names_from = group, values_from = v) |>
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
  ) |> 
  replace_na(list(`Minerals: natural gas` = 0)) |>
  rowwise(t) |>
  mutate(Total = sum(c_across(Food:Others))) |>
  arrange(desc(t)) |> 
  write_csv(here("data", "final", str_glue("{filename}.csv")))

# Plot --------------------------------------------------------------------

df <- here("data", "final", str_glue("{filename}.csv")) |> 
  read_csv() |>
  select(-Total) |> 
  pivot_longer(cols = !t, names_to = "group", values_to = "v") |> 
  add_row(t = 2010, group = c("blank1", "blank2"), v = 0)

labels <- groups |>
  left_join(subset(df, t == 2021), by = c("name" = "group")) |>
  mutate(
    label = str_sub(labs, 1, 1),
    x1 = cumsum(coalesce(v, 0) / sum(coalesce(v, 0))),
    x = (x1 + lag(x1, default = 0)) / 2
  )

plot <- ggplot() +
  geom_vline(xintercept = .75, linewidth = .25, linetype = "dashed", color = "gray25") +
  geom_bar(
    aes(x = v, y = factor(t), fill = factor(group, rev(groups$name))),
    df, stat = "identity", position = "fill", width = .7
  ) +
  geom_vline(xintercept = .25, linewidth = .25, linetype = "dashed", color = "gray25") +
  geom_text(aes(x = x, label = label), labels, y = 20, size = 2.5) +
  scale_x_continuous(breaks = c(.25, .75), labels = c("25%", "75%")) +
  scale_fill_manual(labels = rev(groups$labs), values = rev(groups$color)) +
  guides(fill = guide_legend(
    reverse = TRUE,
    override.aes = list(fill = groups$color_key)
  )) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9, margin = margin(r = -10)),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 8, margin = margin(l = -13)),
    legend.title = element_blank(),
    legend.position = "right",
    legend.box.margin = margin(l = -25),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    plot.margin = margin(15, 2, 12, 2)
  )

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 12, unit = "cm"
)

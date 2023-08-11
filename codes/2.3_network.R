# TRADE NETWORK

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(tidyverse)
library(DBI)
library(duckdb)
library(scales)
library(tidygraph)
library(ggraph)

filename <- "2.3_network"

select <- "Kazakhstan"

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel(sheet = "BACI") |>
  drop_na(iso_num) |> 
  left_join(
    here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
      read_excel(sheet = "Consolidated")
  ) |> 
  select(baci_num, code, name, region) 

# Data --------------------------------------------------------------------

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

df <- dbGetQuery(con, 
    "SELECT t, i, j, sum(v) AS value 
    FROM '../../../Repos/baci/final/BACI_HS02_V202301.parquet'
    WHERE t = 2021
    GROUP BY t, i, j"
  )

dbDisconnect(con, shutdown = TRUE)

# Rank all of i's trading partners. Network only shows nodes that are among the 
# top 3 of another economy's trading partners

df1 <- df |>
  mutate(rank = rank(desc(value)), .by = i) |> 
  mutate(weight = ifelse(rank %in% 1:3, value, 0))

links <- df1 |>
  filter(weight > 0) |>
  summarise(links = n(), .by = j) |>
  rename(baci_num = j)

countries <- left_join(countries, links)

# Create node list

sources <- df |> distinct(i) |> rename(baci_num = i)
destinations <- df |> distinct(j) |> rename(baci_num = j)

nodes <- full_join(sources, destinations) |> 
  inner_join(countries) |>
  replace_na(list(links = 0)) |> 
  select(baci_num, code, name, region, links) |> 
  drop_na()

# Create edge list

edges <- df1 |> 
  filter(weight > 0) |>
  left_join(nodes, by = c("i" = "baci_num")) |> 
  rename(from = code) |> 
  left_join(nodes, by = c("j" = "baci_num")) |>
  rename(to = code) |> 
  select(from, to, weight) |> 
  drop_na()

nodes |> write_csv(here("data", "final", str_glue("{filename}_nodes.csv")))
edges |> write_csv(here("data", "final", str_glue("{filename}_edges.csv")))

# PLOT ----

network <- tbl_graph(nodes = nodes, node_key = "code", edges = edges)

set.seed(7168)  # Discovered through trial and error

plot <- ggraph(network, layout = "dh") +
  geom_edge_link(aes(alpha = weight, width = weight), show.legend = FALSE) +
  geom_node_point(
    aes(color = region),
    size = rescale(nodes$links, c(.5, 20)),
    shape = 16, alpha = 0.8
  ) +
  geom_node_point(
    shape = 21, fill = "#8dc63f", color = "black", stroke = .5,
    size = rescale(nodes$links, c(.5, 20)),
    alpha = ifelse(nodes$name == select, 1, 0)
  ) +
  geom_node_text(
    aes(label = ifelse(links > 7 & name != select, code, "")), 
    size = 2
  ) +
  geom_node_text(
    aes(label = ifelse(name == select, select, "")),
    size = 3, fontface = "bold",
    nudge_x = 3, nudge_y = 4
  ) +
  scale_edge_alpha(range = c(0.1, 1)) +
  scale_edge_width(range = c(0.2, 2)) +
  scale_color_manual(
    values = c(
      "#E9532B",    # East Asia & Pacific
      "#8dc63f",    # Europe & Central Asia
      "#63CCEC",    # Latin America & the Carribean
      "#FDB415",    # Middle East & North Africa
      "#007DB7",    # North America
      "#C8DA2B",    # South Asia
      "#F57F29"     # Sub-Saharan Africa
    )) +   
  guides(color = guide_legend(override.aes = list(size = 2.5, alpha = 1))) +
  theme(
    legend.key = element_blank(),
    legend.key.size = unit(0.75, "lines"),
    legend.text = element_text(size = 8, margin = margin(r = 5)),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box.margin = margin(t = -20),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(0, 2, 10, 2)
  )

ggsave(
  here("figures", str_glue("{filename}.pdf")),
  device = cairo_pdf, width = 16, height = 12, unit = "cm"
)

# Appendix ----------------------------------------------------------------

# Look for a nice graph

for (i in 1:10) {
  
  seed <- floor(runif(1, 1, 9999))
  set.seed(seed)
  
  plot <- ggraph(network, layout = "dh") +
    geom_edge_link(aes(alpha = weight, width = weight), show.legend = FALSE) +
    geom_node_point(
      aes(color = region),
      size = rescale(nodes$links, c(.5, 20)),
      shape = 16, alpha = 0.8
    ) +
    geom_node_point(
      shape = 21, fill = "#8dc63f", color = "black", stroke = .5,
      size = rescale(nodes$links, c(.5, 20)),
      alpha = ifelse(nodes$name == select, 1, 0)
    ) +
    geom_node_text(
      aes(label = ifelse(links > 7 & name != select, code, "")), 
      size = 2
    ) +
    geom_node_text(
      aes(label = ifelse(name == select, select, "")),
      size = 3, fontface = "bold",
      nudge_x = 3, nudge_y = 4
    ) +
    scale_edge_alpha(range = c(0.1, 1)) +
    scale_edge_width(range = c(0.2, 2)) +
    scale_color_manual(
      values = c(
        "#E9532B",        # East Asia & Pacific
        "#8dc63f",        # Europe & Central Asia
        "#63CCEC",        # Latin America & the Carribean
        "#FDB415",        # Middle East & North Africa
        "#007DB7",        # North America
        "#C8DA2B",        # South Asia
        "#F57F29"         # Sub-Saharan Africa
      )) +   
    guides(color = guide_legend(override.aes = list(size = 2.5, alpha = 1))) +
    theme(
      legend.key = element_blank(),
      legend.key.size = unit(0.75, "lines"),
      legend.text = element_text(size = 8, margin = margin(r = 5)),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(t = -20),
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(0, 2, 10, 2)
    )
  
  ggsave(
    here("figures", "figure-drafts", str_glue("network_{seed}.pdf")),
    device = cairo_pdf, width = 16, height = 12, unit = "cm"
  )
}

# TRADE NETWORK

rm(list = ls())
library(tidyverse)
library(scales)
library(tidygraph)
library(ggraph)

countries <- read_excel("data/interim/countries.xlsx") %>%
  select(iso_num, code, name, region)
countries$code[which(countries$name == "Other Asia, not elsewhere specified")] <-
  "TAP"

# DATA ----

df <- read_csv("data/raw/BACI_HS02_V202301/BACI_HS02_Y2021_V202301.csv")

# Rank all of i's trading partners. Network only shows nodes that are among the 
# top 3 of another economy's trading partners

df1 <- df %>%
  group_by(i, j) %>%
  summarise(value = sum(v)) %>% 
  ungroup() %>%
  group_by(i) %>%
  mutate(rank = rank(desc(value))) %>% 
  ungroup() %>%
  mutate(weight = ifelse(rank %in% 1:3, value, 0))

links <- df1 %>%
  filter(weight > 0) %>%
  group_by(j) %>%
  summarise(links = n()) %>%
  rename(iso_num = j)

countries <- countries %>%
  left_join(links)

# Create node list

sources <- df %>%
  distinct(i) %>%
  rename(iso_num = i)

destinations <- df %>%
  distinct(j) %>%
  rename(iso_num = j)

nodes <- full_join(sources, destinations) %>% 
  inner_join(countries) %>%
  rowid_to_column("id") %>%
  replace(is.na(.), 0)

# Create edge list

edges <- df1 %>% 
  filter(weight > 0) %>%
  left_join(nodes, by = c("i" = "iso_num")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("j" = "iso_num")) %>%
  rename(to = id)

edges <- select(edges, from, to, weight)

# PLOT ----

network <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

set.seed(4949)

plot <- ggraph(network, layout = "dh") +
  geom_edge_link(aes(alpha = weight, width = weight), show.legend = FALSE) +
  geom_node_point(
    aes(color = region),
    size = rescale(nodes$links, c(.5, 20)),
    shape = 16,
    alpha = 0.8
  ) +
  geom_node_point(
    shape = 21,
    fill = "#8dc63f",
    color = "black",
    stroke = .5,
    size = rescale(nodes$links, c(.5, 20)),
    alpha = ifelse(nodes$name == "Kazakhstan", 1, 0)
  ) +
  geom_node_text(
    aes(label = ifelse(links > 7 & name != "Kazakhstan", code, "")), 
    size = 2
  ) +
  geom_node_text(
    aes(label = ifelse(name == "Kazakhstan", "Kazakhstan", "")),
    size = 3,
    fontface = "bold",
    nudge_x = 3,
    nudge_y = 4
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
  theme_graph(
    base_family = "Arial",
    plot_margin = margin(-10, 0, 0, 0)
  ) +
  theme(
    plot.margin = margin(0, 2, 10, 2),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, margin = margin(0, 5, 0, 0)),
    legend.key = element_blank(),
    legend.key.size = unit(0.75, "lines"),
    legend.position = "bottom",
    legend.box.margin = margin(-20, 0, 0, 0)
  )

ggsave("figures/2.3_network.pdf", plot, device = cairo_pdf, 
      width = 16, height = 12, unit = "cm")

#ggsave("figures/2.3_network.png", plot, 
#       dpi = 300, width = 16, height = 12, unit = "cm")


# Look for a nice graph

for (i in 1:10) {
  
  seed <- floor(runif(1, 1, 9999))
  set.seed(seed)
  
  plot <- ggraph(network, layout = "dh") +
    geom_edge_link(aes(alpha = weight, width = weight), show.legend = FALSE) +
    geom_node_point(
      aes(color = region),
      size = rescale(nodes$links, c(.5, 20)),
      shape = 16,
      alpha = 0.8
    ) +
    geom_node_point(
      shape = 21,
      fill = "#8dc63f",
      color = "black",
      stroke = .5,
      size = rescale(nodes$links, c(.5, 20)),
      alpha = ifelse(nodes$name == "Kazakhstan", 1, 0)
    ) +
    geom_node_text(
      aes(label = ifelse(links > 7 & name != "Kazakhstan", code, "")), 
      size = 2
    ) +
    geom_node_text(
      aes(label = ifelse(name == "Kazakhstan", "Kazakhstan", "")),
      size = 3,
      fontface = "bold",
      nudge_x = 3,
      nudge_y = 4
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
    theme_graph(
      base_family = "Arial",
      plot_margin = margin(-10, 0, 0, 0)
    ) +
    theme(
      plot.margin = margin(0, 2, 10, 2),
      legend.title = element_blank(),
      legend.text = element_text(size = 8, margin = margin(0, 5, 0, 0)),
      legend.key = element_blank(),
      legend.key.size = unit(0.75, "lines"),
      legend.position = "bottom",
      legend.box.margin = margin(-20, 0, 0, 0)
    )
  
  ggsave(paste0("figures/network_drafts/", seed, "_network.pdf"), plot, 
         device = cairo_pdf, width = 16, height = 12, unit = "cm")
}

######### END #########
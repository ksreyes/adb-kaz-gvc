# REER WEIGHTS

# Setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(arrow)
library(readxl)
library(tidyverse)

filename <- "4.3_reerw"
select <- "Kazakhstan"
display <- c("Mining", "Metals")
N <- 35

sectors <- here("..", "..", "MRIO Processing", "dicts", "sectors.xlsx") |> 
  read_excel() |> 
  distinct(ind, name_short) |> 
  rename(i = ind, sector = name_short)

countries <- here("..", "..", "MRIO Processing", "dicts", "countries.xlsx") |> 
  read_excel() |>
  mutate(region = case_when(
    str_detect(region, "America") ~ "Americas",
    str_detect(region, "Middle East") ~ "Rest of the world",
    is.na(region) ~ "Rest of the world",
    .default = region
  )) |> 
  drop_na(mrio) |> 
  select(mrio, mrio62, name, region)

select_ind <- countries |> filter(name == "Kazakhstan") |> pull(mrio)
display_ind <- sectors |> filter(sector %in% display) |> pull(i)

# Data --------------------------------------------------------------------

reshape_matrix <- function(matrix, G, select_ind, display_ind) {
  
  # Reshapes weights matrix into long format and retains only the rows
  # corresponding to select_ind and display_ind
  
  tlen <- as.integer(nrow(matrix) / (G * N))
  selectlen <- length(select_ind) * length(display_ind)
  
  matrix |>
    mutate(s = rep(1:G, each = N) |> rep(tlen), i = rep(1:N, G) |> rep(tlen)) |>
    filter(s == select_ind & i %in% display_ind) |> 
    pivot_longer(cols = -c(t, s, i), names_to = "rj", values_to = "weight") |>
    mutate(r = rep(1:G, each = N) |> rep(selectlen * tlen)) |>
    summarise(weight = sum(weight), .by = c(t, s, i, r)) |>
    filter(r != select_ind) |>
    mutate(weight = weight / sum(weight), .by = c(t, s, i)) |>
    select(t, s, i, r, weight)
}

reduce_groups <- function(
    df, factor, value, group, show = 2, by = "mean", 
    subgroup = NULL, always_agg = NULL
  ) {
  
  # Reduce number of countries to display in chart by taking the top 2 within
  # each region and aggregating the rest under "Others"
  
  if (by == "mean") { 
    tags <- df |> group_by({{ group }}, {{ subgroup }}) |> 
      summarise("{{ value }}" := mean({{ value }})) 
  }
  else { 
    tags <- df |> filter({{ factor }} == by) |> 
      group_by({{ group }})
  }
  
  tags <- tags |>
      arrange(desc(weight), .by_group = TRUE) |>
      mutate(rank = 1:n(), n = n())

  if (missing(subgroup)) {
    tags <- mutate(tags, reduced_group = case_when(
        {{ group }} %in% always_agg ~ {{ group }},
        (n == show + 1) & (rank > show) ~ {{ group }},
        rank <= show ~ {{ group }},
        .default = "Others"
      ))
  } else {
    tags <- mutate(tags, reduced_group = case_when(
        {{ group }} %in% always_agg ~ {{ group }},
        (n == show + 1) & (rank > show) ~ {{ subgroup }},
        rank <= show ~ {{ subgroup }},
        .default = str_c("Other ", {{ group }})
      ))
  }
  
  tags <- select(tags, {{ subgroup }}, {{ group }}, reduced_group)

  df |> left_join(tags) |>
    select({{ factor }}, {{ subgroup }}, {{ group }}, reduced_group, {{ value }}) |>
    mutate(reduced_group = fct(reduced_group, levels = unique(tags$reduced_group))) |> 
    group_by({{ factor }}, reduced_group) |> 
    summarise("{{ value }}" := sum({{ value }})) |> 
    arrange(reduced_group, .by_group = TRUE)
}

df <- here("..", "..", "MRIO Processing", "data", "reer", "reer62-weights-sector.parquet") |> 
  read_parquet() |> 
  reshape_matrix(G = 63, select_ind, display_ind) |> 
  filter(t < 2017) |> 
  left_join(countries, by = c("r" = "mrio62")) |> 
  bind_rows(
    here("..", "..", "MRIO Processing", "data", "reer", "reer-weights-sector.parquet") |> 
      read_parquet() |> 
      reshape_matrix(G = 73, select_ind, display_ind) |> 
      left_join(countries, by = c("r" = "mrio"))
  ) |> 
  select(i, t, region, name, weight)

df |> filter(i == display_ind[1]) |>   
  reduce_groups(
    factor = t, 
    value = weight, 
    group = region, 
    subgroup = name, 
    always_agg = c("South Asia", "Rest of the world")
  ) |> 
  mutate(
    country = select, 
    sector = display[1], 
    competitor = fct_relevel(reduced_group, "Rest of the world", after = Inf)
  ) |> 
  group_by(t) |>
  arrange(competitor, .by_group = TRUE) |> 
  pivot_wider(names_from = t, values_from = weight) |> 
  select(country, sector, competitor, starts_with("20")) |> 
  write_csv(here("data", "final", str_glue("{filename}_a.csv")))

df |> filter(i == display_ind[2]) |>   
  reduce_groups(
    factor = t, 
    value = weight, 
    group = region, 
    subgroup = name, 
    always_agg = c("South Asia", "Rest of the world")
  ) |> 
  mutate(
    country = select, 
    sector = display[2], 
    competitor = fct_relevel(reduced_group, "Rest of the world", after = Inf)) |> 
  group_by(t) |> 
  arrange(competitor, .by_group = TRUE) |> 
  pivot_wider(names_from = t, values_from = weight) |> 
  select(country, sector, competitor, starts_with("20")) |> 
  write_csv(here("data", "final", str_glue("{filename}_b.csv")))

# Plot --------------------------------------------------------------------

hline <- geom_hline(
    yintercept = c(.25, .75), 
    size = .25, color = "gray35", linetype = "dashed"
  )
xscale <- scale_x_discrete(labels = c(2000, "", "07", "08", "09", 10:22))
yscale <- scale_y_continuous(
    breaks = c(.25, .75), 
    labels = function(x) str_c(100 * x, "%")
  )
colors <- scale_fill_manual(values = c(
    "#007db7", "#00A5D2", "#63CCEC", 
    "#FDB415", "#FCD379", "#FFE4A8",
    "#6BB305", "#8DC63F", "#B4DE7A",
    "#E9532B", "gray75"
  ))
theme <- theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 9, margin = margin(t = -8)),
    axis.text.y = element_text(size = 9),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.position = "right",
    legend.box.margin = margin(l = -7),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(5, 2, 15, 2)
  )

df1 <- here("data", "final", str_glue("{filename}_a.csv")) |> read_csv()
df2 <- here("data", "final", str_glue("{filename}_b.csv")) |> read_csv()

plot1 <- df1 |> 
  pivot_longer(cols = starts_with("20"), names_to = "t", values_to = "weight") |> 
  add_row(t = "2001") |> 
  ggplot(aes(x = factor(t), y = weight, fill = fct(competitor, levels = df1$competitor))) +
  hline + xscale + yscale + colors + theme +
  geom_bar(stat = "identity", position = "stack", width = .7)

ggsave(
  here("figures", str_glue("{filename}_a.pdf")),
  device = cairo_pdf, width = 16, height = 9, unit = "cm"
)

plot2 <- df2 |> 
  pivot_longer(cols = starts_with("20"), names_to = "t", values_to = "weight") |> 
  add_row(t = "2001") |> 
  ggplot(aes(x = factor(t), y = weight, fill = fct(competitor, levels = df2$competitor))) +
  hline + xscale + yscale + colors + theme +
  geom_bar(stat = "identity", position = "stack", width = .7)

ggsave(
  here("figures", str_glue("{filename}_b.pdf")),
  device = cairo_pdf, width = 16, height = 9, unit = "cm"
)

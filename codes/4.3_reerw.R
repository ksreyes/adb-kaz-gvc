# REER WEIGHTS

# SET UP ----

rm(list = ls())
library(here)
library(arrow)
library(tidyverse)

sectors <- here("..", "..", "mrio-processing", "data", "raw", "sectors.xlsx") %>% 
  read_excel() %>% 
  group_by(ind, name_short) %>%
  distinct(ind) %>%
  ungroup() %>% 
  bind_rows(tibble(ind = 0, name_short = "Aggregate"))

focusnames <- c("Mining", "Metals")
focus <- sectors %>% filter(name_short %in% focusnames) %>% pull(ind)

countries <- here("..", "..", "mrio-processing", "data", "raw", "countries.xlsx") %>% 
  read_excel() %>%
  filter(!(is.na(mrio))) %>% 
  mutate(s = mrio) %>% 
  arrange(s) %>%
  select(s, name, region) %>% 
  mutate(
    region = replace(region, region == "Latin America and the Caribbean", "Rest of the world"),
    region = replace(region, region == "Middle East and North Africa", "Rest of the world"),
    region = replace(region, name == "Mexico", "North America"),
    region = replace(region, name == "Rest of the world", "Rest of the world")
  )

select <- countries %>% filter(name == "Kazakhstan") %>% pull(s)

years72 <- 2017:2022
years62 <- 2007:2016
G <- 73
G62 <- 63
N <- 35

# LOAD DATA ----

norm <- function(vector, s) {
  vector1 <- -vector[-s] / sum(vector[-s])
  c(vector1[1:(s - 1)], -sum(vector1), vector1[s:length(vector1)])
}

w_A <- w_B <- w62_A <- w62_B <- tibble()

for (year in years72) {
  
  w_t <- here("..", "..", "mrio-processing", "data", "reer", "reer-weights-sector.parquet") %>% 
    read_parquet() %>% 
    filter(t == year) %>% 
    select(-t)
  
  # Mining
  slice <- tibble(s = rep(1:G, each = N), i = rep(1:N, G), w_t) %>% 
  filter(s == select & i == focus[1])
  
  w_A <- rbind(
      w_A,
      tibble(
        i = focusnames[1],
        t = year,
        s = rep(1:G, each = N), 
        j = rep(1:N, G),
        w_big = as.numeric(slice[, -(1:2)])
      ) %>% 
      group_by(i, t, s) %>% 
      summarize(w1 = sum(w_big)) %>% 
      ungroup() %>% 
      mutate(w = norm(w1, select)) %>% 
      select(i, t, s, w)
    )
  
  # Metals
  slice <- tibble(s = rep(1:G, each = N), i = rep(1:N, G), w_t) %>% 
    filter(s == select & i == focus[2])
  
  w_B <- rbind(
      w_B,
      tibble(
        i = focusnames[2],
        t = year,
        s = rep(1:G, each = N), 
        j = rep(1:N, G),
        w_big = as.numeric(slice[, -(1:2)])
      ) %>% 
      group_by(i, t, s) %>% 
      summarize(w1 = sum(w_big)) %>% 
      ungroup() %>% 
      mutate(w = norm(w1, select)) %>% 
      select(i, t, s, w)
    )
}

for (year in years62) {
  
  w62_t <- here("..", "..", "mrio-processing", "data", "reer", "reer62-weights-sector.parquet") %>% 
    read_parquet() %>% 
    filter(t == year) %>% 
    select(-t)
  
  # Mining
  slice <- tibble(s = rep(c(1:(G62-1), 73), each = N), i = rep(1:N, G62), w62_t) %>% 
    filter(s == select & i == focus[1])
  
  w62_A <- rbind(
    w62_A,
    tibble(
      i = focusnames[1],
      t = year,
      s = rep(c(1:(G62-1), 73), each = N), 
      j = rep(1:N, G62),
      w_big = as.numeric(slice[, -(1:2)])
    ) %>% 
      group_by(i, t, s) %>% 
      summarize(w1 = sum(w_big)) %>% 
      ungroup() %>% 
      mutate(w = norm(w1, select)) %>% 
      select(i, t, s, w)
  )
  
  # Metals
  slice <- tibble(s = rep(c(1:(G62-1), 73), each = N), i = rep(1:N, G62), w62_t) %>% 
    filter(s == select & i == focus[2])
  
  w62_B <- rbind(
    w62_B,
    tibble(
      i = focusnames[2],
      t = year,
      s = rep(c(1:(G62-1), 73), each = N), 
      j = rep(1:N, G62),
      w_big = as.numeric(slice[, -(1:2)])
    ) %>% 
      group_by(i, t, s) %>% 
      summarize(w1 = sum(w_big)) %>% 
      ungroup() %>% 
      mutate(w = norm(w1, select)) %>% 
      select(i, t, s, w)
  )
}

df_A <- bind_rows(w62_A, w_A) %>% 
  left_join(countries) %>% 
  pivot_wider(names_from = t, values_from = w)

df_B <- bind_rows(w62_B, w_B) %>% 
  left_join(countries) %>% 
  pivot_wider(names_from = t, values_from = w)

write_csv(df_A, here("data", "final", "4.3a_reerw.csv"))
write_csv(df_B, here("data", "final", "4.3b_reerw.csv"))

# PLOT ----

segment <- function(df) {
  
  df <- df %>% filter(s != select) %>% select(!s)
  df <- df %>% replace(is.na(.), 0)
  
  # East Asia and Pacific
  
  df_eap <- df %>% 
    filter(region == "East Asia and Pacific") %>% 
    arrange(across(last_col()))
  
  resid_eap <- colSums(df_eap[, -(1:3)]) - colSums(df_eap[1:2, -(1:3)])
  
  df_eap <- df_eap %>% 
    slice(1:2) %>% 
    pivot_longer(cols = !(i:region), names_to = "t", values_to = "w") %>% 
    bind_rows(tibble(
      i = df$i[1],
      name = "Other East Asia and Pacific",
      region = "East Asia and Pacific",
      t = as.character(c(years62, years72)),
      w = resid_eap
    )) %>% 
    pivot_wider(names_from = t, values_from = w)
  
  # Europe and Central Asia
  
  df_eca <- df %>% 
    filter(region == "Europe and Central Asia") %>% 
    arrange(across(last_col()))
  
  resid_eca <- colSums(df_eca[, -(1:3)]) - colSums(df_eca[1:2, -(1:3)])
  
  df_eca <- df_eca %>% 
    slice(1:2) %>% 
    pivot_longer(cols = !(i:region), names_to = "t", values_to = "w") %>% 
    bind_rows(tibble(
      i = df$i[1],
      name = "Other Europe and Central Asia",
      region = "Europe and Central Asia",
      t = as.character(c(years62, years72)),
      w = resid_eca
    )) %>% 
    pivot_wider(names_from = t, values_from = w)
  
  # North America
  
  df_na <- df %>% 
    filter(region == "North America") %>% 
    arrange(across(last_col()))
  
  resid_na <- colSums(df_na[, -(1:3)]) - colSums(df_na[1, -(1:3)])
  
  df_na <- df_na %>% 
    slice(1) %>% 
    pivot_longer(cols = !(i:region), names_to = "t", values_to = "w") %>% 
    bind_rows(tibble(
      i = df$i[1],
      name = "Other North America",
      region = "North America",
      t = as.character(c(years62, years72)),
      w = resid_na
    )) %>% 
    pivot_wider(names_from = t, values_from = w)
  
  # South Asia
  
  df_sa <- df %>% filter(region == "South Asia")
  total_sa <- colSums(df_sa[, -(1:3)])
  
  df_sa <- bind_rows(tibble(
      i = df$i[1],
      name = "South Asia",
      region = "South Asia",
      t = as.character(c(years62, years72)),
      w = total_sa
    )) %>% 
    pivot_wider(names_from = t, values_from = w)
  
  # Combine
  
  df_all <- bind_rows(df_eap, df_eca, df_na, df_sa)
  order <- c(df_all$name, "Rest of the world")
  
  resid_all <- colSums(df[, -(1:3)]) - colSums(df_all[, -(1:3)])
  
  df_all <- df_all %>% 
    pivot_longer(cols = !(i:region), names_to = "t", values_to = "w") %>% 
    bind_rows(tibble(
      i = df$i[1],
      name = "Rest of the world",
      region = "Rest of the world",
      t = as.character(c(years62, years72)),
      w = resid_all
    )) %>% 
    mutate(name = factor(name, levels = order), w = -w)
  
  return(df_all)
}

# .... Mining ----

dfplot_A <- segment(df_A) %>% 
  mutate(t = factor(t, levels = c(years62, years72)))

plot_A <- ggplot(dfplot_A, aes(x = t, y = w, fill = name)) + 
  geom_hline(yintercept = .25, size = .25, color = "gray35", linetype = "dashed") + 
  geom_hline(yintercept = .75, size = .25, color = "gray35", linetype = "dashed") + 
  geom_rect(
    data = dfplot_A[1, ],
    xmin = 11, xmax = Inf, ymin = 0, ymax = 1, fill = "white"
  ) + 
  geom_bar(stat = "identity", position = "stack", width = .7) +
  
  scale_fill_manual(values = c(
    "#007db7", "#00A5D2", "#63CCEC", 
    "#6BB305", "#8DC63F", "#B4DE7A",
    "#E9532B", "#E88468", "#FDB415", 
    "gray75"
  )) + 
  scale_x_discrete(labels = c(2007, "08", "09", 10:22)) +
  scale_y_continuous(breaks = c(.25, .75), labels = function(x) paste0(100 * x, "%")) +
  theme(
    plot.margin = margin(5, 2, 15, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9, margin = margin(-8, 0, 0, 0)),
    axis.text.y = element_text(size = 9),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.position = "right",
    legend.box.margin = margin(0, 0, 0, -7),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  here("figures", "4.3a_reerw.pdf"),
  plot_A,
  device = cairo_pdf,
  width = 16, height = 9, unit = "cm"
)

# .... Metals ----

dfplot_B <- segment(df_B) %>% 
  mutate(t = factor(t, levels = c(years62, years72)))

plot_B <- ggplot(dfplot_B, aes(x = t, y = w, fill = name)) + 
  geom_hline(yintercept = .25, size = .25, color = "gray35", linetype = "dashed") + 
  geom_hline(yintercept = .75, size = .25, color = "gray35", linetype = "dashed") + 
  geom_rect(
    data = dfplot_B[1, ],
    xmin = 11, xmax = Inf, ymin = 0, ymax = 1, fill = "white"
  ) + 
  geom_bar(stat = "identity", position = "stack", width = .7) +
  
  scale_fill_manual(values = c(
    "#007db7", "#00A5D2", "#63CCEC", 
    "#6BB305", "#8DC63F", "#B4DE7A",
    "#E9532B", "#E88468", "#FDB415", 
    "gray75"
  )) + 
  scale_x_discrete(labels = c(2007, "08", "09", 10:22)) +
  scale_y_continuous(breaks = c(.25, .75), labels = function(x) paste0(100 * x, "%")) +
  theme(
    plot.margin = margin(5, 2, 15, 2),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9, margin = margin(-8, 0, 0, 0)),
    axis.text.y = element_text(size = 9),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key = element_blank(),
    legend.key.size = unit(.75, "lines"),
    legend.position = "right",
    legend.box.margin = margin(0, 0, 0, -7),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  here("figures", "4.3b_reerw.pdf"),
  plot_B,
  device = cairo_pdf,
  width = 16, height = 9, unit = "cm"
)

######### END #########
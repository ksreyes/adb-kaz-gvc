# TRADE DIVERSIFICATION

rm(list = ls())
library(tidyverse)

countries <- read_excel("data/interim/countries.xlsx") %>%
  select(iso_num, code, name, region)
countries$code[which(countries$name == "Other Asia, not elsewhere specified")] <- "TAP"

products <- read_csv("data/raw/BACI_HS02_V202301/bacikey_products.csv") %>%
  mutate(hs02_2 = substr(hs02, 1, 2),
         hs02_4 = substr(hs02, 1, 4)) %>% 
  select(hs02_2, hs02_4, hs02, hs02_name) 

years <- c(2005, 2010, 2015, 2021)
party <- c(398)

# DATA ----

df <- tibble()

for (t in 1:length(years)) {
  
  baci <-
    read_csv(paste0(
      "data/raw/BACI_HS02_V202301/BACI_HS02_Y",
      years[t],
      "_V202301.csv"
    ))
  
  df1 <- baci %>%
    filter(i == party[1]) %>%
    left_join(products, by = c("k" = "hs02")) %>%
    left_join(countries, by = c("i" = "iso_num")) %>%
    group_by(t, i, name, hs02_2) %>%
    summarize(v = sum(v)) %>%
    ungroup() %>%
    mutate(share = v / sum(v),
           share2 = share ^ 2) %>%
    group_by(name, t) %>%
    summarize(d = sum(share2)) %>%
    ungroup()
  
  df2 <- baci %>%
    filter(i == party[2]) %>%
    left_join(products, by = c("k" = "hs02")) %>%
    left_join(countries, by = c("i" = "iso_num")) %>%
    group_by(t, i, name, hs02_2) %>%
    summarize(v = sum(v)) %>%
    ungroup() %>%
    mutate(share = v / sum(v),
           share2 = share ^ 2) %>%
    group_by(name, t) %>%
    summarize(d = sum(share2)) %>%
    ungroup()
  
  df3 <- baci %>%
    filter(i == party[3]) %>%
    left_join(products, by = c("k" = "hs02")) %>%
    left_join(countries, by = c("i" = "iso_num")) %>%
    group_by(t, i, name, hs02_2) %>%
    summarize(v = sum(v)) %>%
    ungroup() %>%
    mutate(share = v / sum(v),
           share2 = share ^ 2) %>%
    group_by(name, t) %>%
    summarize(d = sum(share2)) %>%
    ungroup()
  
  df4 <- baci %>%
    filter(i == party[4]) %>%
    left_join(products, by = c("k" = "hs02")) %>%
    left_join(countries, by = c("i" = "iso_num")) %>%
    group_by(t, i, name, hs02_2) %>%
    summarize(v = sum(v)) %>%
    ungroup() %>%
    mutate(share = v / sum(v),
           share2 = share ^ 2) %>%
    group_by(name, t) %>%
    summarize(d = sum(share2)) %>%
    ungroup()
  
  df5 <- baci %>%
    filter(i == party[5]) %>%
    left_join(products, by = c("k" = "hs02")) %>%
    left_join(countries, by = c("i" = "iso_num")) %>%
    group_by(t, i, name, hs02_2) %>%
    summarize(v = sum(v)) %>%
    ungroup() %>%
    mutate(share = v / sum(v),
           share2 = share ^ 2) %>%
    group_by(name, t) %>%
    summarize(d = sum(share2)) %>%
    ungroup()
  
  df <- rbind(df, df1, df2, df3, df4, df5)
  
}

df <- df %>% 
  mutate(t = factor(t, levels = years),
         name = factor(name, levels = c("Bangladesh", "Cambodia", "Sri Lanka",
                                        "Pakistan", "India")))

dfout <- df %>% 
  pivot_wider(countries_from = name,
              values_from = d)

write_csv(dfout, paste0(path[2], "Drafts/diversification.csv"))

# PLOT ----

plot <- ggplot(df, aes(x = name, y = 100 * d, fill = t)) + 
  geom_bar(stat = "identity", position = position_dodge(width = .8), 
           width = .7, ) + 
  geom_hline(yintercept = 0, size = .25, color = "gray25") + 
  scale_fill_manual(values = rev(c("#007db7", "#00A5D2", "#63CCEC", 
                                   "#9EE4FF"))) + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(plot.margin = margin(8, 2, 13, 2),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = c(9, 9, 9, 10, 9),
                                   color = "black",
                                   face = c("plain", "plain", "plain", "bold", 
                                            "plain"),
                                   margin = margin(-5, 0, 0, 0)),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.75, "lines"),
        legend.text = element_text(size = 9, margin = margin(0, 5, 0, 0)),
        legend.position = c(.75, .65),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray75", size = .25, 
                                          linetype = "dashed"))

ggsave(paste0(path[2], "2.5_diversification.png"), plot, 
       type = "cairo", dpi = 600, width = 16, height = 10, unit = "cm")

ggsave(paste0(path[2], "2.5_diversification.pdf"), plot, 
       device = cairo_pdf, dpi = 600, width = 16, height = 10, unit = "cm")

######### END #########
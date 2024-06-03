# CONSTANT PRICE COMPARISON
# Draft

# Setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(here)
library(arrow)
library(readxl)
library(mrio)
library(ggrepel)

filename <- "5.4_constant_comp"
select <- get_country_code("Kazakhstan")

df <- here("data", "raw", "GVC PR Dicrepancy Variability and Divergence Data.xlsx") |> 
  read_excel()

# Data --------------------------------------------------------------------



# Plot --------------------------------------------------------------------

df |> ggplot(aes(x = avg_d_bm_pr, y = var_d_bm_pr)) +
  geom_point(shape = 16, color = "#007db7", size = 4, alpha = .8) + 
  geom_text_repel(aes(label = economy), point.padding = 5, size = 2.5)
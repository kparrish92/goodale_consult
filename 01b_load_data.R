library(here)
library(tidyverse)
library(ggsignif)

mv_long = read.csv(here("data", "tidy", "mv_long.csv"))
dz_long = read.csv(here("data", "tidy", "dz_long.csv")) 


long_all = rbind(mv_long, dz_long) %>% 
  mutate(words = str_replace(words, "anos", "años en")) %>% 
  mutate(words = str_replace(words, "esta", "está")) %>% 
  mutate(words = str_replace(words, "Esta", "Está")) %>% 
  mutate(words = str_replace(words, "vivi", "viví")) %>% 
  mutate(words = str_replace(words, "San", "San Juan")) %>% 
  mutate(words = str_replace(words, "Senora", "Señora")) %>%
  mutate(words = str_replace(words, "olor", "olor a")) %>%
  mutate(words = str_replace(words, "se", "se van a")) %>%
  mutate(words = str_replace(words, "vivír", "vivir a")) %>%
  mutate(words = str_replace(words, "^le$", "le guste el")) %>% 
  mutate(words = str_replace(words, "possible", "posible que")) %>% 
  mutate(words = str_replace(words, "con", "con Manuel!")) %>% 
  mutate(words_cat = paste0(words, "_", position_spec)) %>% 
  mutate(focused = replace_na(focused, 0)) 



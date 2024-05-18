library(here)
library(tidyverse)
library(ggsignif)

mv_long = read.csv(here("data", "tidy", "mv_long.csv")) %>% 
  rename(Gender = SEX)
dz_long = read.csv(here("data", "tidy", "dz_long.csv")) %>% 
  rename(Gender = SEX)


goodale_theme <- function() {
  theme(
    # add border 1)
    # color background 2)
    panel.background = element_rect(fill = "white"),
    
  
  #  panel.grid.major.x = element_line(colour = "black", linetype = 2, size = 0.2),
    #panel.grid.minor.x = element_blank(),
#    panel.grid.major.y =  element_line(colour = "black", linetype = 2, size = 0.2),
   # panel.grid.minor.y = element_blank(),
    #
    # modify grid 3)
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "black", family = "Times New Roman", size = 10),
    axis.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    # legend at the bottom 6)
    legend.position = "bottom",
    legend.title = element_text(colour = "black", family = "Times New Roman", size = 10),
    legend.text = element_text(colour = "black", family = "Times New Roman", size = 10)
  )
} 

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



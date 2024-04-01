library(here)
library(tidyverse)


mv_data = read.csv(here("data", "mv_data.csv"))
dz_data = read.csv(here("data", "dz_data.csv"))

length(unique(mv_data$NAME))
length(unique(dz_data$NAME))

mv_long = mv_data %>% 
  pivot_longer(cols = c(8:39), values_to = "tones", names_to = "sentence") %>% 
  separate(sentence, into = c("sentence","position", "words", "focused")) %>% 
  mutate(sentence_type = substr(sentence, start = 1, stop = 3)) %>% 
  mutate(total_speakers = length(unique(mv_data$NAME))) %>% 
  mutate(position_spec = position) %>% 
  mutate(position = case_when(
    position == "N" ~ "N",
    position == "N1" ~ "N",
    position == "PN1" ~ "PN",
    position == "PN2" ~ "PN",
    position == "PN3" ~ "PN",
    position == "PN4" ~ "PN",
    position == "PN5" ~ "PN"
  ))



mv_long %>% 
  write.csv(here("data", "tidy", "mv_long.csv"))

dz_long = dz_data %>% 
  pivot_longer(cols = c(8:39), values_to = "tones", names_to = "sentence") %>% 
  separate(sentence, into = c("sentence","position", "words", "focused")) %>% 
  mutate(sentence_type = substr(sentence, start = 1, stop = 3)) %>% 
  rename("Age.Group" = "AGE.GROUPS") %>% 
  mutate(total_speakers = length(unique(dz_data$NAME))) %>% 
  mutate(position_spec = position) %>%
  mutate(position = case_when(
    position == "N" ~ "N",
    position == "N1" ~ "N",
    position == "PN1" ~ "PN",
    position == "PN2" ~ "PN",
    position == "PN3" ~ "PN",
    position == "PN4" ~ "PN",
    position == "PN5" ~ "PN"
  ))



dz_long %>% 
  write.csv(here("data", "tidy", "dz_long.csv"))


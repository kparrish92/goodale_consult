library(tidyverse)
library(here)
mv_long = read.csv(here("data", "tidy", "mv_long.csv"))
dz_long = read.csv(here("data", "tidy", "dz_long.csv")) 

long_all = rbind(mv_long, dz_long)

long_all$tones = gsub("L*L%"  , "L* L%" , long_all$tones , fixed = TRUE)

nfd5 = long_all %>% 
  filter(sentence == "NFD5")

unique(nfd5$tones)


nfd5 %>% 
  group_by(DEPT) %>% 
  summarise(n = n())

desc = nfd5 %>% 
  group_by(DEPT, tones) %>% 
  summarise(n = n()) %>% 
  filter(n > 3) %>% 
  mutate(n_adj = case_when(
    DEPT == "Durazno" ~ n/20,
    DEPT == "Montevideo" ~ n/30,
  )) %>% 
  select(-n) %>% 
  pivot_wider(names_from = DEPT, values_from = n_adj) %>%
  mutate(Durazno = replace_na(Durazno, 0))

other = data.frame(tones = "other",
           Durazno = 1 - sum(desc$Durazno),
           Montevideo = 1 - sum(desc$Montevideo))

ready_df = rbind(desc, other)

tone_types = unique(ready_df$tones)[1:3]

for (thistone in 1:length(tone_types)) {
  
  thisdf = ready_df %>% filter(tones == tone_types[thistone]) %>% 
    mutate(adj = c(1.5, 1)) %>%
    mutate(count_adj = count*adj)
  
  poisson_model_city <- glm(count_adj ~ City, data = 
                              thisdf, 
                            family = poisson(link = "log"))
  
  
  results[thistone, 1] = tone_types[thistone]
  results[thistone, 2] = st
  results[thistone, 3] = summary(poisson_model_city)[["coefficients"]][2,4]
  
}


res_df = results %>% 
  as.data.frame() %>% 
  rename(tones = V1,
         statement_type = V2,
         p_value = V3) %>% 
  mutate(p_value = as.numeric(p_value)) %>% 
  mutate(result = case_when(
    p_value > .05 ~ "NS",
    p_value < .05 & p_value > .005 ~ "*",
    p_value < .005 & p_value > .0005 ~ "**",
    p_value < .0005 ~ "***"
  ))

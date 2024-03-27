
data = long_all

adjnum = data %>% 
  filter(position == "N1") %>% 
  filter(sentence_type == "NFD") %>% 
  group_by(DEPT, sentence) %>% 
  summarize(n = n()) 

plotdf = data %>% 
  filter(position == p) %>% 
  filter(sentence_type == st) %>% 
  group_by(tones, DEPT, sentence) %>% 
  summarize(n = n()) %>% 
  mutate(n = case_when(
    DEPT == "Montevideo" ~ n/30,
    DEPT == "Durazno" ~ n/20,
  )) %>% 
 filter(n > .1) %>% 
  pivot_wider(names_from = sentence, values_from = n) %>% 
  mutate(tones = replace_na(tones, "N/A"),
         NFD6 = replace_na(NFD6, 0),
         NFD5 = replace_na(NFD6, 0),
         NFD4 = replace_na(NFD4, 0),
         NFD3 = replace_na(NFD3, 0),
         NFD2 = replace_na(NFD2, 0),
         NFD1 = replace_na(NFD1, 0)) %>% 
  pivot_longer(cols = 3:8, names_to = "Sentence", values_to = "Percentage")

plotdf %>% 
  write.csv(here("data", "tidy", "stim_df.csv"))

plotdf %>% 
  ggplot(aes(x = tones, y = Percentage, fill = DEPT, position = "dodge")) + 
  geom_col(color = "black", position = "dodge2") + theme_minimal() +
  facet_wrap(~Sentence) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
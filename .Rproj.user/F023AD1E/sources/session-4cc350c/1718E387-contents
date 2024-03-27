
data = long_all

adjnum = data %>% 
  filter(position == "N") %>% 
  filter(sentence_type == "BFD") %>% 
  group_by(DEPT, sentence) %>% 
  summarize(n = n()) 

plotdf = data %>% 
  filter(position == "N") %>% 
  filter(sentence_type == "BFD") %>% 
  group_by(tones, DEPT, sentence) %>% 
  summarize(n = n()) %>% 
  mutate(n = case_when(
    DEPT == "Montevideo" ~ n/30,
    DEPT == "Durazno" ~ n/20,
  )) %>% 
  pivot_wider(names_from = sentence, values_from = n) %>% 
  mutate(tones = replace_na(tones, "N/A"),
         BFD4 = replace_na(BFD4, 0),
         BFD1 = replace_na(BFD1, 0),
         BFD2 = replace_na(BFD2, 0)) %>% 
  pivot_longer(cols = 3:5, names_to = "Sentence", values_to = "Percentage")

plotdf %>% 
  write.csv(here("data", "tidy", "stim_df_bfd.csv"))

plotdf %>% 
  ggplot(aes(x = tones, y = Percentage, fill = DEPT, position = "dodge")) + 
  geom_col(color = "black", position = "dodge2") + theme_minimal() +
  facet_wrap(~Sentence) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

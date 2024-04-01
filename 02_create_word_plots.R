source(here::here("00_helpers.R"))
mv_long = read.csv(here("data", "tidy", "mv_long.csv"))
dz_long = read.csv(here("data", "tidy", "dz_long.csv")) 

stim_df = read.csv(here("data", "tidy", "stim_df.csv"))
stim_df_bfd = read.csv(here("data", "tidy", "stim_df_bfd.csv"))

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
  mutate(words_cat = paste0(words, "_", position_spec))

unique(long_all$words_cat)  
  

## overall 
plot_word_by_word("Montevideo", "BFD1")
plot_word_by_word("Durazno", "BFD1")

plot_word_by_word("Montevideo", "BFD2")
plot_word_by_word("Durazno", "BFD2")

plot_word_by_word("Montevideo", "BFD4")
plot_word_by_word("Durazno", "BFD4")

plot_word_by_word("Montevideo", "NFD1")
plot_word_by_word("Durazno", "NFD1")

plot_word_by_word("Montevideo", "NFD2")
plot_word_by_word("Durazno", "NFD2")

plot_word_by_word("Montevideo", "NFD3")
plot_word_by_word("Durazno", "NFD3")

plot_word_by_word("Montevideo", "NFD4")
plot_word_by_word("Durazno", "NFD4")

plot_word_by_word("Montevideo", "NFD5")
plot_word_by_word("Durazno", "NFD5")

plot_word_by_word("Montevideo", "NFD6")
plot_word_by_word("Durazno", "NFD6")

# plots by sex 

plot_word_by_word_sex("Montevideo", "BFD1")
plot_word_by_word_sex("Durazno", "BFD1")

plot_word_by_word_sex("Montevideo", "BFD2")
plot_word_by_word_sex("Durazno", "BFD2")

plot_word_by_word_sex("Montevideo", "BFD4")
plot_word_by_word_sex("Durazno", "BFD4")

plot_word_by_word_sex("Montevideo", "NFD1")
plot_word_by_word_sex("Durazno", "NFD1")

plot_word_by_word_sex("Montevideo", "NFD2")
plot_word_by_word_sex("Durazno", "NFD2")

plot_word_by_word_sex("Montevideo", "NFD3")
plot_word_by_word_sex("Durazno", "NFD3")

plot_word_by_word_sex("Montevideo", "NFD4")
plot_word_by_word_sex("Durazno", "NFD4")

plot_word_by_word_sex("Montevideo", "NFD5")
plot_word_by_word_sex("Durazno", "NFD5")

plot_word_by_word_sex("Montevideo", "NFD6")
plot_word_by_word_sex("Durazno", "NFD6")

# Education 

# plots by education

plot_word_by_word_education("Montevideo", "BFD1")
plot_word_by_word_education("Durazno", "BFD1")

plot_word_by_word_education("Montevideo", "BFD2")
plot_word_by_word_education("Durazno", "BFD2")

plot_word_by_word_education("Montevideo", "BFD4")
plot_word_by_word_education("Durazno", "BFD4")

plot_word_by_word_education("Montevideo", "NFD1")
plot_word_by_word_education("Durazno", "NFD1")

plot_word_by_word_education("Montevideo", "NFD2")
plot_word_by_word_education("Durazno", "NFD2")

plot_word_by_word_education("Montevideo", "NFD3")
plot_word_by_word_education("Durazno", "NFD3")

plot_word_by_word_education("Montevideo", "NFD4")
plot_word_by_word_education("Durazno", "NFD4")

plot_word_by_word_education("Montevideo", "NFD5")
plot_word_by_word_education("Durazno", "NFD5")

plot_word_by_word_education("Montevideo", "NFD6")
plot_word_by_word_education("Durazno", "NFD6")



# Age 

# plots by age

plot_word_by_word_age("Montevideo", "BFD1")
plot_word_by_word_age("Durazno", "BFD1")

plot_word_by_word_age("Montevideo", "BFD2")
plot_word_by_word_age("Durazno", "BFD2")

plot_word_by_word_age("Montevideo", "BFD4")
plot_word_by_word_age("Durazno", "BFD4")

plot_word_by_word_age("Montevideo", "NFD1")
plot_word_by_word_age("Durazno", "NFD1")

plot_word_by_word_age("Montevideo", "NFD2")
plot_word_by_word_age("Durazno", "NFD2")

plot_word_by_word_age("Montevideo", "NFD3")
plot_word_by_word_age("Durazno", "NFD3")

plot_word_by_word_age("Montevideo", "NFD4")
plot_word_by_word_age("Durazno", "NFD4")

plot_word_by_word_age("Montevideo", "NFD5")
plot_word_by_word_age("Durazno", "NFD5")

plot_word_by_word_age("Montevideo", "NFD6")
plot_word_by_word_age("Durazno", "NFD6")


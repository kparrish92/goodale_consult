plot_8_function("Montevideo", "NFD1")

plot_8_function = function(city, sent)
  
{
  
  df_p7 = mv_long %>% 
    filter(DEPT == city & focused == 1 & sentence == sent) %>% 
    group_by(tones) %>% 
    summarize(n = n()) %>% 
    filter(n > 1) 
  
  a = df_p7 %>% 
    ggplot(aes(x = tones, y = n, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") + goodale_theme() +
    scale_y_continuous(breaks=seq(1,max(df_p7$n),1))
  
  
  b = mv_long %>% 
    filter(DEPT == city & focused == 1 & sentence == sent) %>% 
    group_by(tones, gender) %>% 
    summarize(n = n()) %>% 
    filter(tones %in% df_p7$tones) %>% 
    ggplot(aes(x = tones, y = n, fill = gender)) + 
    geom_col(color = "black") + goodale_theme() +
    scale_y_continuous(breaks=seq(1,10,1))
  
  
  
  c = mv_long %>% 
    filter(DEPT == city & focused == 1 & sentence == sent) %>% 
    group_by(tones, Age.Group) %>% 
    summarize(n = n()) %>% 
    filter(tones %in% df_p7$tones) %>% 
    ggplot(aes(x = tones, y = n, fill = Age.Group)) + 
    geom_col(color = "black") + goodale_theme() +
    scale_y_continuous(breaks=seq(1,10,1))
  
  plot = ggpubr::ggarrange(a,b,c, nrow = 1)
  return(plot)  
}


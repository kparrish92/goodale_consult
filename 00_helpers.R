create_xminp = function(times)
  
{
  mins = matrix(ncol = 1, nrow = times)
  
  for (i in 1:times) {
    t = .75 + 1*i-1
    mins[[i]] = t
  }
  return(mins)
}

create_xmaxp = function(times)
  
{
  mins = matrix(ncol = 1, nrow = times)
  
  for (i in 1:times) {
    t = 1.25 + 1*i-1
    mins[[i]] = t
  }
  return(mins)
}


make_sig_plot = function(p, st, data)
  
{
  # make a dataframe to plot
  
  
  adjnum = data %>% 
    filter(position == p) %>% 
    filter(sentence_type == st) %>% 
    group_by(DEPT) %>% 
    summarize(n = n()) 
  
  plotdf = data %>% 
    filter(position == p) %>% 
    filter(sentence_type == st) %>% 
    group_by(tones, DEPT) %>% 
    summarize(n = n()) %>% 
    pivot_wider(names_from = DEPT, values_from = n) %>% 
    mutate(Montevideo = round(replace_na(Montevideo, 0)/adjnum$n[2], digits = 2)*100) %>% 
    mutate(Durazno = round(replace_na(Durazno, 0)/adjnum$n[1], digits = 2)*100) %>% 
    filter(Montevideo > 5 | Durazno > 5)  %>% 
    pivot_longer(cols = 2:3, names_to = "City", values_to = "Percentage") %>% 
    mutate(tones = replace_na(tones, "N/A")) 
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 3, nrow = length(tone_types))
  
  model_df = long_all %>% 
    group_by(tones, DEPT, sentence_type) %>% 
    summarize(n = n()) %>% 
    pivot_wider(names_from = DEPT, values_from = n) %>% 
    filter(Montevideo > 5 | Durazno > 5) %>% 
    pivot_longer(cols = 3:4, names_to = "City", values_to = "count") %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    mutate(total_speakers = case_when(
      City == "Montevideo" ~ 30,
      City == "Durazno" ~ 20
    ))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = model_df %>% filter(tones == tone_types[thistone] & sentence_type == st) %>% 
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
  
  
  # get y positions 
  ypost = plotdf %>% 
    group_by(tones) %>% 
    summarize(yp = max(Percentage)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  pdf = left_join(res_df, ypost, by = "tones")
  
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = Percentage, fill = City, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") + theme_minimal() + 
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp(nrow(pdf))), 
                xmax = c(create_xmaxp(nrow(pdf))), annotation = c(pdf$result),
                tip_length = 0)
  
  return(plot)
}


make_sig_plot_gender = function(p, st, data)
  
{
  # make a dataframe to plot
  adjnum = data %>% 
    filter(position == p) %>% 
    filter(sentence_type == st) %>% 
    group_by(DEPT, SEX) %>% 
    summarize(n = n()) 
  
  
  plotdf = data %>% 
    filter(position == p) %>% 
    filter(sentence_type == st) %>% 
    group_by(tones, DEPT, SEX) %>% 
    summarize(n = n()) %>% 
    pivot_wider(names_from = DEPT, values_from = n) %>% 
    mutate(Montevideo = round(replace_na(Montevideo, 0)/adjnum$n[3], digits = 2)*100) %>% 
    mutate(Durazno = round(replace_na(Durazno, 0)/adjnum$n[1], digits = 2)*100) %>% 
    filter(Montevideo > 5 | Durazno > 5)  %>% 
    pivot_longer(cols = 3:4, names_to = "City", values_to = "Percentage") %>% 
    mutate(tones = replace_na(tones, "N/A")) 
  

  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = Percentage, fill = SEX, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") + theme_minimal() +
    facet_wrap(~City)
  
  return(plot)
}




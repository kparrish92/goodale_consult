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
    geom_col(color = "black", position = "dodge2") +
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
    group_by(DEPT, gender) %>% 
    summarize(n = n()) 
  
  
  plotdf = data %>% 
    filter(position == p) %>% 
    filter(sentence_type == st) %>% 
    group_by(tones, DEPT, gender) %>% 
    summarize(n = n()) %>% 
    pivot_wider(names_from = DEPT, values_from = n) %>% 
    mutate(Montevideo = round(replace_na(Montevideo, 0)/adjnum$n[3], digits = 2)*100) %>% 
    mutate(Durazno = round(replace_na(Durazno, 0)/adjnum$n[1], digits = 2)*100) %>% 
    filter(Montevideo > 5 | Durazno > 5)  %>% 
    pivot_longer(cols = 3:4, names_to = "City", values_to = "Percentage") %>% 
    mutate(tones = replace_na(tones, "N/A")) 
  

  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = Percentage, fill = gender, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    facet_wrap(~City)
  
  return(plot)
}



plot_word_by_word = function(city, sent)
{
  
  
  ## Filter for the city and sentence 
  find_tones = long_all %>%
    filter(DEPT == city) %>% 
    filter(sentence == sent) %>% 
    mutate(tones_new = str_trim(str_replace(tones, "L%", "")))
  
  ## Change order 
  find_tones$words_cat = 
    factor(find_tones$words_cat, levels=c(unique(find_tones$words_cat)))
  
  words_unique = unique(find_tones$words_cat)
  
  for (i in 1:length(words_unique)) {
    
    plot_r = find_tones %>% 
      group_by(tones_new, words_cat) %>% 
      summarize(n = n()) %>% 
      filter(words_cat == words_unique[i]) %>% 
      ggplot(aes(x = tones_new, y = n, fill = tones_new)) + 
      geom_col(color = "black") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(
        title = words_unique[i],
        subtitle = element_text(sent),
        caption = element_text(city),
        y = "Total occurences",
        x = "Tone") +
      theme(legend.position = "none")
  
    
    
    ggsave(filename = paste0(sent, "_", city, "_", i,"_", words_unique[i],".png"), 
           plot = plot_r, path = here("word_plots"))

  }
  
}



plot_word_by_word_sex = function(city, sent)
{
  
  
  ## Filter for the city and sentence 
  find_tones = long_all %>%
    filter(DEPT == city) %>% 
    filter(sentence == sent) %>% 
    mutate(tones_new = str_trim(str_replace(tones, "L%", "")))
  
  ## Change order 
  find_tones$words_cat = 
    factor(find_tones$words_cat, levels=c(unique(find_tones$words_cat)))
  
  words_unique = unique(find_tones$words_cat)
  
  for (i in 1:length(words_unique)) {
    
    plot_r = find_tones %>% 
      group_by(tones_new, words_cat, SEX) %>% 
      summarize(n = n()) %>% 
      filter(words_cat == words_unique[i]) %>% 
      ggplot(aes(x = tones_new, y = n, fill = SEX)) + 
      geom_col(color = "black") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(
        title = words_unique[i],
        subtitle = element_text(sent),
        caption = element_text(city),
        y = "Total occurences",
        x = "Tone")
    
    
    
    ggsave(filename = paste0(sent, "_", city, "_", i,"_", words_unique[i],"_sex.png"), 
           plot = plot_r, path = here("word_plots_sex"))
    
  }
  
}


plot_word_by_word_education = function(city, sent)
{
  
  
  ## Filter for the city and sentence 
  find_tones = long_all %>%
    filter(DEPT == city) %>% 
    filter(sentence == sent) %>% 
    mutate(tones_new = str_trim(str_replace(tones, "L%", "")))
  
  ## Change order 
  find_tones$words_cat = 
    factor(find_tones$words_cat, levels=c(unique(find_tones$words_cat)))
  
  words_unique = unique(find_tones$words_cat)
  
  for (i in 1:length(words_unique)) {
    
    plot_r = find_tones %>% 
      group_by(tones_new, words_cat, EDU.Simplified) %>% 
      summarize(n = n()) %>% 
      filter(words_cat == words_unique[i]) %>% 
      ggplot(aes(x = tones_new, y = n, fill = EDU.Simplified)) + 
      geom_col(color = "black") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(
        title = words_unique[i],
        subtitle = element_text(sent),
        caption = element_text(city),
        y = "Total occurences",
        x = "Tone")
    
    
    
    ggsave(filename = paste0(sent, "_", city, "_", i,"_", words_unique[i],"_edu.png"), 
           plot = plot_r, path = here("word_plots_education"))
    
  }
  
}



plot_word_by_word_age = function(city, sent)
{
  
  
  ## Filter for the city and sentence 
  find_tones = long_all %>%
    filter(DEPT == city) %>% 
    filter(sentence == sent) %>% 
    mutate(tones_new = str_trim(str_replace(tones, "L%", "")))
  
  ## Change order 
  find_tones$words_cat = 
    factor(find_tones$words_cat, levels=c(unique(find_tones$words_cat)))
  
  words_unique = unique(find_tones$words_cat)
  
  for (i in 1:length(words_unique)) {
    
    plot_r = find_tones %>% 
      group_by(tones_new, words_cat, Age.Group) %>% 
      summarize(n = n()) %>% 
      filter(words_cat == words_unique[i]) %>% 
      ggplot(aes(x = tones_new, y = n, fill = Age.Group)) + 
      geom_col(color = "black") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(
        title = words_unique[i],
        subtitle = element_text(sent),
        caption = element_text(city),
        y = "Total occurences",
        x = "Tone")
    
    
    
    ggsave(filename = paste0(sent, "_", city, "_", i,"_", words_unique[i],"_age.png"), 
           plot = plot_r, path = here("word_plots_age"))
    
  }
  
}



make_sig_plot_f = function(foc, data)
  
{
  # make a dataframe to plot
  
  adjnum = data %>% 
    filter(focused == foc) %>% 
    filter(sentence_type == "NFD") %>% 
    group_by(DEPT) %>% 
    summarize(n = n()) 
  
  plotdf = data %>% 
    filter(focused == foc) %>% 
    filter(sentence_type == "NFD") %>% 
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
    
    thisdf = model_df %>% filter(tones == tone_types[thistone] & sentence_type == "NFD") %>% 
      mutate(adj = c(1.5, 1)) %>%
      mutate(count_adj = count*adj)
    
    poisson_model_city <- glm(count_adj ~ City, data = 
                                thisdf, 
                              family = poisson(link = "log"))
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "NFD"
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
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp(nrow(pdf))), 
                xmax = c(create_xmaxp(nrow(pdf))), annotation = c(pdf$result),
                tip_length = 0)
  
  return(plot)
}



plot_sex_per_city = function(city, st, p)
  
{
  plotdf = long_all %>% 
    mutate(tones_w_boundary = tones) %>% 
  #  mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    filter(sentence_type == st) %>% 
    filter(position == p) %>% 
    group_by(gender, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = gender, values_from = n) %>% 
    mutate(Female = replace_na(Female, 0)) %>% 
    mutate(Male = replace_na(Male, 0)) %>% 
    filter(Male > 5 & Female > 5) %>% 
    pivot_longer(cols = 2:3, names_to = "gender", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 3, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    poisson_model_city <- glm(n ~ gender, data = 
                                thisdf, 
                              family = poisson(link = "log"))
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "NFD"
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
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  pdf = left_join(res_df, ypost, by = "tones")
  
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = gender, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp(nrow(pdf))), 
                xmax = c(create_xmaxp(nrow(pdf))), annotation = c(pdf$result),
                tip_length = 0)
  
  return(plot)
}


plot_ed_per_city_nfd = function(city, st, f)
  
{
  plotdf = long_all %>% 
    mutate(tones_w_boundary = tones) %>% 
  #  mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    filter(sentence_type == st) %>% 
    filter(focused == f) %>% 
    group_by(EDU.Simplified, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = EDU.Simplified, values_from = n) %>% 
    mutate(`Primary/Middle` = replace_na(`Primary/Middle`, 0)) %>% 
    mutate(Secondary = replace_na(Secondary, 0)) %>% 
    mutate(Tertiary = replace_na(Tertiary, 0)) %>%  
    filter(Tertiary > 5 | Secondary > 5 | `Primary/Middle` > 5) %>% 
    pivot_longer(cols = 2:4, names_to = "EDU.Simplified", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 4, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    thisdf$EDU.Simplified = as.factor(thisdf$EDU.Simplified)
    thisdf$EDU.Simplified = relevel(thisdf$EDU.Simplified, ref = "Secondary")
    
    if (thisdf[2,3] == 0){
      poisson_model_city = "No test"
    } else {
      poisson_model_city = glm(n ~ EDU.Simplified, data = thisdf, family = poisson(link = "log"))
    }
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "NFD"
    results[thistone, 3] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][2,4] < .05, print("*"), "NS"),
                                  "NT")
    results[thistone, 4] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][3,4] < .05, print("*"), "NS"),
                                  "NT")
  }
  
  
  
  plotdf %>% 
    pivot_wider(names_from = EDU.Simplified, values_from = n) %>% 
    mutate(p_value_sec_c = case_when(
      Secondary == 0 | `Primary/Middle` == 0 ~ "NT"
    ))
  
  res_df = results %>% 
    as.data.frame() %>% 
    rename(tones = V1,
           statement_type = V2,
           p_value_sec = V3,
           p_value_ter = V4)
  
  ypost_c1 = plotdf %>% 
    # filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Secondary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  ypost_c2 = plotdf %>% 
    filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Tertiary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  pdf = left_join(res_df, ypost_c1, by = "tones")
  pdf2 = left_join(res_df, ypost_c2, by = "tones")
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = EDU.Simplified, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp_c1(nrow(pdf))), 
                xmax = c(create_xmaxp_c1(nrow(pdf))), annotation = c(pdf$p_value_sec),
                tip_length = 0)  +
    geom_signif(y_position = c(pdf$yp+2), xmin = c(create_xminp_c2(nrow(pdf))), 
                xmax = c(create_xmaxp_c2(nrow(pdf))), annotation = c(pdf$p_value_ter),
                tip_length = 0) 
  
  
  return(plot)
}



plot_ed_per_city = function(city, st, p)
  
{
  plotdf = long_all %>% 
    mutate(tones_w_boundary = tones) %>% 
 #   mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    filter(sentence_type == st) %>% 
    filter(position == p) %>% 
    group_by(EDU.Simplified, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = EDU.Simplified, values_from = n) %>% 
    mutate(`Primary/Middle` = replace_na(`Primary/Middle`, 0)) %>% 
    mutate(Secondary = replace_na(Secondary, 0)) %>% 
    mutate(Tertiary = replace_na(Tertiary, 0)) %>%  
    filter(Tertiary > 5 | Secondary > 5 | `Primary/Middle` > 5) %>% 
    pivot_longer(cols = 2:4, names_to = "EDU.Simplified", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 4, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    thisdf$EDU.Simplified = as.factor(thisdf$EDU.Simplified)
    thisdf$EDU.Simplified = relevel(thisdf$EDU.Simplified, ref = "Secondary")
    
    if (thisdf[2,3] == 0){
      poisson_model_city = "No test"
    } else {
      poisson_model_city = glm(n ~ EDU.Simplified, data = thisdf, family = poisson(link = "log"))
    }
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "BFD"
    results[thistone, 3] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][2,4] < .05, print("*"), "NS"),
                                  "NT")
    results[thistone, 4] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][3,4] < .05, print("*"), "NS"),
                                  "NT")
  }
  
  
  
  plotdf %>% 
    pivot_wider(names_from = EDU.Simplified, values_from = n) %>% 
    mutate(p_value_sec_c = case_when(
      Secondary == 0 | `Primary/Middle` == 0 ~ "NT"
    ))
  
  res_df = results %>% 
    as.data.frame() %>% 
    rename(tones = V1,
           statement_type = V2,
           p_value_sec = V3,
           p_value_ter = V4)
  
  ypost_c1 = plotdf %>% 
    # filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Secondary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  ypost_c2 = plotdf %>% 
    filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Tertiary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  pdf = left_join(res_df, ypost_c1, by = "tones")
  pdf2 = left_join(res_df, ypost_c2, by = "tones")
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = EDU.Simplified, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp_c1(nrow(pdf))), 
                xmax = c(create_xmaxp_c1(nrow(pdf))), annotation = c(pdf$p_value_sec),
                tip_length = 0)  +
    geom_signif(y_position = c(pdf$yp+2), xmin = c(create_xminp_c2(nrow(pdf))), 
                xmax = c(create_xmaxp_c2(nrow(pdf))), annotation = c(pdf$p_value_ter),
                tip_length = 0) 
  
  
  return(plot)
}


plot_age_per_city = function(city, st, p)
  
{
  plotdf = long_all %>% 
    mutate(tones_w_boundary = tones) %>% 
#    mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    filter(sentence_type == st) %>% 
    filter(position == p) %>% 
    group_by(Age.Group, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = Age.Group, values_from = n) %>% 
    janitor::clean_names() %>% 
    mutate(x18_35 = replace_na(x18_35, 0)) %>% 
    mutate(x36_59 = replace_na(x36_59, 0)) %>% 
    mutate(x60 = replace_na(x60, 0)) %>%  
    filter(x18_35 > 5 | x36_59 > 5 | x60 > 5) %>% 
    pivot_longer(cols = 2:4, names_to = "Age.Group", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 4, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    thisdf$Age.Group = as.factor(thisdf$Age.Group)
    thisdf$Age.Group = relevel(thisdf$Age.Group, ref = "x36_59")
    
    if (thisdf[2,3] == 0){
      poisson_model_city = "No test"
    } else {
      poisson_model_city = glm(n ~ Age.Group, data = thisdf, family = poisson(link = "log"))
    }
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = st
    results[thistone, 3] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][2,4] < .05, print("*"), "NS"),
                                  "NT")
    results[thistone, 4] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][3,4] < .05, print("*"), "NS"),
                                  "NT")
  }
  
  
  res_df = results %>% 
    as.data.frame() %>% 
    rename(tones = V1,
           statement_type = V2,
           p_value_sec = V3,
           p_value_ter = V4)
  
  ypost_c1 = plotdf %>% 
    # filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Secondary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  pdf = left_join(res_df, ypost_c1, by = "tones")
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = Age.Group, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp_c1(nrow(pdf))), 
                xmax = c(create_xmaxp_c1(nrow(pdf))), annotation = c(pdf$p_value_sec),
                tip_length = 0)  +
    geom_signif(y_position = c(pdf$yp+2), xmin = c(create_xminp_c2(nrow(pdf))), 
                xmax = c(create_xmaxp_c2(nrow(pdf))), annotation = c(pdf$p_value_ter),
                tip_length = 0) 
  
  
  return(plot)
}



sig_plot_for_combined_age = function(city)
  
{
  plotdf = long_all %>%
    filter(focused == 0 & position == "PN") %>% 
    filter(!stringr::str_detect(tones, '-')) %>% 
    #    mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    group_by(Age.Group, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = Age.Group, values_from = n) %>% 
    janitor::clean_names() %>% 
    mutate(x18_35 = replace_na(x18_35, 0)) %>% 
    mutate(x36_59 = replace_na(x36_59, 0)) %>% 
    mutate(x60 = replace_na(x60, 0)) %>%  
    filter(x18_35 > 5 | x36_59 > 5 | x60 > 5) %>% 
    pivot_longer(cols = 2:4, names_to = "Age.Group", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 4, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    thisdf$Age.Group = as.factor(thisdf$Age.Group)
    thisdf$Age.Group = relevel(thisdf$Age.Group, ref = "x36_59")
    
    if (thisdf[2,3] == 0){
      poisson_model_city = "No test"
    } else {
      poisson_model_city = glm(n ~ Age.Group, data = thisdf, family = poisson(link = "log"))
    }
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "unfocued pn"
    results[thistone, 3] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][2,4] < .05, print("*"), "NS"),
                                  "NT")
    results[thistone, 4] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][3,4] < .05, print("*"), "NS"),
                                  "NT")
  }
  
  
  res_df = results %>% 
    as.data.frame() %>% 
    rename(tones = V1,
           statement_type = V2,
           p_value_sec = V3,
           p_value_ter = V4)
  
  ypost_c1 = plotdf %>% 
    # filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Secondary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  pdf = left_join(res_df, ypost_c1, by = "tones")
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = Age.Group, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp_c1(nrow(pdf))), 
                xmax = c(create_xmaxp_c1(nrow(pdf))), annotation = c(pdf$p_value_sec),
                tip_length = 0)  +
    geom_signif(y_position = c(pdf$yp+2), xmin = c(create_xminp_c2(nrow(pdf))), 
                xmax = c(create_xmaxp_c2(nrow(pdf))), annotation = c(pdf$p_value_ter),
                tip_length = 0) 
  
  
  return(plot)
}



plot_ed_per_city = function(city, st, p)
  
{
  plotdf = long_all %>% 
    mutate(tones_w_boundary = tones) %>% 
    mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    filter(sentence_type == st) %>% 
    filter(position == p) %>% 
    group_by(EDU.Simplified, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = EDU.Simplified, values_from = n) %>% 
    mutate(`Primary/Middle` = replace_na(`Primary/Middle`, 0)) %>% 
    mutate(Secondary = replace_na(Secondary, 0)) %>% 
    mutate(Tertiary = replace_na(Tertiary, 0)) %>%  
    filter(Tertiary > 5 | Secondary > 5 | `Primary/Middle` > 5) %>% 
    pivot_longer(cols = 2:4, names_to = "EDU.Simplified", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 4, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    thisdf$EDU.Simplified = as.factor(thisdf$EDU.Simplified)
    thisdf$EDU.Simplified = relevel(thisdf$EDU.Simplified, ref = "Secondary")
    
    if (thisdf[2,3] == 0){
      poisson_model_city = "No test"
    } else {
      poisson_model_city = glm(n ~ EDU.Simplified, data = thisdf, family = poisson(link = "log"))
    }
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "BFD"
    results[thistone, 3] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][2,4] < .05, print("*"), "NS"),
                                  "NT")
    results[thistone, 4] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][3,4] < .05, print("*"), "NS"),
                                  "NT")
  }
  
  
  
  plotdf %>% 
    pivot_wider(names_from = EDU.Simplified, values_from = n) %>% 
    mutate(p_value_sec_c = case_when(
      Secondary == 0 | `Primary/Middle` == 0 ~ "NT"
    ))
  
  res_df = results %>% 
    as.data.frame() %>% 
    rename(tones = V1,
           statement_type = V2,
           p_value_sec = V3,
           p_value_ter = V4)
  
  ypost_c1 = plotdf %>% 
    # filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Secondary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  ypost_c2 = plotdf %>% 
    filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Tertiary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  pdf = left_join(res_df, ypost_c1, by = "tones")
  pdf2 = left_join(res_df, ypost_c2, by = "tones")
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = EDU.Simplified, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp_c1(nrow(pdf))), 
                xmax = c(create_xmaxp_c1(nrow(pdf))), annotation = c(pdf$p_value_sec),
                tip_length = 0)  +
    geom_signif(y_position = c(pdf$yp+2), xmin = c(create_xminp_c2(nrow(pdf))), 
                xmax = c(create_xmaxp_c2(nrow(pdf))), annotation = c(pdf$p_value_ter),
                tip_length = 0) 
  
  
  return(plot)
}


plot_age_per_city_nfd = function(city, st, f)
  
{
  plotdf = long_all %>% 
    mutate(tones_w_boundary = tones) %>% 
 #   mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    filter(sentence_type == st) %>% 
    filter(focused == f) %>% 
    group_by(Age.Group, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = Age.Group, values_from = n) %>% 
    janitor::clean_names() %>% 
    mutate(x18_35 = replace_na(x18_35, 0)) %>% 
    mutate(x36_59 = replace_na(x36_59, 0)) %>% 
    mutate(x60 = replace_na(x60, 0)) %>%  
    filter(x18_35 > 5 | x36_59 > 5 | x60 > 5) %>% 
    pivot_longer(cols = 2:4, names_to = "Age.Group", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 4, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    thisdf$Age.Group = as.factor(thisdf$Age.Group)
    thisdf$Age.Group = relevel(thisdf$Age.Group, ref = "x36_59")
    
    if (thisdf[2,3] == 0){
      poisson_model_city = "No test"
    } else {
      poisson_model_city = glm(n ~ Age.Group, data = thisdf, family = poisson(link = "log"))
    }
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = st
    results[thistone, 3] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][2,4] < .05, print("*"), "NS"),
                                  "NT")
    results[thistone, 4] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][3,4] < .05, print("*"), "NS"),
                                  "NT")
  }
  
  
  res_df = results %>% 
    as.data.frame() %>% 
    rename(tones = V1,
           statement_type = V2,
           p_value_sec = V3,
           p_value_ter = V4)
  
  ypost_c1 = plotdf %>% 
    # filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Secondary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  pdf = left_join(res_df, ypost_c1, by = "tones")
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = Age.Group, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp_c1(nrow(pdf))), 
                xmax = c(create_xmaxp_c1(nrow(pdf))), annotation = c(pdf$p_value_sec),
                tip_length = 0)  +
    geom_signif(y_position = c(pdf$yp+2), xmin = c(create_xminp_c2(nrow(pdf))), 
                xmax = c(create_xmaxp_c2(nrow(pdf))), annotation = c(pdf$p_value_ter),
                tip_length = 0) 
  
  
  return(plot)
}


plot_sex_per_city_nfd = function(city, st, f)
  
{
  plotdf = long_all %>% 
    mutate(tones_w_boundary = tones) %>% 
#    mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    filter(sentence_type == st) %>% 
    filter(focused == f) %>% 
    group_by(gender, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = gender, values_from = n) %>% 
    mutate(Female = replace_na(Female, 0)) %>% 
    mutate(Male = replace_na(Male, 0)) %>% 
    filter(Male > 5 & Female > 5) %>% 
    pivot_longer(cols = 2:3, names_to = "gender", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 3, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    poisson_model_city <- glm(n ~ gender, data = 
                                thisdf, 
                              family = poisson(link = "log"))
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "NFD"
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
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  pdf = left_join(res_df, ypost, by = "tones")
  
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = gender, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp(nrow(pdf))), 
                xmax = c(create_xmaxp(nrow(pdf))), annotation = c(pdf$result),
                tip_length = 0)
  
  return(plot)
}



create_xminp_c1 = function(times)
  
{
  mins = matrix(ncol = 1, nrow = times)
  
  for (i in 1:times) {
    t = .67 + 1*i-1
    mins[[i]] = t
  }
  return(mins)
}

create_xmaxp_c1 = function(times)
  
{
  mins = matrix(ncol = 1, nrow = times)
  
  for (i in 1:times) {
    t = 1 + 1*i-1
    mins[[i]] = t
  }
  return(mins)
}


create_xminp_c2 = function(times)
  
{
  mins = matrix(ncol = 1, nrow = times)
  
  for (i in 1:times) {
    t = 1 + 1*i-1
    mins[[i]] = t
  }
  return(mins)
}

create_xmaxp_c2 = function(times)
  
{
  mins = matrix(ncol = 1, nrow = times)
  
  for (i in 1:times) {
    t = 1.33 + 1*i-1
    mins[[i]] = t
  }
  return(mins)
}



sig_plot_for_combined = function(city, comparison)
  
{
  plotdf = long_all %>%
    filter(focused == 0 & position == "PN") %>% 
    filter(!stringr::str_detect(tones, '-')) %>% 
    #    mutate(tones_w_boundary = tones) %>% 
    #  mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    group_by(gender, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = comparison, values_from = n) %>% 
    mutate(Female = replace_na(Female, 0)) %>% 
    mutate(Male = replace_na(Male, 0)) %>% 
    filter(Male > 5 & Female > 5) %>% 
    pivot_longer(cols = 2:3, names_to = paste0(comparison), values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 3, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    colnames(thisdf)[2] <- c("comparison")
    
    poisson_model_city <- glm(n ~ comparison, data = 
                                thisdf, 
                              family = poisson(link = "log"))
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "Non_focused PN"
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
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  pdf = left_join(res_df, ypost, by = "tones")
  
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = gender, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") + 
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp(nrow(pdf))), 
                xmax = c(create_xmaxp(nrow(pdf))), annotation = c(pdf$result),
                tip_length = 0)
  
  return(plot)
}


plot_seven_function_gender = function(city, sentence_t, comparison)
  
{
  plotdf = long_all %>%
    filter(focused == 1 & sentence_type == sentence_t) %>% 
    #    mutate(tones_w_boundary = tones) %>% 
    #  mutate(tones = str_trim(str_replace(tones, "L%", ""))) %>% 
    filter(DEPT == city) %>% 
    group_by(gender, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = comparison, values_from = n) %>% 
    mutate(Female = replace_na(Female, 0)) %>% 
    mutate(Male = replace_na(Male, 0)) %>% 
    filter(Male > 5 & Female > 5) %>% 
    pivot_longer(cols = 2:3, names_to = paste0(comparison), values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 3, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    colnames(thisdf)[2] <- c("comparison")
    
    poisson_model_city <- glm(n ~ comparison, data = 
                                thisdf, 
                              family = poisson(link = "log"))
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "Non_focused PN"
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
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  pdf = left_join(res_df, ypost, by = "tones")
  
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = gender, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") + 
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp(nrow(pdf))), 
                xmax = c(create_xmaxp(nrow(pdf))), annotation = c(pdf$result),
                tip_length = 0)
  
  return(plot)
}


plot_seven_function_age = function(city, sentence_t)
  
{
  plotdf = long_all %>%
    filter(focused == 1 & sentence_type == sentence_t) %>%
    filter(DEPT == city) %>% 
    group_by(Age.Group, tones) %>%
    summarize(n = n()) %>% 
    pivot_wider(names_from = Age.Group, values_from = n) %>% 
    janitor::clean_names() %>% 
    mutate(x18_35 = replace_na(x18_35, 0)) %>% 
    mutate(x36_59 = replace_na(x36_59, 0)) %>% 
    mutate(x60 = replace_na(x60, 0)) %>%  
    filter(x18_35 > 5 | x36_59 > 5 | x60 > 5) %>% 
    pivot_longer(cols = 2:4, names_to = "Age.Group", values_to = "n") %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  tone_types = unique(plotdf$tones)
  
  results = matrix(ncol = 4, nrow = length(tone_types))
  
  
  # Run BFD models and store p_values 
  for (thistone in 1:length(tone_types)) {
    
    thisdf = plotdf %>% filter(tones == tone_types[thistone]) 
    
    thisdf$Age.Group = as.factor(thisdf$Age.Group)
    thisdf$Age.Group = relevel(thisdf$Age.Group, ref = "x36_59")
    
    if (thisdf[2,3] == 0){
      poisson_model_city = "No test"
    } else {
      poisson_model_city = glm(n ~ Age.Group, data = thisdf, family = poisson(link = "log"))
    }
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = "unfocued pn"
    results[thistone, 3] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][2,4] < .05, print("*"), "NS"),
                                  "NT")
    results[thistone, 4] = ifelse(typeof(poisson_model_city) == "list", 
                                  ifelse(summary(poisson_model_city)[["coefficients"]][3,4] < .05, print("*"), "NS"),
                                  "NT")
  }
  
  
  res_df = results %>% 
    as.data.frame() %>% 
    rename(tones = V1,
           statement_type = V2,
           p_value_sec = V3,
           p_value_ter = V4)
  
  ypost_c1 = plotdf %>% 
    # filter(EDU.Simplified == "Primary/Middle" | EDU.Simplified == "Secondary") %>% 
    group_by(tones) %>% 
    summarize(yp = max(n)) %>% 
    mutate(tones = replace_na(tones, "N/A"))
  
  pdf = left_join(res_df, ypost_c1, by = "tones")
  
  plotdf$tones = factor(plotdf$tones, levels = c(res_df$tones))
  
  plot = plotdf %>% 
    mutate(tones = replace_na(tones, "N/A")) %>% 
    ggplot(aes(x = tones, y = n, fill = Age.Group, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp_c1(nrow(pdf))), 
                xmax = c(create_xmaxp_c1(nrow(pdf))), annotation = c(pdf$p_value_sec),
                tip_length = 0)  +
    geom_signif(y_position = c(pdf$yp+2), xmin = c(create_xminp_c2(nrow(pdf))), 
                xmax = c(create_xmaxp_c2(nrow(pdf))), annotation = c(pdf$p_value_ter),
                tip_length = 0) 
  
  
  return(plot)
}


plot_8_function = function(city, sent)
  
{
  
  df_p7 = long_all %>% 
    filter(DEPT == city & focused == 1 & sentence == sent) %>% 
    group_by(tones) %>% 
    summarize(n = n()) %>% 
    filter(n > 1) 
  
  a = df_p7 %>% 
    ggplot(aes(x = tones, y = n, position = "dodge")) + 
    geom_col(color = "black", position = "dodge2") + goodale_theme() +
    scale_y_continuous(breaks=seq(1,max(df_p7$n),1))
  
  
  b = long_all %>% 
    filter(DEPT == city & focused == 1 & sentence == sent) %>% 
    group_by(tones, gender) %>% 
    summarize(n = n()) %>% 
    filter(tones %in% df_p7$tones) %>% 
    ggplot(aes(x = tones, y = n, fill = gender)) + 
    geom_col(color = "black") + goodale_theme() +
    scale_y_continuous(breaks=seq(1,max(df_p7$n),1))
  
  
  
  c = long_all %>% 
    filter(DEPT == city & focused == 1 & sentence == sent) %>% 
    group_by(tones, Age.Group) %>% 
    summarize(n = n()) %>% 
    filter(tones %in% df_p7$tones) %>% 
    ggplot(aes(x = tones, y = n, fill = Age.Group)) + 
    geom_col(color = "black") + goodale_theme() +
    scale_y_continuous(breaks=seq(1,max(df_p7$n),1))
  
  library(patchwork)
  
  combined <- a + b + c & theme(legend.position = "bottom")
  plot = combined + plot_layout(guides = "collect")
  
  return(plot)  
}

make_sig_plot_2 = function(data)
  
{
  # make a dataframe to plot
  
  
  adjnum = data %>% 
    filter(focused == 0 & position_spec != "N") %>%  
    group_by(DEPT) %>% 
    summarize(n = n()) 
  
  plotdf = data %>% 
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
    
    thisdf = model_df %>% filter(tones == tone_types[thistone]) %>% 
      mutate(adj = c(1.5, 1, 1.5, 1)) %>%
      mutate(count_adj = count*adj)
    
    poisson_model_city <- glm(count_adj ~ City, data = 
                                thisdf, 
                              family = poisson(link = "log"))
    
    
    results[thistone, 1] = tone_types[thistone]
    results[thistone, 2] = ":)"
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
    geom_col(color = "black", position = "dodge2") +
    geom_signif(y_position = c(pdf$yp+1), xmin = c(create_xminp(nrow(pdf))), 
                xmax = c(create_xmaxp(nrow(pdf))), annotation = c(pdf$result),
                tip_length = 0)
  
  return(plot)
}



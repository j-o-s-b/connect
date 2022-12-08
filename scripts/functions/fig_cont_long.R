# function for descriptive graph of continuous variables

fig_cont_long <- function(var) {
  temp_dat <- alspac_long %>% 
    group_by(age) %>%
    summarise(mean = mean({{var}}, na.rm = T)) %>% 
    na.omit() 
  
  nm <- select(alspac_long, {{var}}) %>% names()
  
  temp_dat %>% 
    ggplot(aes(age, mean)) + 
    geom_point() +
    geom_line() +
    labs(x = "Age",
         y = "Mean",
         title = str_remove(nm, "_fct") %>%
           str_replace_all("_", " ") %>% 
           str_to_title(),
         caption = "*Missing cases excluded") +
    theme_bw() +
    scale_x_continuous(breaks = temp_dat$age %>% unique())
}
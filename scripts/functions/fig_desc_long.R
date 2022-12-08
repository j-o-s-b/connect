# function for descriptive graph of categorical variables

fig_desc_long <- function(var){
  temp_dat <- alspac_long %>% 
    count(age, {{var}}) %>% 
    na.omit() %>% 
    group_by(age) %>% 
    mutate(prop = n/sum(n)) %>% 
    mutate(age = age)
  
  nm <- select(alspac_long, {{var}}) %>% names()
  
  temp_dat %>% 
    ggplot(aes(age, prop, fill = {{var}})) + 
    geom_bar(position = "fill", stat = "identity") +
    labs(x = "Age",
         y = "Proportion",
         fill = "",
         title = str_remove(nm, "_fct") %>%
           str_replace_all("_", " ") %>% 
           str_to_title(),
         caption = "*Missing cases excluded") +
    theme_bw() +
    scale_x_continuous(breaks = temp_dat$age %>% unique())
}

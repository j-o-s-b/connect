# function for longitudinal descriptive statistics

tab_desc_tab <- function(var) {
  
  
  nm <- select(alspac_long, {{var}}) %>% names() %>% 
    str_remove("_fct") %>%
    str_replace_all("_", " ") %>% 
    str_to_title()

  alspac_long %>% 
    count(age, {{var}}) %>%
    group_by(age) %>% 
    mutate(prop = n/sum(n) * 100) %>% 
    filter(prop != 100) %>% 
    setNames(c("Age", "Category", "N", "Prop")) %>% 
    mutate(Category = fct_explicit_na(Category, na_level  = "Missing")) %>% 
    knitr::kable(digits = 1, caption = nm)
}

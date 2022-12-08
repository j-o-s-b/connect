
# Admin -------------------------------------------------


library(tidyverse)

# load data
alspac_long <- read_rds("./data/clean/alspac_long_v01.rds")

# load functions
list.files("./scripts/functions/", full.names = T) %>% 
  map(source)

# Univariate statistics ----------------------------

# Outcomes -------------------------------------

dep_change <- alspac_long %>% 
  select(age, anxiety, depression, depression_smfq) %>% 
  group_by(age) %>% 
  summarise(Anxiety_prop = mean(anxiety, na.rm = T),
            Anxiety_prop_miss = mean(is.na(anxiety)),
            Depression_prop = mean(depression, na.rm = T),
            Depression_prop_miss = mean(is.na(depression)),
            Depression_smfq_prop = mean(depression_smfq, na.rm = T),
            Depression_smfq_miss = mean(is.na(depression_smfq)))

dep_change %>% 
  select(age, ends_with("prop")) %>% 
  gather(value = value, key = var, -age) %>% 
  mutate(var = str_remove(var, "_prop"),
         age = as.factor(age)) %>% 
  na.omit() %>% 
  ggplot(aes(age, value, color = var, group = var)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age",
       y = "Percentage of cases",
       color = "")

ggsave("./output/fig/dep_trends.png", dpi = 300)

# Key predictors
# bullying

bully_var <- c("bullying_victim_fct", 
               "bullying_often_bullied_fct",
               "bullying_frequency_bullied_fct")
map(bully_var, function(x) tab_desc_tab(.data[[x]]))

map(bully_var, function(x) fig_desc_long(.data[[x]]))


# friends

friends_var <- c("friends_plays_with_kids_fct",
                 "friends_freq_visits_fct",
                 "friends_happy_num_friends_fct",
                 "friends_understand_fct",
                 "friends_overall_happy_fct",
                 "friends_talk_problems_fct",
                 "friends_freq_see_outside_school_fct",
                 "friends_whether_any_close_friend",
                 "friends_num_close_friend_fct")
map(friends_var, function(x) tab_desc_tab(.data[[x]]))

map(friends_var, function(x) fig_desc_long(.data[[x]]))




# cont vars
alspac_long %>%
  select(age, friends_score, friends_score_log) %>% 
  rename_all(~str_replace_all(., "_", ".")) %>% 
  group_by(age) %>% 
  summarise_all(list(mean = ~mean(., na.rm = T),
                sd = ~sd(., na.rm = T),
                miss = ~mean(is.na(.)) * 100)) %>% 
  na.omit() %>% 
  pivot_longer(!age,
                names_sep = "_",
                names_to = c("Variable", ".value")) %>%
  mutate(Variable = str_replace_all(Variable, "\\.", "_")) %>% 
  arrange(Variable, age) %>% 
  select(Variable, age, everything()) %>% 
  rename_all(~str_to_title(.)) %>% 
  knitr::kable()

qplot(alspac_long$friends_score) + theme_bw()
qplot(alspac_long$friends_score_log) +theme_bw()

fig_cont_long(friends_score)



# school


school_vars <- c("school_talk_friends_fct", "school_likes_teacher_fct",
                 "school_left_out_fct", "school_everybodys_friend_fct", 
                 "school_classmates_not_all_their_friends_fct", 
                 "school_all_pupils_are_friends_fct", 
                 "school_some_classmates_mean_fct", 
                 "school_likes_schoolmates_fct", 
                 "school_frightened_by_schoolmates_fct", 
                 "school_feel_popular_fct", 
                 "school_get_on_with_classmates_fct", 
                 "school_teacher_fair_fct", 
                 "school_schoolmates_accept_them_fct", 
                 "school_feel_lonely_fct", "school_people_trust_them_fct", 
                 "school_people_depend_on_them_fct")

map(school_vars, function(x) tab_desc_tab(.data[[x]]))

map(school_vars, function(x) fig_desc_long(.data[[x]]))



# parents
alspac_long %>%
  select(age, parent_networks_mother_social_support_score, 
         parent_networks_mother_social_networks_score,
         parent_networks_partner_social_networks_score) %>% 
  rename_all(~str_replace_all(., "_", ".")) %>% 
  group_by(age) %>% 
  summarise_all(list(mean = ~mean(., na.rm = T),
                     sd = ~sd(., na.rm = T),
                     miss = ~mean(is.na(.)) * 100)) %>% 
  pivot_longer(!age,
               names_sep = "_",
               names_to = c("Variable", ".value")) %>%
  mutate(Variable = str_replace_all(Variable, "\\.", "_")) %>% 
  arrange(Variable, age) %>% 
  select(Variable, age, everything()) %>% 
  rename_all(~str_to_title(.)) %>% 
  filter(!is.na(Sd)) %>% 
  knitr::kable()


qplot(alspac_long$parent_networks_mother_social_support_score) +
  theme_bw()
qplot(alspac_long$parent_networks_mother_social_networks_score) +
  theme_bw()
qplot(alspac_long$parent_networks_partner_social_networks_score) +
  theme_bw()


fig_cont_long(parent_networks_mother_social_support_score)
fig_cont_long(parent_networks_mother_social_networks_score)
fig_cont_long(parent_networks_partner_social_networks_score)


# peer problem + social cohesion -----

# describe continuous vars
alspac_long %>%
  select(age, peer_problem_score, social_cohesion, social_discord) %>% 
  rename_all(~str_replace_all(., "_", ".")) %>% 
  group_by(age) %>% 
  summarise_all(list(mean = ~mean(., na.rm = T),
                     sd = ~sd(., na.rm = T),
                     miss = ~mean(is.na(.)) * 100)) %>% 
  pivot_longer(!age,
               names_sep = "_",
               names_to = c("Variable", ".value")) %>%
  mutate(Variable = str_replace_all(Variable, "\\.", "_")) %>% 
  arrange(Variable, age) %>% 
  select(Variable, age, everything()) %>% 
  rename_all(~str_to_title(.)) %>% 
  filter(!is.na(Sd)) %>% 
  knitr::kable()


qplot(alspac_long$peer_problem_score) # skewed
qplot(alspac_long$social_cohesion)

fig_cont_long(peer_problem_score)
fig_cont_long(social_cohesion)


# loneliness ---
tab_desc_tab(loneliness_frequency_fct)
fig_desc_long(loneliness_frequency_fct)





# romantic -----
romantic_var <- c("romantic_in_relationship_or_out_with_someone_fct",
                  "romantic_in_relationship_fct",
                  "romantic_emotionally_close_fct")

map(romantic_var, function(x) tab_desc_tab(.data[[x]]))
map(romantic_var, function(x) fig_desc_long(.data[[x]]))

# religion -----

religion_var <- c("religion_support_mother_fct",
                  "religion_support_partner_fct")

map(religion_var, function(x) tab_desc_tab(.data[[x]]))
map(religion_var, function(x) fig_desc_long(.data[[x]]))



# neighbour -------------------

neighbour_var <- c("neighbour_mother_neighourhood_opinion_fct",
                 "neighbour_burglary_worries_dad_fct",
                 "neighbour_mugging_worries_dad_fct",
                 "neighbour_sex_assault_pestering_worries_dad_fct",
                 "neighbour_vandalism_worries_dad_fct",
                 "neighbour_dad_lively_neighbourhood_fct",
                 "neighbour_dad_friendly_neighbourhood_fct",
                 "neighbour_dad_noisy_neighbourhood_fct",
                 "neighbour_dad_clean_neighbourhood_fct",
                 "neighbour_dad_attractive_neighbourhood_fct",
                 "neighbour_dad_polluted_neighbourhood_fct")

map(religion_var, function(x) tab_desc_tab(.data[[x]]))
map(religion_var, function(x) fig_desc_long(.data[[x]]))


 
qplot(alspac_long$neighbour_stress_score) +
  theme_bw()
fig_cont_long(neighbour_stress_score)


# family ----------

alspac_long %>%
  select(age, starts_with("family")) %>% 
  rename_all(~str_replace_all(., "_", ".")) %>% 
  group_by(age) %>% 
  summarise_all(list(mean = ~mean(., na.rm = T),
                     sd = ~sd(., na.rm = T),
                     miss = ~mean(is.na(.)) * 100)) %>% 
  pivot_longer(!age,
               names_sep = "_",
               names_to = c("Variable", ".value")) %>%
  mutate(Variable = str_replace_all(Variable, "\\.", "_")) %>% 
  arrange(Variable, age) %>% 
  select(Variable, age, everything()) %>% 
  rename_all(~str_to_title(.)) %>% 
  filter(!is.na(Sd)) %>% 
  knitr::kable()



# bereavement ------


tab_desc_tab(bereavement_death_in_family_since_fct)
fig_desc_long(bereavement_death_in_family_since_fct)


# life ----

qplot(alspac_long$life_score) +
  theme_bw()
fig_cont_long(life_score)


# Bivariate statistics ----------------------------
  

# bullying

bully_var <- c("bullying_victim_fct", 
               "bullying_often_bullied_fct",
               "bullying_frequency_bullied_fct")

alspac_long2 <- alspac_long %>% 
  select(uniqid, age, anxiety, depression, bullying_victim_fct,
         depression_smfq) %>%
  group_by(uniqid) %>% 
  mutate(bully_victim_8 = ifelse(age == 8 & bullying_victim_fct == "Yes",
                                 1, 0),
         bully_victim_8 = max(bully_victim_8, na.rm = T),
         bully_victim_10 = ifelse(age == 10 & bullying_victim_fct == "Yes",
                                  1, 0),
         bully_victim_10 = max(bully_victim_10, na.rm = T)) %>% 
  ungroup()



trends_bully_8 <- alspac_long2 %>% 
  group_by(age, bully_victim_8) %>% 
  summarise(Anxiety_prop = mean(anxiety, na.rm = T),
            Depression_prop = mean(depression, na.rm = T),
            Depress_smfq_prop = mean(depression_smfq, na.rm = T)) %>% 
  #  filter(!is.nan(Anxiety_prop)) %>% 
  rename(bully = bully_victim_8) %>% 
  mutate(bully_age = 8) %>% 
  ungroup()


trends_bully_10 <- alspac_long2 %>% 
  group_by(age, bully_victim_10) %>% 
  summarise(Anxiety_prop = mean(anxiety, na.rm = T),
            Depression_prop = mean(depression, na.rm = T),
            Depress_smfq_prop = mean(depression_smfq, na.rm = T)) %>% 
  #  filter(!is.nan(Anxiety_prop)) %>% 
  rename(bully = bully_victim_10) %>% 
  mutate(bully_age = 10) %>% 
  ungroup()

trends_bully <- rbind(trends_bully_8, trends_bully_10)


trends_bully %>% 
  gather(value = value, key = var, -age, -bully, -bully_age) %>%
  na.omit() %>% 
  mutate(var = str_remove(var, "_prop"),
         age = age,
         bully = ifelse(bully == 1, "Yes", "No"),
         bully = as.factor(bully) %>% fct_rev(),
         bully_age = ifelse(bully_age == 8,
                            "Bullied at age 8",
                            "Bullied at age 10"),
         bully_age = as.factor(bully_age) %>% fct_rev()) %>% 
  ggplot(aes(age, value, color = bully, 
             group = bully)) + 
  geom_line() +
  geom_point() +
  facet_grid(var~bully_age, scales = "free") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = unique(trends_bully$age)) +
  labs(x = "Age",
       y = "Percentage of cases",
       color = "Bullied") 

ggsave("./output/fig/dep_trends_bully.png", dpi = 100)



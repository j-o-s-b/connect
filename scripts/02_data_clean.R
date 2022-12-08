

# Admin -------------------------------------------------

# load packages
library(knitr)
library(tidyverse)
library(gt)
library(gtsummary)
library(ggthemes)
library(shiny)
library(viridis)
library(lavaan)
library(blavaan)
library(MplusAutomation)
library(rmarkdown)
library(glue)

# load functions
list.files("./scripts/functions/", full.names = T) %>% 
  map(source)

# load data
alspac_r <- read_rds("./data/clean/alspac_reduced_07.12.22.rds")

# explore data
glimpse(alspac_r)

# Reshape data in long ----------------------------------

alspac_r_small <- alspac_r %>% 
  select(uniqid, starts_with("anxiety"), starts_with("bullying"),
         starts_with("bereavement"), starts_with("depression"), 
         starts_with("family"), starts_with("friends"),
         starts_with("life"), 
         starts_with("school"), starts_with("parent"),
         starts_with("peer"), starts_with("social"),
         starts_with("loneliness"), starts_with("romantic"),
         starts_with("religion"), starts_with("neighbour"),
         -starts_with("school_enjoys"), -contains("inconsistent"))

glimpse(alspac_r_small)

# round number year
full_years <- names(alspac_r_small) %>%  
  str_extract_all("[0-9]+", simplify = T) %>% 
  as_tibble() %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate(V2 = ifelse(is.na(V2), 0, V2),
         V3 = ifelse(V2 > 6, 1, 0),
         round = V1 + V3)
  
# check if it looks ok
temp_nms <- cbind(names(alspac_r_small),
      str_replace(names(alspac_r_small), 
            "_[0-9].+", 
            str_c(".", as.character(full_years$round)))) 

# check duplicates
temp_nms %>%
  as.data.frame() %>% 
  count(V2) %>% 
  filter(n>1)

# clean names and round to years
alspac_r_small2 <- alspac_r_small %>% 
  rename_all(~str_replace(., "age_", "~")) %>% 
  rename_all(~str_replace(., "(_)([0-9].+$)", "~\\2")) %>% 
  rename_all(~str_replace(.,"~[0-9].+",
                          str_c("~", as.character(full_years$round)))) %>% 
  rename_all(~str_replace_all(., "__", "_")) %>% 
  rename_all(~str_replace(., "~gest", "~-1")) %>% 
  rename_all(~str_replace(., "_~", "~"))



# make long format
alspac_long <- alspac_r_small2 %>% 
  pivot_longer(!uniqid,
               names_sep = "~",
               names_to = c(".value", "age")) %>% 
  mutate(age = as.numeric(age))

# check result
glimpse(alspac_long)
count(alspac_long, age) %>% print(n = 100)


# Descriptives ---------------------------------------------


# Outcomes ------------


dep_change <- alspac_long %>% 
  select(age, anxiety, depression, depression_smfq) %>% 
  group_by(age) %>% 
  summarise(Anxiety_prop = mean(anxiety, na.rm = T),
            Anxiety_prop_miss = mean(is.na(anxiety)),
            Depression_prop = mean(depression, na.rm = T),
            Depression_prop_miss = mean(is.na(depression)),
            Depression_smfq_prop = mean(depression_smfq, na.rm = T),
            Depression_smfq_miss = mean(is.na(depression_smfq)))

# select non-missing rows
dep_change <- dep_change %>% 
  filter(!(Anxiety_prop_miss == 1 & Depression_prop_miss == 1 &
             Depression_smfq_miss == 1))

dep_change

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

# Key predictors

# bullying -----------------

# data clean
alspac_long <- mutate(alspac_long,
                      bullying_victim_fct = factor(bullying_victim,
                                                   labels = c("No", "Yes")),
                      bullying_often_bullied_fct = 
                        factorise(bullying_often_bullied),
                      bullying_often_bullied_fct = 
                        fct_recode(bullying_often_bullied_fct,
                                   NULL = "Don't know"),
                      bullying_frequency_bullied_fct = 
                        factorise(bullying_frequency_bullied))


# describe
count(alspac_long, bullying_victim, bullying_victim_fct)
count(alspac_long, bullying_often_bullied, bullying_often_bullied_fct)
count(alspac_long, bullying_frequency_bullied, bullying_frequency_bullied_fct)



             
# friends ---------------

# data clean
alspac_long <- mutate(alspac_long,
                      friends_plays_with_kids_fct = 
                        factorise(friends_plays_with_kids),
                      friends_freq_visits_fct = 
                        factorise(friends_freq_visits),
                      friends_happy_num_friends_fct = 
                        factorise(friends_happy_num_friends),
                      friends_happy_num_friends_fct = 
                        fct_recode(friends_happy_num_friends_fct,
                                   NULL = "Ch said DK",
                                   NULL = "Unknown"),
                      friends_understand_fct = 
                        factorise(friends_understand),
                      friends_understand_fct = 
                        fct_recode(friends_understand_fct,
                                   NULL = "Ch said DK",
                                   NULL = "Unknown"),
                      friends_overall_happy_fct = 
                        factorise(friends_overall_happy),
                      friends_overall_happy_fct = 
                        fct_recode(friends_overall_happy_fct,
                                   NULL = "Ch said DK",
                                   NULL = "Unknown"),
                      friends_talk_problems_fct = 
                        factorise(friends_talk_problems),
                      friends_talk_problems_fct = 
                        fct_recode(friends_talk_problems_fct,
                                   NULL = "Ch said DK",
                                   NULL = "Unknown"),
                      friends_freq_see_outside_school_fct = 
                        factorise(friends_freq_see_outside_school),
                      friends_freq_see_outside_school_fct = 
                        fct_recode(friends_freq_see_outside_school_fct,
                                   NULL = "Ch said DK",
                                   NULL = "Unknown",
                                   NULL = "Not Applicable"),
                      friends_score_log = log(friends_score),
                      friends_num_close_friend_fct = 
                        factor(friends_num_close_friend),
                      friends_num_close_friend_fct = 
                      fct_relevel(friends_num_close_friend_fct,
                                  "0", "1", "2-4", "5-9"))

                      

                      
# describe
count(alspac_long, friends_plays_with_kids, friends_plays_with_kids_fct)
count(alspac_long, friends_freq_visits, friends_freq_visits_fct)
count(alspac_long, friends_happy_num_friends, friends_happy_num_friends_fct)
count(alspac_long, friends_understand, friends_understand_fct)
count(alspac_long, friends_overall_happy, friends_overall_happy_fct)
count(alspac_long, friends_talk_problems, friends_talk_problems_fct)
count(alspac_long, friends_freq_see_outside_school, 
      friends_freq_see_outside_school_fct)
count(alspac_long, friends_score) %>% print(n = 100)
qplot(alspac_long$friends_score)
qplot(alspac_long$friends_score_log)


count(alspac_long, friends_num_close_friend_fct)
count(alspac_long, friends_num_close_friend, friends_num_close_friend_fct)
count(alspac_long, friends_whether_any_close_friend)




# school -------------------

# recode
alspac_long <- mutate(alspac_long,
                      school_talk_friends_fct = 
                        factorise(school_talk_friends),
                      school_likes_teacher_fct = 
                        factorise(school_likes_teacher))

# descriptives
count(alspac_long, school_talk_friends, school_talk_friends_fct)
count(alspac_long, school_likes_teacher, school_likes_teacher_fct)
            
# recode categorical vars
alspac_long <- mutate(alspac_long,
                      school_left_out_fct = 
                        factorise(school_left_out),
                      school_everybodys_friend_fct = 
                        factorise(school_everybodys_friend),
                      school_classmates_not_all_their_friends_fct = 
                        factorise(school_classmates_not_all_their_friends),
                      school_all_pupils_are_friends_fct = 
                        factorise(school_all_pupils_are_friends),
                      school_some_classmates_mean_fct = 
                        factorise(school_some_classmates_mean),
                      school_likes_schoolmates_fct = 
                        factorise(school_likes_schoolmates),
                      school_frightened_by_schoolmates_fct = 
                        factorise(school_frightened_by_schoolmates),
                      school_get_on_with_classmates_fct = 
                        factorise(school_get_on_with_classmates),
                      school_feel_popular_fct = 
                        factorise(school_feel_popular),
                      school_teacher_fair_fct = 
                        factorise(school_teacher_fair),
                      
                      school_schoolmates_accept_them_fct = 
                        factorise(school_schoolmates_accept_them),
                      school_feel_lonely_fct = 
                        factorise(school_feel_lonely),
                      school_people_trust_them_fct = 
                        factorise(school_people_trust_them),
                      school_people_depend_on_them_fct = 
                        factorise(school_people_depend_on_them),

                      school_schoolmates_accept_them_fct = 
                        fct_recode(school_schoolmates_accept_them_fct,
                                   NULL = "Don't know"),
                      school_feel_lonely_fct = 
                        fct_recode(school_feel_lonely_fct,
                                   NULL = "Don't know"),
                      school_people_trust_them_fct = 
                        fct_recode(school_people_trust_them_fct,
                                   NULL = "Don't know"),
                      school_people_depend_on_them_fct = 
                        fct_recode(school_people_depend_on_them_fct,
                                   NULL = "Don't know"),
                      school_get_on_with_classmates_fct = 
                        fct_recode(school_get_on_with_classmates_fct,
                                   NULL = "Don't know"),

                      
                      school_everybodys_friend_fct = 
                        fct_relevel(school_everybodys_friend_fct,
                                    "Yes", "Sometimes"),
                      school_classmates_not_all_their_friends_fct = 
                        fct_relevel(school_classmates_not_all_their_friends_fct,
                                    "Yes", "Sometimes"),
                      school_all_pupils_are_friends_fct = 
                        fct_relevel(school_all_pupils_are_friends_fct,
                                    "Yes", "Sometimes"),
                      school_some_classmates_mean_fct = 
                        fct_relevel(school_some_classmates_mean_fct,
                                    "Yes", "Sometimes"))



# count(alspac_long, school_looks_forward_see_teacher)
count(alspac_long, school_left_out, school_left_out_fct)
count(alspac_long, school_everybodys_friend, school_everybodys_friend_fct)
count(alspac_long, school_classmates_not_all_their_friends,
      school_classmates_not_all_their_friends_fct)
count(alspac_long, school_all_pupils_are_friends, 
      school_all_pupils_are_friends_fct)
count(alspac_long, school_some_classmates_mean, school_some_classmates_mean_fct)
count(alspac_long, school_likes_schoolmates, school_likes_schoolmates_fct)
count(alspac_long, school_frightened_by_schoolmates,
      school_frightened_by_schoolmates_fct)
count(alspac_long, school_feel_popular, school_feel_popular_fct)
count(alspac_long, school_get_on_with_classmates, 
      school_get_on_with_classmates_fct)
count(alspac_long, school_teacher_fair, school_teacher_fair_fct)


count(alspac_long, school_schoolmates_accept_them,
      school_schoolmates_accept_them_fct)
count(alspac_long, school_feel_lonely,
      school_feel_lonely_fct)
count(alspac_long, school_people_trust_them, school_people_trust_them_fct)
count(alspac_long, school_people_depend_on_them, 
      school_people_depend_on_them_fct)






# parents ---------------


# descriptives

qplot(alspac_long$parent_networks_mother_social_support_score)
qplot(alspac_long$parent_networks_partner_social_networks_score)



# peer problem --------------

count(alspac_long, peer_problem_score)
qplot(alspac_long$peer_problem_score) # skewed


# social cohesion -------------
count(alspac_long, social_cohesion)
qplot(alspac_long$social_cohesion)

# lots of negative values
count(alspac_long, social_discord)



# loneliness -------------------
# data clean
alspac_long <- mutate(alspac_long,
                      loneliness_frequency_fct = 
                        factorise(loneliness_frequency))

count(alspac_long, loneliness_frequency, loneliness_frequency_fct)


# romantic ----------------
alspac_long %>% 
  select(starts_with("romantic")) %>% 
  names()

alspac_long <- mutate(alspac_long,
                      romantic_in_relationship_or_out_with_someone_fct = 
                        factor(romantic_in_relationship_or_out_with_someone,
                               labels = c("No", "Yes")),
                      romantic_in_relationship_fct = 
                        factor(romantic_in_relationship,
                               labels = c("No", "Yes")),
                      romantic_emotionally_close_fct = 
                        factorise(romantic_emotionally_close),
                      romantic_emotionally_close_fct = 
                        fct_recode(romantic_emotionally_close_fct,
                                   NULL = "Don't know"))


count(alspac_long, romantic_in_relationship_or_out_with_someone,
      romantic_in_relationship_or_out_with_someone_fct)
count(alspac_long, romantic_in_relationship, romantic_in_relationship_fct)
count(alspac_long, romantic_emotionally_close, romantic_emotionally_close_fct)



# religion ----------------
alspac_long %>% 
  select(starts_with("religion")) %>% 
  names()

alspac_long <- mutate(alspac_long,
                      religion_support_mother_fct = 
                        factor(religion_support_mother,
                               labels = c("No", "Yes")),
                      religion_support_partner_fct = 
                        factor(religion_support_partner,
                               labels = c("No", "Yes")))

count(alspac_long, religion_support_mother, religion_support_mother_fct)
count(alspac_long, religion_support_partner, religion_support_partner_fct)

# neighbour -------------------
alspac_long %>% 
  select(starts_with("neighbour")) %>% 
  names()


alspac_long <- alspac_long %>% 
  mutate_at(vars(neighbour_mother_neighourhood_opinion,
                 neighbour_burglary_worries_dad,
                 neighbour_mugging_worries_dad,
                 neighbour_sex_assault_pestering_worries_dad,
                 neighbour_vandalism_worries_dad,
                 neighbour_dad_lively_neighbourhood,
                 neighbour_dad_friendly_neighbourhood,
                 neighbour_dad_noisy_neighbourhood,
                 neighbour_dad_clean_neighbourhood,
                 neighbour_dad_attractive_neighbourhood,
                 neighbour_dad_polluted_neighbourhood),
            list("fct" = ~factorise(.)))

count(alspac_long, neighbour_stress_score) %>% print(n = 100)
qplot(alspac_long$neighbour_stress_score)

count(alspac_long, neighbour_mother_neighourhood_opinion,
      neighbour_mother_neighourhood_opinion_fct)

# to code and describe
# count(alspac_long, neighbour_burglary_worries_mother)
# count(alspac_long, neighbour_mugging_worries_mother)
# count(alspac_long, neighbour_sex_assault_pestering_worries_mother)
# count(alspac_long, neighbour_vandalism_worries_mother)
# count(alspac_long, neighbour_lively_neighbourhood)
# count(alspac_long, neighbour_friendly_neighbourhood)
# count(alspac_long, neighbour_noisy_neighbourhood)
# count(alspac_long, neighbour_clean_neighbourhood)
# count(alspac_long, neighbour_attractive_neighbourhood)
# count(alspac_long, neighbour_polluted_neighbourhood)
# count(alspac_long, neighbour_neighbourhood_index)
# count(alspac_long, neighbour_dads_opinion_of_neighbourhood)






count(alspac_long, neighbour_burglary_worries_dad,
      neighbour_burglary_worries_dad_fct)
count(alspac_long, neighbour_mugging_worries_dad,
      neighbour_mugging_worries_dad_fct)
count(alspac_long, neighbour_sex_assault_pestering_worries_dad,
      neighbour_sex_assault_pestering_worries_dad_fct)
count(alspac_long, neighbour_vandalism_worries_dad,
      neighbour_vandalism_worries_dad_fct)
count(alspac_long, neighbour_dad_lively_neighbourhood,
      neighbour_dad_lively_neighbourhood_fct)
count(alspac_long, neighbour_dad_friendly_neighbourhood,
      neighbour_dad_friendly_neighbourhood_fct)
count(alspac_long, neighbour_dad_noisy_neighbourhood,
      neighbour_dad_noisy_neighbourhood_fct)
count(alspac_long, neighbour_dad_clean_neighbourhood,
      neighbour_dad_clean_neighbourhood_fct)
count(alspac_long, neighbour_dad_attractive_neighbourhood,
      neighbour_dad_attractive_neighbourhood_fct)
count(alspac_long, neighbour_dad_polluted_neighbourhood,
      neighbour_dad_polluted_neighbourhood_fct)


# family ----
alspac_long %>% 
  select(starts_with("family")) %>% 
  names()

count(alspac_long, family_maternal_bond)
qplot(alspac_long$family_maternal_bond)

count(alspac_long, family_paternal_bond)
qplot(alspac_long$family_paternal_bond)

count(alspac_long, family_maternal_care)
qplot(alspac_long$family_maternal_care) # skewed

count(alspac_long, family_maternal_over_protect)
qplot(alspac_long$family_maternal_over_protect)

count(alspac_long, family_paternal_over_protect)
qplot(alspac_long$family_paternal_over_protect)

count(alspac_long, family_affection_of_partner)
qplot(alspac_long$family_affection_of_partner) 

count(alspac_long, family_affection_of_mother)
qplot(alspac_long$family_affection_of_mother) 

count(alspac_long, family_aggression_of_partner)
qplot(alspac_long$family_aggression_of_partner)

count(alspac_long, family_aggression_of_mother)
qplot(alspac_long$family_aggression_of_mother)

alspac_long <- alspac_long %>% 
  mutate(family_mother_satisfaction_with_partner = 
           ifelse(family_mother_satisfaction_with_partner == -2,
                  NA, 
                  family_mother_satisfaction_with_partner))

count(alspac_long, family_mother_satisfaction_with_partner)
qplot(alspac_long$family_mother_satisfaction_with_partner)

count(alspac_long, family_mother_satisfaction_with_partner_interim)
qplot(alspac_long$family_mother_satisfaction_with_partner_interim)

count(alspac_long, family_warmth_of_partner)
qplot(alspac_long$family_warmth_of_partner)


count(alspac_long, family_authority_of_partner)
qplot(alspac_long$family_authority_of_partner)


count(alspac_long, family_parent_child_rel_quality)
qplot(alspac_long$family_parent_child_rel_quality) # skewed


# bereavement ------

alspac_long <- alspac_long %>% 
  mutate(bereavement_death_in_family_since_fct = 
           factor(bereavement_death_in_family_since,
                  labels = c("No", "Yes")))

count(alspac_long, bereavement_death_in_family_since,
      bereavement_death_in_family_since_fct)

# life ----

count(alspac_long, life_score)
qplot(alspac_long$life_score)

# Export data -----------------------------------

write_rds(alspac_long, "./data/clean/alspac_long_v01.rds")




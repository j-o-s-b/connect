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

# load custom functions
list.files("./scripts/functions/", full.names = T) %>% 
  map(source)

# load data created in the first stage
alspac_r <- read_rds("./data/clean/alspac_reduced_20.12.22.rds")
alspac_r_20.01.23 <- read_rds("./data/clean/alspac_reduced_20.01.23.rds")

# new time constant predictors
alspac_r_constant <- alspac_r_20.01.23 %>% 
  select(uniqid, SDQ_total_difficulties_age_6y9m,
         mother_education_age_gest,
         adverse_life_events_score_age_4_to_8,
         adverse_life_events_score_to_age_3)


# explore data
glimpse(alspac_r)

# Reshape data in long ----------------------------------

alspac_r_small <- alspac_r %>% 
  select(uniqid, starts_with("anxiety"), starts_with("bullying"),
         starts_with("bereavement"), starts_with("depression"), 
         starts_with("family"), starts_with("friends"),
         starts_with("childcare"),
         starts_with("school"), starts_with("parent"),
         starts_with("peer"), starts_with("social"), starts_with("sibling"),
         starts_with("romantic"), starts_with("relationship"),
         starts_with("religion"), starts_with("neighbour"),
         starts_with("life_event_moved_home"),
         -starts_with("school_enjoys"), -contains("inconsistent"))


# rename childcare_frequency_child_sees_grandparents_age_5y5m to 
# shift to next year
alspac_r_small <- rename(alspac_r_small,
                         childcare_frequency_child_sees_grandparents_age_5y7m =
                           childcare_frequency_child_sees_grandparents_age_5y5m)


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


# make coding of home moving consistent
alspac_r_small2 <- alspac_r_small2 %>% 
  mutate_at(vars(`life_event_moved_home~1`, `life_event_moved_home~2`,
                 `life_event_moved_home~3`, `life_event_moved_home~16`),
            ~case_when(. == 1 ~ 1, . == 2 ~ 0)) %>% 
  mutate_at(vars(`life_event_moved_home~6`, `life_event_moved_home~23`,
                 `life_event_moved_home~24`),
            ~case_when(. == "Yes" ~ 1, . == "No" ~ 0))

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


## Outcomes ------------

# select only outcomes
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

# describe change in time
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



## Key predictors ------

### Bullying -----------------

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



             
### Friends ---------------

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



# new variables developed 09.12.2022

# make factors
alspac_long <- alspac_long %>% 
  mutate_at(vars(friends_yps_friends_fall_out_with_them,
                 friends_friends_support_yp_when_they_need_them,
                 friends_friends_put_yp_down_in_front_of_others,
                 friends_friends_make_yp_feel_confident,
                 friends_child_has_one_good_friend_last_six_months,
                 friends_ch_one_good_friend_past,
                 friends_yp_has_a_best_friend),
            list("fct" = ~factorise(.))) %>% 
  mutate(friends_yp_has_a_best_friend_fct = 
           fct_recode(friends_yp_has_a_best_friend_fct, 
                      NULL = "Don't know"))


count(alspac_long, friends_yps_friends_fall_out_with_them,
      friends_yps_friends_fall_out_with_them_fct)
count(alspac_long, friends_friends_support_yp_when_they_need_them,
      friends_friends_support_yp_when_they_need_them_fct)
count(alspac_long, friends_friends_put_yp_down_in_front_of_others,
      friends_friends_put_yp_down_in_front_of_others_fct)
count(alspac_long, friends_friends_make_yp_feel_confident,
      friends_friends_make_yp_feel_confident_fct)
count(alspac_long, friends_child_has_one_good_friend_last_six_months,
      friends_child_has_one_good_friend_last_six_months_fct)
count(alspac_long, friends_ch_one_good_friend_past,
      friends_ch_one_good_friend_past_fct)
count(alspac_long, friends_yp_has_a_best_friend,
      friends_yp_has_a_best_friend_fct)

count(alspac_long, friends_whether_any_close_friend)



### School -------------------

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



count(alspac_long, school_schoolmates_accept_them,
      school_schoolmates_accept_them_fct)
count(alspac_long, school_feel_lonely,
      school_feel_lonely_fct)
count(alspac_long, school_people_trust_them, school_people_trust_them_fct)
count(alspac_long, school_people_depend_on_them, 
      school_people_depend_on_them_fct)




### Parents ---------------


# descriptives
qplot(alspac_long$parent_networks_mother_social_support_score)
qplot(alspac_long$parent_networks_partner_social_networks_score)
qplot(alspac_long$parent_networks_partner_social_support_score)



### Peer problem --------------

count(alspac_long, peer_problem_score)
qplot(alspac_long$peer_problem_score) # skewed


### Social cohesion -------------
count(alspac_long, social_cohesion)
qplot(alspac_long$social_cohesion)

# lots of negative values
count(alspac_long, social_discord)





### Romantic ----------------

# list variables
alspac_long %>% 
  select(starts_with("romantic")) %>% 
  names()

# recode
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

# describe
count(alspac_long, romantic_in_relationship_or_out_with_someone,
      romantic_in_relationship_or_out_with_someone_fct)
count(alspac_long, romantic_in_relationship, romantic_in_relationship_fct)
count(alspac_long, romantic_emotionally_close, romantic_emotionally_close_fct)



### Religion ----------------

# get names
alspac_long %>% 
  select(starts_with("religion")) %>% 
  names()

# recode
alspac_long <- mutate(alspac_long,
                      religion_support_mother_fct = 
                        factor(religion_support_mother,
                               labels = c("No", "Yes")),
                      religion_support_partner_fct = 
                        factor(religion_support_partner,
                               labels = c("No", "Yes")))

# check
count(alspac_long, religion_support_mother, religion_support_mother_fct)
count(alspac_long, religion_support_partner, religion_support_partner_fct)

### Neighbor -------------------

# get names
alspac_long %>% 
  select(starts_with("neighbour")) %>% 
  names()

# describe
count(alspac_long, neighbour_stress_score) %>% print(n = 100)
qplot(alspac_long$neighbour_stress_score)




### Family ----------------

# get variables
alspac_long %>% 
  select(starts_with("family")) %>% 
  names()


# describe
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


# new vars developed on 09.12.2022

# continious
# family_overall_parenting_score
# family_partner_parenting_score
# family_mother_parenting_score
# family_female_parenting_score
# family_male_parenting_score

count(alspac_long, family_overall_parenting_score)
qplot(alspac_long$family_overall_parenting_score)

count(alspac_long, family_partner_parenting_score)
qplot(alspac_long$family_partner_parenting_score)

count(alspac_long, family_mother_parenting_score)
qplot(alspac_long$family_mother_parenting_score)

count(alspac_long, family_female_parenting_score)
qplot(alspac_long$family_female_parenting_score)

count(alspac_long, family_male_parenting_score)
qplot(alspac_long$family_male_parenting_score)

# categorical
# family_how_close_yp_feels_to_their_siblings
# family_frequency_child_visits_relatives_age
# family_child_sees_grandparents
# family_child_gets_on_well_with_rest_of_family
# family_child_sees_his_or_her_grandparents

# make factors
alspac_long <- alspac_long %>% 
  mutate_at(vars(family_how_close_yp_feels_to_their_siblings,
                 family_frequency_child_visits_relatives,
                 family_child_sees_grandparents,
                 family_child_gets_on_well_with_rest_of_family,
                 family_child_sees_his_or_her_grandparents),
            list("fct" = ~factorise(.)))


# describe
count(alspac_long, family_how_close_yp_feels_to_their_siblings,
      family_how_close_yp_feels_to_their_siblings_fct)

count(alspac_long, family_frequency_child_visits_relatives,
      family_frequency_child_visits_relatives_fct)

count(alspac_long, family_child_sees_grandparents,
      family_child_sees_grandparents_fct)

count(alspac_long, family_child_gets_on_well_with_rest_of_family,
      family_child_gets_on_well_with_rest_of_family_fct)

count(alspac_long, family_child_sees_his_or_her_grandparents,
      family_child_sees_his_or_her_grandparents_fct)



### Childcare ------------------------------

# recode variables
alspac_long <- alspac_long %>%
  mutate_at(
    vars(
      childcare_grandparent_looks_after_ch,
      childcare_other_rel_looks_after_ch,
      childcare_friend_or_neighbour_looks_after_ch,
      childcare_gdprt_looks_after_ch,
      childcare_frd_or_neighbour_looks_after_ch
    ),
    list("fct" = ~ factor(., labels = c("No", "Yes")))
  ) %>%
  mutate(
    childcare_frequency_child_sees_grandparents_fct =
      factorise(childcare_frequency_child_sees_grandparents),
    childcare_frequency_child_visits_relatives_fct =
      factorise(childcare_frequency_child_visits_relatives)
  )

# describe
count(alspac_long, childcare_grandparent_looks_after_ch,
      childcare_grandparent_looks_after_ch_fct)
count(alspac_long, childcare_other_rel_looks_after_ch,
      childcare_other_rel_looks_after_ch_fct)
count(alspac_long, childcare_friend_or_neighbour_looks_after_ch,
      childcare_friend_or_neighbour_looks_after_ch_fct)
count(alspac_long, childcare_gdprt_looks_after_ch,
      childcare_gdprt_looks_after_ch_fct)
count(alspac_long, childcare_frd_or_neighbour_looks_after_ch,
      childcare_frd_or_neighbour_looks_after_ch_fct)

count(alspac_long, childcare_frequency_child_sees_grandparents)
count(alspac_long, childcare_frequency_child_visits_relatives)


### Relationship -----------------

# make factors
alspac_long <- alspac_long %>% 
  mutate_at(vars(relationship_to_parents_how_close_to_parents,
                 relationship_to_parents_partner_really_loves_ch,
                 relationship_to_parents_partner_loves_study_child,
                 relationship_to_parents_mum_really_loves_toddler,
                 relationship_to_parents_partner_really_loves_child,
                 relationship_to_parents_mum_really_loves_ch,
                 relationship_to_parents_mother_really_loves_child,
                 relationship_to_parents_mother_close_to_child,
                 relationship_to_parents_partner_close_to_child,
                 relationship_to_parents_partner_very_close_to_study_child,
                 relationship_to_parents_partner_very_close_child,
                 relationship_to_parents_mothers_partner_close_to_study_child,
                 relationship_to_parents_mother_loves_study_child),
            list("fct" = ~factorise(.))) %>% 
  mutate(relationship_to_parents_how_close_to_parents_fct = 
           fct_recode(relationship_to_parents_how_close_to_parents_fct, 
                      NULL = "No parents"),
         relationship_to_parents_partner_really_loves_ch_fct = 
           fct_recode(relationship_to_parents_partner_really_loves_ch_fct, 
                      NULL = "No Partner"),
         relationship_to_parents_partner_loves_study_child_fct = 
           fct_recode(relationship_to_parents_partner_loves_study_child_fct, 
                      NULL = "No partner"))

# describe

count(alspac_long, relationship_to_parents_how_close_to_parents,
      relationship_to_parents_how_close_to_parents_fct)
count(alspac_long, relationship_to_parents_partner_really_loves_ch,
      relationship_to_parents_partner_really_loves_ch_fct)
count(alspac_long, relationship_to_parents_partner_loves_study_child,
      relationship_to_parents_partner_loves_study_child_fct)


count(alspac_long, relationship_to_parents_mum_really_loves_toddler,
      relationship_to_parents_mum_really_loves_toddler_fct)
count(alspac_long, relationship_to_parents_partner_really_loves_child,
      relationship_to_parents_partner_really_loves_child_fct)
count(alspac_long, relationship_to_parents_mum_really_loves_ch,
      relationship_to_parents_mum_really_loves_ch_fct)
count(alspac_long, relationship_to_parents_mother_really_loves_child,
      relationship_to_parents_mother_really_loves_child_fct)
count(alspac_long, relationship_to_parents_mother_close_to_child,
      relationship_to_parents_mother_close_to_child_fct)
count(alspac_long, relationship_to_parents_partner_close_to_child,
      relationship_to_parents_partner_close_to_child_fct)
count(alspac_long, relationship_to_parents_partner_very_close_to_study_child,
      relationship_to_parents_partner_very_close_to_study_child_fct)
count(alspac_long, relationship_to_parents_partner_very_close_child,
      relationship_to_parents_partner_very_close_child_fct)
count(alspac_long, relationship_to_parents_mothers_partner_close_to_study_child,
      relationship_to_parents_mothers_partner_close_to_study_child_fct)
count(alspac_long, relationship_to_parents_mother_loves_study_child,
      relationship_to_parents_mother_loves_study_child_fct)




count(alspac_long, relationship_to_parents_partner_positive_relationship_score)
qplot(alspac_long$relationship_to_parents_partner_positive_relationship_score)

count(alspac_long, relationship_to_parents_partner_negative_relationship_score)
qplot(alspac_long$relationship_to_parents_partner_negative_relationship_score)


count(alspac_long, relationship_to_parents_partner_interaction_score)
qplot(alspac_long$relationship_to_parents_partner_interaction_score)

count(alspac_long, relationship_to_parents_mother_interaction_score)
qplot(alspac_long$relationship_to_parents_mother_interaction_score)



### Social -------

# exclude problematic variables
alspac_long <- select(alspac_long,
                      -social_conflict_child_afraid_of_other_relative,
                      -social_conflict_child_afraid_of_father,
                      -social_conflict_child_afraid_of_mother,
                      -social_conflict_child_afraid_of_other_children)

social_vars <- alspac_long %>% 
  select(starts_with("social_"), 
         -starts_with("social_cohesion"),
         -starts_with("social_discord")) %>% 
  names()

# make factors
alspac_long <- alspac_long %>% 
  mutate_at(vars(social_vars),
            list("fct" = ~ factorise(.)))

# describe
count(alspac_long, social_interaction_times_child_taken_to_friends_family,
      social_interaction_times_child_taken_to_friends_family_fct)
count(alspac_long, social_interaction_frequency_child_visits_family_friends,
      social_interaction_frequency_child_visits_family_friends_fct)
count(alspac_long, social_conflict_child_quarrels_with_older_children,
      social_conflict_child_quarrels_with_older_children_fct)
count(alspac_long, social_conflict_child_quarrels_with_twin,
      social_conflict_child_quarrels_with_twin_fct)
count(alspac_long, social_interaction_child_affectionate_to_younger_sibs,
      social_interaction_child_affectionate_to_younger_sibs_fct)
count(alspac_long, 
      social_interaction_frequency_child_plays_with_children_not_sibs,
      social_interaction_frequency_child_plays_with_children_not_sibs_fct)
count(alspac_long, social_interaction_frequency_child_visits_friends_family,
      social_interaction_frequency_child_visits_friends_family_fct)
count(alspac_long, social_interaction_frequency_child_plays_w_other_children,
      social_interaction_frequency_child_plays_w_other_children_fct)
count(alspac_long, social_interaction_child_plays_with_children_not_sibs,
      social_interaction_child_plays_with_children_not_sibs_fct)
count(alspac_long, social_support_frequency_feeling_close_to_people,
      social_support_frequency_feeling_close_to_people_fct)
count(alspac_long, social_support_frequency_feeling_loved,
      social_support_frequency_feeling_loved_fct)
count(alspac_long, social_conflict_mealtime_arguments_between_kids,
      social_conflict_mealtime_arguments_between_kids_fct)
count(alspac_long, social_conflict_mealtime_arguments_between_kids_and_adults,
      social_conflict_mealtime_arguments_between_kids_and_adults_fct)
count(alspac_long, social_conflict_mealtime_arguments_between_adults,
      social_conflict_mealtime_arguments_between_adults_fct)
count(alspac_long, social_interaction_frequency_child_visits_friends,
      social_interaction_frequency_child_visits_friends_fct)
count(alspac_long, social_interaction_frq_child_plays_w_children_outside_school,
      social_interaction_frq_child_plays_w_children_outside_school_fct)
count(alspac_long, social_network_child_has_one_good_friend_past,
      social_network_child_has_one_good_friend_past_fct)



### Siblings ------

# recode
alspac_long <- alspac_long %>% 
  mutate(sibling_interaction_child_argues_with_siblings_fct = 
           factorise(sibling_interaction_child_argues_with_siblings),
         sibling_interaction_child_argues_with_older_child_fct = 
           factorise(sibling_interaction_child_argues_with_older_child))

# check
count(alspac_long, sibling_interaction_child_argues_with_siblings,
      sibling_interaction_child_argues_with_siblings_fct)
count(alspac_long, sibling_interaction_child_argues_with_older_child,
      sibling_interaction_child_argues_with_older_child_fct)

count(alspac_long, sibling_interaction_sibling_interaction_score)
qplot(alspac_long$sibling_interaction_sibling_interaction_score) +
  theme_bw()




# Control variables clean ----------------------------------


control_data <- read_rds("./data/clean/alspac_control_variables_05.01.23.rds")
glimpse(control_data)
 

## Make codebook ----
codebook <- map_df(control_data, function(x) attributes(x)$label) %>% 
  gather(key = name, value = label)
   
View(codebook)


## Recode socio-dems ----------

### Sex ---------

control_data <- control_data %>% 
  mutate(female = case_when(kz021 == 1 ~ 0,
                            kz021 == 2 ~ 1),
         sex_fct = factor(female, labels = c("Male", "Female")))

# check
count(control_data, kz021, female, sex_fct)

### Ethnicity ------------------ 

# c804, YPH2010, YPH2010, YPH2012

# recode
control_data <- control_data %>% 
  mutate(white = case_when(c804 == 1 ~ 1,
                           c804 == 2 ~ 0),
         white = case_when(is.na(white) & YPH2012 == 1 ~ 1,
                           is.na(white) & YPH2012 == 2 ~ 0,
                           TRUE ~ white))

# check
control_data %>% 
  filter(baseline_age_7y9m == 1) %>% 
  count(white, c804, YPH2012)


### Education needs ---------

# sa030, se030

# recode
control_data <- control_data %>% 
  mutate(special_edu_needs = case_when(sa030 == 1 ~ 1,
                                       sa030 == 2 ~ 0),
         special_edu_needs = 
           case_when((is.na(special_edu_needs) | special_edu_needs == 0) & 
                                                se030 == 1 ~ 1,
                     is.na(special_edu_needs) & se030 == 2 ~ 0,
                     TRUE ~ special_edu_needs))


# check
count(control_data, special_edu_needs, sa030)
count(control_data, special_edu_needs, se030)

count(control_data, special_edu_needs, sa030, se030) %>% print(n = 30)


### Mother's education -----------

# recode
control_data <- control_data %>% 
  mutate(mom_edu_fct = case_when(c645 %in% 1:3 ~ "Other",
                                 c645 == 4 ~ "A level",
                                 c645 == 5 ~ "Degree"),
         mom_edu_fct = as.factor(mom_edu_fct),
         mom_degree = ifelse(mom_edu_fct == "Degree", 1, 0))


# check
count(control_data, c645, mom_edu_fct, mom_degree)


control_data %>% 
  filter(baseline_age_7y9m == 1) %>% 
  count(mom_edu_fct)


### Mother's marital status and employment --------------

# recode
control_data <- control_data %>% 
  mutate(mom_partner = case_when(j370 %in% 1:4 ~ 0,
                                 j370 %in% 5:6 ~ 1),
         mom_work = case_when(c710 == 1 ~ 1,
                              c710 == 2 ~ 0))

# check
control_data %>% 
  filter(baseline_age_7y9m == 1) %>% 
  count(mom_partner)

control_data %>% 
  filter(baseline_age_7y9m == 1) %>% 
  count(mom_work)


### IMD ----------------

# check
control_data %>% 
  select(matches("imd")) %>% 
  mutate_all(~ifelse(. < 0, NA, .)) %>% 
  summarise_all(~mean(is.na(.))) %>% 
  gather() %>% 
  arrange(value)

count(control_data, tf1imd2000q5)
count(control_data, kfimd2000q5)

# recode
control_data <- control_data %>% 
  mutate_at(vars(starts_with("kaimd")),
            ~ifelse(. < 0, NA, .)) %>% 
  rename_all(~str_remove_all(., "ka|q5"))


### Health -------------

# kr010, ks1000, kv1000, kw1000, tb1000

# check
health_vars <- c("kr010", "ks1000", "kv1000", "kw1000", "tb1000")

count(control_data, kr010)
count(control_data, ks1000)
count(control_data, kv1000)
count(control_data, kw1000)
count(control_data, tb1000)

# recode
control_data <- control_data %>% 
  mutate(health_8 = kr010,
         health_9 = ks1000,
         health_11 = kv1000,
         health_12 = kw1000,
         health_14 = tb1000) %>% 
  mutate_at(vars(starts_with("health")),
           ~ifelse(. < 0, NA, .)) %>%
  mutate_at(vars(starts_with("health")),
            ~4 -.) %>% 
  mutate_at(vars(starts_with("health")),
            list("fct" = ~factor(., 
                                 labels = c("Almost always unwell",
                                            "Sometimes quite ill",
                                            "Healthy, but a few minor problems",
                                            "Very healthy, no problems")))) %>%
  mutate(healthy_very_8 = ifelse(health_8 == 3, 1, 0),
         healthy_very_9 = ifelse(health_9 == 3, 1, 0),
         healthy_very_11 = ifelse(health_11 == 3, 1, 0),
         healthy_very_12 = ifelse(health_12 == 3, 1, 0),
         healthy_very_14 = ifelse(health_14 == 3, 1, 0)) 


# - d171a (mother severe depression) 
# - sibling kq653 (-2 indicate absence of sibling)

# recode
control_data <- mutate(control_data,
                       mom_depression = case_when(d171a == 1 ~ 1,
                                                  d171a == 2 ~ 0),
                       siblings = case_when(kq653 == -2 | kq653 == 0 ~ 0,
                                            kq653 > 0 ~ 1))

# check
count(control_data, d171a, mom_depression)
count(control_data, kq653, siblings) %>% print(n = 50)



### Change school --------------

# kt5014 (9)
# ccs2030 (17)

count(control_data, kt5014)
count(control_data, ccs2030)

# recode
control_data <- mutate(control_data,
                       change_school_9 = case_when(kt5014 %in% 1:4 ~ 1,
                                                   kt5014 == 5 ~ 0),
                       change_school_17 = case_when(ccs2030 == 1 ~ 1,
                                                    ccs2030 == 2 ~ 0))

# check
count(control_data, kt5014, change_school_9)
count(control_data, ccs2030, change_school_17)



## Select variables of interest ------------------
control_data_small <- select(control_data, 
                             uniqid, baseline_age_7y9m, female,
                             white, special_edu_needs, mom_edu_fct, 
                             mom_degree, mom_partner, mom_work, 
                             starts_with("imd"), starts_with("health"),
                             mom_depression, siblings, 
                             change_school_9, change_school_17)

# Merge control and long data ----------------

alspac_long <- left_join(alspac_long, control_data_small, by = "uniqid")

# Recode new key variables for analysis ----------------------

## Bully ---------------


### make bully time constant

# check
count(alspac_long, age, bullying_experienced) %>% 
  na.omit()
count(alspac_long, age, bullying_victim) %>% 
  na.omit()

# recode
alspac_long <- alspac_long %>% 
  group_by(uniqid) %>% 
  mutate(bullied_8 = ifelse(bullying_victim == 1 & age == 8, 1, 0),
         bullied_8 = ifelse(is.na(bullying_victim) | age != 8, NA, bullied_8),
         bullied_8 = max(bullied_8, na.rm = T),
         bullied_8 = ifelse(is.infinite(bullied_8), NA, bullied_8),
         bullied_10 = ifelse(bullying_victim == 1 & age == 10, 1, 0),
         bullied_10 = ifelse(is.na(bullying_victim) & age != 10, NA, bullied_10),
         bullied_10 = max(bullied_10, na.rm = T),
         bullied_10 = ifelse(is.infinite(bullied_10), NA, bullied_10)) %>% 
  ungroup()

# check
alspac_long %>% 
  count(age, bullying_victim, bullied_8) %>% print(n = 100)

alspac_long %>% 
  count(age, bullying_victim, bullied_10) %>% print(n = 100)

#### frequent bullied

# recode
alspac_long <- alspac_long %>% 
  mutate(bullied_8_freq = ifelse(bullying_frequency_bullied < 3 , 1, 0),
         bullied_8_freq = ifelse(is.na(bullying_frequency_bullied), 
                                 NA, bullied_8_freq)) %>% 
  group_by(uniqid) %>% 
  mutate(bullied_8_freq = max(bullied_8_freq, na.rm = T),
         bullied_8_freq = ifelse(is.infinite(bullied_8_freq), 
                                 NA, bullied_8_freq)) %>% 
  ungroup()

# check
alspac_long %>% 
  count(age, bullying_frequency_bullied, bullied_8_freq) %>% 
  print(n = 100)


## School likes teacher ---------

# recode
alspac_long <- alspac_long %>% 
  mutate(school_likes_teacher_rev = 4 - school_likes_teacher) %>% 
  group_by(uniqid) %>% 
  mutate(school_likes_teacher_rev_avg = mean(school_likes_teacher_rev, 
                                             na.rm = T),
         school_likes_teacher_always = 
           ifelse(school_likes_teacher_rev_avg == 3, 1, 0)) %>% 
  ungroup() 

# describe
alspac_long %>% 
  count(age, school_likes_teacher, school_likes_teacher_rev, 
        school_likes_teacher_always) %>% 
  na.omit()

qplot(alspac_long$school_likes_teacher_rev_avg)

## School get on with colleagues -------

# recode
alspac_long <- alspac_long %>% 
  mutate(school_get_on_with_classmates = 
           ifelse(school_get_on_with_classmates == 9, 
                  NA, 
                  school_get_on_with_classmates),
           school_get_on_with_classmates_rev = 
            4 - school_get_on_with_classmates) %>% 
  group_by(uniqid) %>% 
  mutate(school_get_on_with_classmates_avg = 
           mean(school_get_on_with_classmates_rev, na.rm = T),
         school_get_on_with_classmates_always = 
           ifelse(school_get_on_with_classmates_avg == 3, 1, 0)) %>% 
  ungroup() 

# describe
alspac_long %>% 
  count(school_get_on_with_classmates, school_get_on_with_classmates_rev,
        school_get_on_with_classmates_always) 


qplot(alspac_long$school_get_on_with_classmates_avg)


## School feel lonely ----------

# recode
alspac_long <- alspac_long %>% 
  mutate(school_feel_lonely = 
           ifelse(school_feel_lonely == 9, 
                  NA, 
                  school_feel_lonely),
         school_feel_lonely_rev = 
           4 - school_feel_lonely) %>% 
  group_by(uniqid) %>% 
  mutate(school_feel_lonely_avg = 
           mean(school_feel_lonely_rev, na.rm = T),
         school_feel_lonely_never = 
           ifelse(school_feel_lonely_avg == 0, 1, 0),
         school_feel_lonely_never_11 = mean(school_feel_lonely_never, 
                                            na.rm = T),
         school_feel_lonely_never_11 = ifelse(
           school_feel_lonely_never_11 %in% 0:1 & age < 11, 0, 
           school_feel_lonely_never_11
         ),
         school_feel_lonely_never_14 = mean(school_feel_lonely_never, 
                                            na.rm = T),
         school_feel_lonely_never_14 = ifelse(
           school_feel_lonely_never_14 %in% 0:1 & age < 14, 0, 
           school_feel_lonely_never_14
         )) %>% 
  ungroup() 


# check
alspac_long %>% 
  count(school_feel_lonely, school_feel_lonely_rev, school_feel_lonely_never) 


alspac_long %>% 
  count(age, school_feel_lonely_never, 
        school_feel_lonely_never_14) %>% 
  filter(age %in% c(11, 14))


qplot(alspac_long$school_feel_lonely_avg)

## Friend score, peer problem score, friends play with kids --------

# recode
alspac_long <- alspac_long %>% 
  mutate(friends_plays_with_kids_rev = 5 - friends_plays_with_kids) %>% 
  group_by(uniqid) %>% 
  mutate(friends_score_avg = mean(friends_score, na.rm = T),
         peer_problem_score_avg = mean(peer_problem_score, na.rm = T),
         friends_plays_with_kids_rev_stab = 
           mean(friends_plays_with_kids_rev, na.rm = T)) %>% 
  ungroup() 

# check
count(alspac_long, friends_plays_with_kids)

## Romantic ----------

# recode
alspac_long <- alspac_long %>% 
  group_by(uniqid) %>% 
  mutate(romantic_13 = 
           mean(romantic_in_relationship_or_out_with_someone, na.rm = T),
         romantic_13 = ifelse(age < 13, 0, romantic_13),
         romantic_16 = 
           ifelse(age == 16, romantic_in_relationship, NA),
         romantic_16 = mean(romantic_16, na.rm = T),
         romantic_16 = ifelse(age < 16, 0, romantic_16),
         romantic_17 = 
           ifelse(age == 17, romantic_in_relationship, NA),
         romantic_17 = mean(romantic_17, na.rm = T),
         romantic_17 = ifelse(age < 17, 0, romantic_17),
         romantic_24 = 
           ifelse(age == 24, romantic_in_relationship, NA),
         romantic_24 = mean(romantic_24, na.rm = T),
         romantic_24 = ifelse(age < 24, 0, romantic_24)) %>% 
  ungroup()


# check
count(alspac_long, 
      age, romantic_in_relationship_or_out_with_someone, romantic_13) %>% 
  print(n = 100)

count(alspac_long, age, 
      romantic_in_relationship, romantic_16, romantic_17, romantic_24) %>% 
  na.omit() %>% print(n = 100)


## Family bond -------------

# recode
alspac_long <- alspac_long %>% 
  group_by(uniqid) %>% 
  mutate(family_maternal_bond_1 = ifelse(age == 1, family_maternal_bond, NA),
         family_maternal_bond_1 = mean(family_maternal_bond_1, na.rm = T),
         family_maternal_bond_3 = ifelse(age == 3, family_maternal_bond, NA),
         family_maternal_bond_3 = mean(family_maternal_bond_3, na.rm = T),
         family_paternal_bond_1 = ifelse(age == 1, family_paternal_bond, NA),
         family_paternal_bond_1 = mean(family_paternal_bond_1, na.rm = T),
         family_paternal_bond_2 = ifelse(age == 2, family_paternal_bond, NA),
         family_paternal_bond_2 = mean(family_paternal_bond_2, na.rm = T),
         family_mother_satisfaction_with_partner_avg =
           mean(family_mother_satisfaction_with_partner, na.rm = T),
         parent_networks_mother_social_support_score_avg =
           mean(parent_networks_mother_social_support_score, na.rm = T),
         childcare_grandparent_looks_after_ch = 
           childcare_grandparent_looks_after_ch - 1,
         childcare_grandparent_looks_after_ch_0 =
           mean(childcare_grandparent_looks_after_ch, na.rm = T)) %>% 
  ungroup()

# check
alspac_long %>% 
  arrange(uniqid, age) %>% 
  select(uniqid, age, 
         family_maternal_bond, family_maternal_bond_1, family_maternal_bond_3) %>% 
  print(n = 300)

count(alspac_long, childcare_grandparent_looks_after_ch,
      childcare_grandparent_looks_after_ch_0)

## Religion, social cohesion and social discord -------------

# recode
alspac_long <- alspac_long %>% 
  group_by(uniqid) %>% 
  mutate(religion_support_mother_avg = mean(religion_support_mother, na.rm = T),
         social_cohesion_avg = mean(social_cohesion, na.rm = T),
         social_discord_avg = mean(social_discord, na.rm = T),
         social_cohesion_avg_0_3 = ifelse(age < 4, social_cohesion, NA),
         social_cohesion_avg_0_3 = mean(social_cohesion_avg_0_3, na.rm = T),
         social_cohesion_avg_5_7 = ifelse(age %in% 5:7, social_cohesion, NA),
         social_cohesion_avg_5_7 = mean(social_cohesion_avg_5_7, na.rm = T),
         social_cohesion_avg_10 = ifelse(age == 10, social_cohesion, NA),
         social_cohesion_avg_10 = mean(social_cohesion_avg_10, na.rm = T),
         social_cohesion_avg_18 = ifelse(age == 18, social_cohesion, NA),
         social_cohesion_avg_18 = mean(social_cohesion_avg_18, na.rm = T),
         
         social_discord_avg_0_3 = ifelse(age < 4, social_discord, NA),
         social_discord_avg_0_3 = mean(social_discord_avg_0_3, na.rm = T),
         social_discord_avg_5_7 = ifelse(age %in% 5:7, social_discord, NA),
         social_discord_avg_5_7 = mean(social_discord_avg_5_7, na.rm = T),
         social_discord_avg_10 = ifelse(age == 10, social_discord, NA),
         social_discord_avg_10 = mean(social_discord_avg_10, na.rm = T),
         social_discord_avg_18 = ifelse(age == 18, social_discord, NA),
         social_discord_avg_18 = mean(social_discord_avg_18, na.rm = T)) %>% 
  ungroup()

# check
alspac_long %>% 
  arrange(uniqid, age) %>% 
  select(uniqid, age, social_cohesion, social_cohesion_avg_0_3, 
         social_cohesion_avg_18) %>% 
  print(n = 500)


## Moved home ----------- 

count(alspac_long, age, life_event_moved_home) %>% print(n = 100)

# recode
alspac_long <- alspac_long %>% 
  group_by(uniqid) %>% 
  mutate(move_home_before8 = ifelse(age < 8, life_event_moved_home, 0),
         move_home_before8 = max(move_home_before8, na.rm = T),
         move_home_1 = ifelse(age == 1, life_event_moved_home, NA),
         move_home_1 = mean(move_home_1, na.rm = T),
         move_home_2 = ifelse(age == 2, life_event_moved_home, NA),
         move_home_2 = mean(move_home_2, na.rm = T),
         move_home_3 = ifelse(age == 3, life_event_moved_home, NA),
         move_home_3 = mean(move_home_3, na.rm = T),
         move_home_6 = ifelse(age == 6, life_event_moved_home, NA),
         move_home_6 = mean(move_home_6, na.rm = T),
         move_home_16 = ifelse(age == 16, life_event_moved_home, NA),
         move_home_16 = mean(move_home_16, na.rm = T),
         move_home_23 = ifelse(age == 23, life_event_moved_home, NA),
         move_home_23 = mean(move_home_23, na.rm = T),
         move_home_24 = ifelse(age == 24, life_event_moved_home, NA),
         move_home_24 = mean(move_home_24, na.rm = T)) %>% 
  ungroup()


# check
count(alspac_long, age, life_event_moved_home, move_home_before8) %>% 
  print(n = 100)
count(alspac_long, age, life_event_moved_home, move_home_16) %>% 
  print(n = 100)


## Piecemeal friends, peer and parent networks ------------

# - friends_score_avg: 8-10, 12-13, exclude 17
# - peer_problem: 7-10, 12-13, exclude 16
# - parent_networks_mother_social_support_score_avg: -1 to 6, 9, 12

# recode
alspac_long <- alspac_long %>% 
  group_by(uniqid) %>% 
  mutate(friends_score_avg_8_10 = ifelse(age < 11, friends_score, NA),
         friends_score_avg_8_10 = mean(friends_score_avg_8_10, na.rm = T),
         friends_score_avg_12_13 = ifelse(age %in% 11:13, friends_score, NA),
         friends_score_avg_12_13 = mean(friends_score_avg_12_13, na.rm = T),
         
         peer_problem_avg_7_10 = ifelse(age < 11,  peer_problem_score, NA),
         peer_problem_avg_7_10 = mean(peer_problem_avg_7_10, na.rm = T),
         peer_problem_avg_12_13 = ifelse(age %in% 11:13, peer_problem_score, NA),
         peer_problem_avg_12_13 = mean(peer_problem_avg_12_13, na.rm = T),
         
         parent_networks_mother_social_support_score_avg_0_6 = 
           ifelse(age < 7, parent_networks_mother_social_support_score, NA),
         parent_networks_mother_social_support_score_avg_0_6 = 
           mean(parent_networks_mother_social_support_score_avg_0_6, na.rm = T),
         parent_networks_mother_social_support_score_avg_9 = 
           ifelse(age == 9, parent_networks_mother_social_support_score, NA),
         parent_networks_mother_social_support_score_avg_9 = 
           mean(parent_networks_mother_social_support_score_avg_9, na.rm = T),
         parent_networks_mother_social_support_score_avg_12 = 
           ifelse(age == 12, parent_networks_mother_social_support_score, NA),
         parent_networks_mother_social_support_score_avg_12 = 
           mean(parent_networks_mother_social_support_score_avg_12, na.rm = T),
         ) %>% 
  ungroup()

# check coding
alspac_long %>% 
  arrange(uniqid, age) %>% 
  select(uniqid, age, friends_score, friends_score_avg_8_10, 
         friends_score_avg_12_13) %>% 
  print(n = 500)

alspac_long %>% 
  arrange(uniqid, age) %>% 
  select(uniqid, age, peer_problem_score, peer_problem_avg_7_10, 
         peer_problem_avg_12_13) %>% 
  print(n = 500)

alspac_long %>% 
  arrange(uniqid, age) %>% 
  select(uniqid, age, parent_networks_mother_social_support_score, 
         parent_networks_mother_social_support_score_avg_0_6, 
         parent_networks_mother_social_support_score_avg_9,
         parent_networks_mother_social_support_score_avg_12) %>% 
  print(n = 500)



# Add time constant predictors -----------------------------

alspac_long <- left_join(alspac_long, alspac_r_constant,
                         by = "uniqid")

# Export data -----------------------------------

write_rds(alspac_long, "./data/clean/alspac_long_v02.rds")




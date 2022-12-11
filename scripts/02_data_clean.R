

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
alspac_r <- read_rds("./data/clean/alspac_reduced_09.12.22.rds")

# explore data
glimpse(alspac_r)

# Reshape data in long ----------------------------------

alspac_r_small <- alspac_r %>% 
  select(uniqid, starts_with("anxiety"), starts_with("bullying"),
         starts_with("bereavement"), starts_with("depression"), 
         starts_with("family"), starts_with("friends"),
         starts_with("life"), starts_with("childcare"),
         starts_with("school"), starts_with("parent"),
         starts_with("peer"), starts_with("social"), starts_with("sibling"),
         starts_with("romantic"), starts_with("relationship"),
         starts_with("religion"), starts_with("neighbour"),
         -starts_with("school_enjoys"), -contains("inconsistent"))


# rename life score so we can reshape it to long
alspac_r_small <- alspac_r_small %>% 
  rename_at(vars(starts_with("life_score")),
            ~str_replace(., "since_.+_age", "since_age")) 

# rename childcare_frequency_child_sees_grandparents_age_5y5m to shift to next 
# year
alspac_r_small <- rename(alspac_r_small,
                         childcare_frequency_child_sees_grandparents_age_5y7m =
                           childcare_frequency_child_sees_grandparents_age_5y5m)



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



# new vars (09.12)

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
qplot(alspac_long$parent_networks_partner_social_support_score)



# peer problem --------------

count(alspac_long, peer_problem_score)
qplot(alspac_long$peer_problem_score) # skewed


# social cohesion -------------
count(alspac_long, social_cohesion)
qplot(alspac_long$social_cohesion)

# lots of negative values
count(alspac_long, social_discord)





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



count(alspac_long, neighbour_stress_score) %>% print(n = 100)
qplot(alspac_long$neighbour_stress_score)




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


# new vars (09.12)

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

# life ----

count(alspac_long, life_score_since)
qplot(alspac_long$life_score_since)






# childcare ------------------------------


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


# relationship -----------------

# relationship_to_parents_how_close_to_parents
# relationship_to_parents_mum_really_loves_toddler
# relationship_to_parents_partner_really_loves_child
# relationship_to_parents_partner_positive_relationship_score
# relationship_to_parents_partner_negative_relationship_score
# relationship_to_parents_mum_really_loves_ch
# relationship_to_parents_partner_really_loves_ch
# relationship_to_parents_partner_interaction_score
# relationship_to_parents_mother_interaction_score
# relationship_to_parents_mother_really_loves_child
# relationship_to_parents_mother_close_to_child
# relationship_to_parents_partner_close_to_child
# relationship_to_parents_partner_very_close_to_study_child
# relationship_to_parents_partner_very_close_child
# relationship_to_parents_mothers_partner_close_to_study_child
# relationship_to_parents_mother_loves_study_child
# relationship_to_parents_partner_loves_study_child


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



# social -------

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

# 
# count(alspac_long, social_conflict_child_afraid_of_other_relative)
# count(alspac_long, social_conflict_child_afraid_of_father)
# count(alspac_long, social_conflict_child_afraid_of_mother)
# count(alspac_long, social_conflict_child_afraid_of_other_children)
# 


# siblings ------

# sibling_interaction_child_argues_with_siblings
# sibling_interaction_sibling_interaction_score
# sibling_interaction_child_argues_with_older_child


alspac_long <- alspac_long %>% 
  mutate(sibling_interaction_child_argues_with_siblings_fct = 
           factorise(sibling_interaction_child_argues_with_siblings),
         sibling_interaction_child_argues_with_older_child_fct = 
           factorise(sibling_interaction_child_argues_with_older_child))

count(alspac_long, sibling_interaction_child_argues_with_siblings,
      sibling_interaction_child_argues_with_siblings_fct)
count(alspac_long, sibling_interaction_child_argues_with_older_child,
      sibling_interaction_child_argues_with_older_child_fct)

count(alspac_long, sibling_interaction_sibling_interaction_score)
qplot(alspac_long$sibling_interaction_sibling_interaction_score) +
  theme_bw()



# Control variables clean ----------------------------------

control_data <- read_rds("./data/clean/alspac_control_variables.rds")
glimpse(contol_data)
 

codebook <- map_df(control_data, function(x) attributes(x)$label) %>% 
  gather(key = name, value = label)
   
View(codebook)




# sex - kz021

control_data <- control_data %>% 
  mutate(female = case_when(kz021 == 1 ~ 0,
                            kz021 == 2 ~ 1),
         sex_fct = factor(female, labels = c("Male", "Female")))

count(control_data, kz021, female, sex_fct)

# ethnicity - c804, YPH2010, YPH2010, YPH2012

control_data <- control_data %>% 
  mutate(white = case_when(c804 == 1 ~ 1,
                           c804 == 2 ~ 0),
         white = case_when(is.na(white) & YPH2012 == 1 ~ 1,
                           is.na(white) & YPH2012 == 2 ~ 0,
                           TRUE ~ white))

count(control_data, white, c804, YPH2012)


# education needs - sa030, se030

control_data <- control_data %>% 
  mutate(special_edu_needs = case_when(sa030 == 1 ~ 1,
                                       sa030 == 2 ~ 0),
         special_edu_needs = 
           case_when(is.na(special_edu_needs) & se030 == 1 ~ 1,
                     is.na(special_edu_needs) & se030 == 2 ~ 0,
                     TRUE ~ special_edu_needs))

count(control_data, special_edu_needs, sa030)
count(control_data, special_edu_needs, se030)

count(control_data, sa030, se030) %>% print(n = 30)


# mom edu - c645

count(control_data, c645)

control_data <- control_data %>% 
  mutate(mom_edu_fct = case_when(c645 %in% 1:3 ~ "Other",
                                 c645 == 4 ~ "A level",
                                 c645 == 5 ~ "Degree"),
         mom_edu_fct = as.factor(mom_edu_fct),
         mom_degree = ifelse(mom_edu_fct == "Degree", 1, 0))

count(control_data, c645, mom_edu_fct, mom_degree)

# mom-marital status - j370, n8040
control_data <- control_data %>% 
  mutate(mom_partner = case_when(j370 %in% 1:4 ~ 0,
                                 j370 %in% 5:6 ~ 1),
         mom_work = case_when(c710 == 1 ~ 1,
                              c710 == 2 ~ 0))


count(control_data, j370, mom_partner)


# mom-employment - c710
count(control_data, c710, mom_work)

control_data_small <- select(control_data, 
                             uniqid, white, special_edu_needs, mom_edu_fct, 
                             mom_degree, mom_partner, mom_work)

# merge control and long data ----------------


alspac_long <- left_join(alspac_long, control_data_small, by = "uniqid")

# Export data -----------------------------------

write_rds(alspac_long, "./data/clean/alspac_long_v01.rds")




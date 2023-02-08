
# Admin -------------------------------------------------

# load packages
library(tidyverse)
library(lavaan)
library(blavaan)
library(lme4)
library(rstanarm)
library(broom.mixed)

# set up multiple processors
options(mc.cores = parallel::detectCores())

# load data
alspac_long_full <- read_rds("./data/clean/alspac_long_v02.rds")

# load functions
list.files("./scripts/functions/", full.names = T) %>% 
  map(source)


# select cases that participated at age 7
alspac_long <- alspac_long_full %>% 
  filter(baseline_age_7y9m == 1) 

# make time variable
alspac_long <- mutate(alspac_long,
                      age0 = age - 8,
                      age_e = ifelse(age %in% 9:15, 1, 0),
                      age_l = ifelse(age >15 , 1, 0))


# make wide version of the data
alspac_dep_w <- alspac_long %>% 
  select(uniqid, age, depression, anxiety, depression_smfq) %>% 
  pivot_wider(values_from = !c(uniqid),
              names_sep = "_",
              names_from = "age") %>% 
  select(-starts_with("age"))

# keep variables with valid answers
sel_data <- map_dbl(alspac_dep_w, mean, na.rm = T) %>% 
  is.nan()
alspac_dep_w <- alspac_dep_w[, !sel_data]


# rename to make coding easier
alspac_dep_w <- alspac_dep_w %>% 
  rename_all(~str_replace(., "depression_smfq", "smfq")) %>% 
  rename_all(~str_replace(., "depression", "d")) %>% 
  rename_all(~str_replace(., "anxiety", "a")) %>% 
  mutate(id = row_number())


## Data Exploration ------

### Cross-sectional GLM -----------------------------  

# list variables
vars_control <- c("female", "white", "mom_edu_fct", 
                  "imd2000", "healthy_very_8", "mom_depression", "siblings")
vars_int <- c("bullied_8", "bullied_10", 
              "school_likes_teacher_always", 
              "school_get_on_with_classmates_always",
              "school_feel_lonely_never",
              "friends_score_avg", "peer_problem_score_avg",
              "friends_plays_with_kids_rev_stab",
              "romantic_13", 
              "family_maternal_bond_1", 
              "family_mother_satisfaction_with_partner_avg",
              "parent_networks_mother_social_support_score_avg",
              "childcare_grandparent_looks_after_ch_0",
              "religion_support_mother_avg", 
              "social_cohesion_avg",
              "social_discord_avg",
              "change_school_9", "move_home_before8")

# loop over outcomes and starting and end age
vars_outcome <- c("depression_smfq", "anxiety", "depression")

args <- tibble(outcome = rep(vars_outcome, each = 2),
               age_select = c(10, 23, 8, 24, 8, 24))

# cross-sectional models for the three outcomes for start and end ages
glm_results <- pmap(args, function(outcome, age_select) {
  
  glm_formula <- str_c(outcome,
                       " ~ ",
                       str_c(vars_control, collapse = " + "),
                       " + ",
                       str_c(vars_int, collapse = " + "))
  
  
  glm(glm_formula, 
      data = filter(alspac_long, age == age_select),
      family = binomial)
})

# check and save results
glm_results %>% map_df(glance) %>% cbind(args)

save(glm_results, file = "./output/models/glm_results.RData")

# look only at significant predictors in all models
glm_results %>% 
  map(tidy, exp = TRUE) %>% 
  map(function(x) filter(x, p.value < 0.1))


### Explore missingness -------------------------------

# run models with individual predictors to see missing patterns
vars_final <- c("female", "white", "mom_edu_fct", 
                  "imd2000", "healthy_very_8", "mom_depression", "siblings",
                  "bullied_8", "bullied_10", 
                  "school_likes_teacher_always", 
                  "school_get_on_with_classmates_always",
                  "school_feel_lonely_never",
                  "friends_score_avg", 
                  "peer_problem_score_avg",
                  "romantic_13", 
                  "romantic_17", 
                  "religion_support_mother_avg", 
                  "social_cohesion_avg", "social_discord_avg",
                  "parent_networks_mother_social_support_score_avg",
                  "childcare_grandparent_looks_after_ch_0",
                  "change_school_9" )


reg_sing <- map(c(vars_final), function(x){
  glm_formula <- str_c("depression_smfq",
                       " ~ ",
                       x)
  
  glm(glm_formula, 
      data = alspac_long,
      family = binomial)
})


# look at most problematic variables in terms of missing
map_df(reg_sing, glance) %>% 
  mutate(var = c(vars_final)) %>% 
  select(var, nobs, everything()) %>% 
  arrange(nobs) %>% View()







# Theoretical model selected -------------------------------------------

# model selected based on theoretical consideration to include key variables
# at different ages 
# (some variables excluded due to missing data, see m_smfq_9 for reference)

#	Sex
#	Mother education (degree vs no degree; ref: no degree) [mother_education_age_gest]
#	Health status at age 8
#	Mother depression
#	Adverse life events (age 1, 2 and 3)  [adverse_life_events_score_to_age_3]
#	Adverse life events (age 4, 5, 8) [adverse_life_events_score_age_4_to_8]
#	SDQ (age 7, removing peer problems score) [SDQ_total_difficulties_age_6y9m]
#	Note: SDQ score not present at age 7, closest age was 6 years 9 months. 

## Variables at age 0-3
#	Mother social support (age 0) – not priority, can be excluded if needed
#	Maternal bond (age 0) [family_maternal_bond_1]
#	Maternal bond (age 3) [family_maternal_bond_3]
#	Social cohesion (average of age 0 and 3) [social_cohesion_avg_0_3]
#	Social discord (average age 0 and 3) [social_discord_avg_0_3]

## Variables at age 4-7
#	Mother social support (age 6) – not priority, can be excluded if needed
#	Social cohesion (average age 5 and 7) [social_cohesion_avg_5_7]
#	Social discord (average age 5 and 7) [social_discord_avg_5_7]

##  Variables at age 8-11
#	Mother social support (age 9) – not priority, can be excluded if needed
#	Bullied (age 8)
#	Bullied (age 10)
#	Friends score (average age 8 and 10) [friends_score_avg_8_10]
#	Peer problems (average age 7 and 10 [peer_problem_avg_7_10]
#	School feels lonely never (age 11)
#	Social cohesion (age 10)
#	Social discord (age 10)

## Variables at age 12-15
#	Mother social support (age 12) – not priority, can be excluded if needed
#	Friends score (average age 12 and 13) [friends_score_avg_12_13]
#	Peer problems (average age 12 and 13) [peer_problem_avg_12_13]
#	School feels lonely never (age 14)
#	Romantic (age 13) – not priority, can be excluded if needed

# run multilevel model with random intercept and slope at the individual 
# level and with square time effects
m_smfq_10 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                     mother_education_age_gest +
                     healthy_very_8 +
                     mom_depression +
                     adverse_life_events_score_to_age_3 +
                     adverse_life_events_score_age_4_to_8 +
                     SDQ_total_difficulties_age_6y9m +
                     family_maternal_bond_1 +
                     family_maternal_bond_3 +
                     social_cohesion_avg_0_3 +
                     social_discord_avg_0_3 +
                     social_cohesion_avg_5_7 +
                     social_discord_avg_5_7 +
                     bullied_8 + bullied_10 + 
                     friends_score_avg_8_10 +
                     peer_problem_avg_7_10 +
                     school_feel_lonely_never_11+
                     social_cohesion_avg_10 +
                     social_discord_avg_10 +
                     friends_score_avg_12_13 +
                     peer_problem_avg_12_13 + 
                     school_feel_lonely_never_14 +
                     age0:bullied_10  +
                     (1 + age0 | uniqid),
                   data = alspac_long, family = binomial)

summary(m_smfq_10)
tidy(m_smfq_10, exp = TRUE)
glance(m_smfq_10)

save(m_smfq_10, file = "./output/models/m_smfq_10.RData")



# due to estimation issues we re-run the model using Bayesian estimation
m_smfq_10_stan <- stan_glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                               mother_education_age_gest +
                               healthy_very_8 +
                               mom_depression +
                               adverse_life_events_score_to_age_3 +
                               adverse_life_events_score_age_4_to_8 +
                               SDQ_total_difficulties_age_6y9m +
                               family_maternal_bond_1 +
                               family_maternal_bond_3 +
                               social_cohesion_avg_0_3 +
                               social_discord_avg_0_3 +
                               social_cohesion_avg_5_7 +
                               social_discord_avg_5_7 +
                               bullied_8 + bullied_10 + 
                               friends_score_avg_8_10 +
                               peer_problem_avg_7_10 +
                               school_feel_lonely_never_11+
                               social_cohesion_avg_10 +
                               social_discord_avg_10 +
                               friends_score_avg_12_13 +
                               peer_problem_avg_12_13 + 
                               school_feel_lonely_never_14 +
                               age0:bullied_10  +
                               (1 + age0 | uniqid),
                             data = alspac_long, family = binomial(),
                             init_r = 0,
                             iter = 6000)

summary(m_smfq_10_stan)
tidy(m_smfq_10_stan, exp = TRUE)
glance(m_smfq_10_stan)

save(m_smfq_10_stan, file = "./output/models/m_smfq_10_stan.RData")


# Exploratory model building -----------------------------------

# here we run a series of models to explore how best to model the outcomes
# this is a more data driven model development

## SMFQ ------------------

### Unconditional models ---------------

# empty random effects model
gm0 <- glmer(depression_smfq ~ 1 + (1 | uniqid),
             data = alspac_long, family = binomial)


summary(gm0)

save(gm0, file = "./output/models/m_smfq_gm0.RData")


# unconditional change model
gm1 <- glmer(depression_smfq ~ 1 + age0 + (1 + age0 | uniqid),
             data = alspac_long, family = binomial)


summary(gm1)

tidy(gm1)

save(gm1, file = "./output/models/m_smfq_gm1.RData")


# non-linear change model - convergence issues
gm2 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
               (1 + age0 | uniqid),
             data = alspac_long, family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5)))

summary(gm2)
tidy(gm2)

save(gm2, file = "./output/models/m_smfq_gm2.RData")


# non-linear change model using Bayesian estimation
gm2_stan <- stan_glmer(depression_smfq ~ 1 + age0 + I(age0^2) +
                         (1 + age0 | uniqid),
             data = alspac_long, family = binomial())


summary(gm2_stan)
tidy(gm2_stan)
save(gm2_stan, file = "./output/models/m_smfq_gm2_stan.RData")



# different ways to model non-linear change in time but all lead to 
# convergence issues

# warning
gm3 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + I(age0^3) +
               (1 + age0 | uniqid),
             data = alspac_long, family = binomial)

summary(gm3)

# warning
gm4 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + I(age0^3) + I(age0^4) +
               (1 + age0 | uniqid),
             data = alspac_long, family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5)))

summary(gm4)


# warning
gm5 <- glmer(depression_smfq ~ as.factor(age0) + (1 + age0 | uniqid),
             data = alspac_long, family = binomial)

summary(gm5)



# Conditional models ---------------------------------------------------

# here we build on the reference unconditional model 
# (random intercept and slope model) with square effects for time
# we add blocks of predictors to explore relationships and impact of missing 
# data

# reference unconditional model
m_smfq_1 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_1)
tidy(m_smfq_1)

save(m_smfq_1, file = "./output/models/m_smfq_1.RData")

# first block of predictors: socio-demographic controls
m_smfq_2 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    imd2000 + healthy_very_8 + mom_depression + siblings +
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_2)
tidy(m_smfq_2)

save(m_smfq_2, file = "./output/models/m_smfq_2.RData")


# first key variables of interest block
m_smfq_3 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    imd2000 + healthy_very_8 + mom_depression + siblings +
                    bullied_8 + bullied_10 + 
                    age0:bullied_8 + age0:bullied_10 +
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_3)
tidy(m_smfq_3, exp = TRUE)
glance(m_smfq_3)

save(m_smfq_3, file = "./output/models/m_smfq_3.RData")



# second block of key variables

# use:
# school_likes_teacher_rev_avg (ind stable) or school_likes_teacher_always (dummy)
# school_get_on_with_classmates_avg (ind stable) or school_get_on_with_classmates_always
# school_feel_lonely_avg (ind stable) or school_feel_lonely_never
m_smfq_4 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    imd2000 + healthy_very_8 + mom_depression + siblings +
                    school_likes_teacher_always + 
                    school_get_on_with_classmates_always +
                    school_feel_lonely_never +
                    age0:school_likes_teacher_always + 
                    age0:school_get_on_with_classmates_always + 
                    age0:school_feel_lonely_never +
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_4)
tidy(m_smfq_4, exp = TRUE)
glance(m_smfq_4)

save(m_smfq_4, file = "./output/models/m_smfq_4.RData")






# third block of key variables

# friends_score_avg
# peer_problem_score_avg
# friends_plays_with_kids_rev_stab
# romantic_13, romantic_16, romantic_17, romantic_24


m_smfq_5 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    imd2000 + healthy_very_8 + mom_depression + siblings +
                    friends_score_avg + 
                    peer_problem_score_avg +
                    friends_plays_with_kids_rev_stab +
                    romantic_13 + romantic_16 + romantic_17 + romantic_24 +
                    age0:friends_score_avg + 
                    age0:peer_problem_score_avg + 
                    age0:friends_plays_with_kids_rev_stab +
                    age0:romantic_13 +
                    age0:romantic_16 +
                    age0:romantic_17 +
                    age0:romantic_24 +
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_5)
tidy(m_smfq_5, exp = TRUE)
glance(m_smfq_5)

save(m_smfq_5, file = "./output/models/m_smfq_5.RData")





# forth block of key variable

# family_maternal_bond_1
# family_mother_satisfaction_with_partner_avg
# parent_networks_mother_social_support_score_avg
# childcare_grandparent_looks_after_ch_0


m_smfq_6 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    imd2000 + healthy_very_8 + mom_depression + siblings +
                    family_maternal_bond_1 +
                    family_mother_satisfaction_with_partner_avg +
                    parent_networks_mother_social_support_score_avg +
                    childcare_grandparent_looks_after_ch_0 +
                    age0:family_maternal_bond_1 + 
                    age0:family_mother_satisfaction_with_partner_avg +
                    age0:parent_networks_mother_social_support_score_avg +
                    age0:childcare_grandparent_looks_after_ch_0 +
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_6)
tidy(m_smfq_6, exp = TRUE)
glance(m_smfq_6)

save(m_smfq_6, file = "./output/models/m_smfq_6.RData")



# fifth block of key variables

# CH has SIB 
# religion_support_mother 
# social_discord 
# social_cohesion 


m_smfq_7 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    imd2000 + healthy_very_8 + mom_depression + siblings +
                    religion_support_mother_avg + 
                    social_cohesion_avg +
                    social_discord_avg +
                    change_school_9 + change_school_17 + 
                    move_home_before8 +
                    age0:religion_support_mother_avg + 
                    age0:social_cohesion_avg + 
                    age0:social_discord_avg +
                    age0:change_school_9 +
                    age0:change_school_17 +
                    age0:move_home_before8 +
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_7)
tidy(m_smfq_7, exp = TRUE)
glance(m_smfq_7)

save(m_smfq_7, file = "./output/models/m_smfq_7.RData")





# best model so far based on previous results. This tries to minimize missing 
# data and keep key variables of interest that are significant

m_smfq_8 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    healthy_very_8 + mom_depression + siblings +
                    bullied_8 + bullied_10 + 
                    school_likes_teacher_always + 
                    school_get_on_with_classmates_always +
                    school_feel_lonely_never +
                    friends_score_avg + 
                    peer_problem_score_avg +
                    romantic_13 + 
                    romantic_17 + 
                    social_cohesion_avg + 
                    parent_networks_mother_social_support_score_avg +
                    childcare_grandparent_looks_after_ch_0 +
                    age0:school_feel_lonely_never +
                    age0:childcare_grandparent_looks_after_ch_0 +
                    age0:peer_problem_score_avg +
                    age0:romantic_13 +
                    age0:romantic_17 + 
                    age0:bullied_10 +
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_8)
tidy(m_smfq_8, exp = TRUE)
glance(m_smfq_8)

save(m_smfq_8, file = "./output/models/m_smfq_8.RData")
                    






# best model so far with bayes estimation some changes from above due to 
# estimation issues. I eliminated: 
# romantic_16, family_paternal_bond_2, religion_support_mother_avg 

m_smfq_8_stan <- 
  stan_glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
               female + white + mom_edu_fct + 
               healthy_very_8 + mom_depression + siblings +
               bullied_8 + bullied_10 + 
               school_likes_teacher_always + 
               school_get_on_with_classmates_always +
               school_feel_lonely_never +
               friends_score_avg + 
               peer_problem_score_avg +
               romantic_13 + 
               romantic_17 + 
               social_cohesion_avg + 
               parent_networks_mother_social_support_score_avg +
               childcare_grandparent_looks_after_ch_0 +
               age0:school_feel_lonely_never +
               age0:childcare_grandparent_looks_after_ch_0 +
               age0:peer_problem_score_avg +
               age0:romantic_13 +
               age0:romantic_17 + 
               age0:bullied_10 +
               (1 + age0 | uniqid),
               data = alspac_long, family = binomial(),
               init_r = 0,
             iter = 6000)

summary(m_smfq_8_stan)
tidy(m_smfq_8_stan, exp = TRUE)
glance(m_smfq_8_stan)

save(m_smfq_8_stan, file = "./output/models/m_smfq_8_stan.RData")



# run full model with all predictors for refrence
# considerable loss of sample

m_smfq_full <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    imd2000 + healthy_very_8 + mom_depression + siblings +
                      bullied_8 + bullied_10 + 
                      school_likes_teacher_always + 
                      school_get_on_with_classmates_always +
                      school_feel_lonely_never +
                      friends_score_avg + 
                      peer_problem_score_avg +
                      friends_plays_with_kids_rev_stab +
                      romantic_13 + romantic_16 + romantic_17 + romantic_24 +
                      family_maternal_bond_1 +
                      family_mother_satisfaction_with_partner_avg +
                      parent_networks_mother_social_support_score_avg +
                      childcare_grandparent_looks_after_ch_0 +
                      religion_support_mother_avg + 
                      change_school_9 + change_school_17 + 
                      move_home_before8 + 
                      social_cohesion_avg +
                      social_discord_avg +
                      age0:bullied_8 + age0:bullied_10 +
                      age0:school_likes_teacher_always + 
                      age0:school_get_on_with_classmates_always + 
                      age0:school_feel_lonely_never +
                      age0:friends_score_avg + 
                      age0:peer_problem_score_avg + 
                      age0:friends_plays_with_kids_rev_stab +
                      age0:romantic_13 +
                      age0:romantic_16 +
                      age0:family_maternal_bond_1 + 
                      age0:family_mother_satisfaction_with_partner_avg +
                      age0:parent_networks_mother_social_support_score_avg +
                      age0:childcare_grandparent_looks_after_ch_0 +
                      age0:religion_support_mother_avg + 
                     (1 + age0 | uniqid) ,
                  data = alspac_long, family = binomial)

summary(m_smfq_full)
tidy(m_smfq_full, exp = TRUE)
glance(m_smfq_full)

save(m_smfq_full, file = "./output/models/m_smfq_full.RData")




# initial model with piecemeal predictors at different ages. Used to build
# model 10 presented at the top of the script

m_smfq_9 <- glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    healthy_very_8 + mom_depression +
                    imd2000 +
                    bullied_8 + bullied_10 + 
                    school_get_on_with_classmates_always +
                    school_feel_lonely_never +
                    romantic_13 + 
                    romantic_17 + 
                    friends_score_avg_8_10 + friends_score_avg_12_13 + 
                    peer_problem_avg_7_10 + peer_problem_avg_12_13 + 
                    parent_networks_mother_social_support_score_avg_0_6 + 
                    parent_networks_mother_social_support_score_avg_9 +
                    parent_networks_mother_social_support_score_avg_12 + 
                    social_cohesion_avg_0_3 + social_cohesion_avg_5_7 + 
                    social_cohesion_avg_10 + social_discord_avg_0_3 + 
                    social_discord_avg_5_7 + social_discord_avg_10 +
                    age0:school_feel_lonely_never +
                    age0:romantic_13 +
                    age0:romantic_17 + 
                    age0:bullied_10 +
                    (1 + age0 | uniqid),
                  data = alspac_long, family = binomial)

summary(m_smfq_9)
tidy(m_smfq_9, exp = TRUE)
glance(m_smfq_9)

save(m_smfq_9, file = "./output/models/m_smfq_9.RData")

# Bayesian estimation to deal with convergence
m_smfq_9_stan <- stan_glmer(depression_smfq ~ 1 + age0 + I(age0^2) + 
                    female + white + mom_edu_fct + 
                    healthy_very_8 + mom_depression +
                    imd2000 +
                    bullied_8 + bullied_10 + 
                    school_get_on_with_classmates_always +
                    school_feel_lonely_never +
                    romantic_13 + 
                    romantic_17 + 
                    friends_score_avg_8_10 + friends_score_avg_12_13 + 
                    peer_problem_avg_7_10 + peer_problem_avg_12_13 + 
                    parent_networks_mother_social_support_score_avg_0_6 + 
                    parent_networks_mother_social_support_score_avg_9 +
                    parent_networks_mother_social_support_score_avg_12 + 
                    social_cohesion_avg_0_3 + social_cohesion_avg_5_7 + 
                    social_cohesion_avg_10 + social_discord_avg_0_3 + 
                    social_discord_avg_5_7 + social_discord_avg_10 +
                    age0:school_feel_lonely_never +
                    age0:romantic_13 +
                    age0:romantic_17 + 
                    age0:bullied_10 +
                    (1 + age0 | uniqid),
                    data = alspac_long, family = binomial(),
                    init_r = 0,
                    iter = 6000)

summary(m_smfq_9_stan)
tidy(m_smfq_9_stan, exp = TRUE)
glance(m_smfq_9_stan)

save(m_smfq_9_stan, file = "./output/models/m_smfq_9_stan.RData")


load("./output/models/m_smfq_9_stan.RData")




# explore missingness for piecemeal model

vars_final <- c("female", "white", "mom_edu_fct", 
                "imd2000", "healthy_very_8", "mom_depression", 
                "bullied_8", "bullied_10", 
                "school_likes_teacher_always", 
                "school_get_on_with_classmates_always",
                "school_feel_lonely_never",
                "friends_score_avg_8_10", "friends_score_avg_12_13",
                "peer_problem_avg_7_10", "peer_problem_avg_12_13",
                "romantic_13", 
                "romantic_17", 
                "parent_networks_mother_social_support_score_avg_0_6", 
                  "parent_networks_mother_social_support_score_avg_9",
                  "parent_networks_mother_social_support_score_avg_12", 
                  "social_cohesion_avg_0_3", "social_cohesion_avg_5_7", 
                  "social_cohesion_avg_10", "social_discord_avg_0_3",
                  "social_discord_avg_5_7", "social_discord_avg_10"
                )


reg_sing <- map(c(vars_final), function(x){
  glm_formula <- str_c("depression_smfq",
                       " ~ ",
                       x)
  
  glm(glm_formula, 
      data = alspac_long,
      family = binomial)
})

map_df(reg_sing, glance) %>% 
  mutate(var = c(vars_final)) %>% 
  select(var, nobs, everything()) %>% 
  arrange(nobs) %>% View()




# explore coefficients from piecemeal model

summary(m_smfq_9_stan)
tidy(m_smfq_9_stan, exp = TRUE)
glance(m_smfq_9_stan)

m9_coef <- broom::tidy(m_smfq_9_stan, conf.int = TRUE)  

# save coefficients
m9_coef2 <- m9_coef %>% 
  mutate(odds = exp(estimate),
         odds_lci = exp(conf.low),
         odds_uci = exp(conf.high)) %>%
  rename(log_odds = estimate) %>% 
  select(term, odds, odds_lci, odds_uci, log_odds, everything()) 

write_csv(m9_coef2, "./output/m9_coef2.csv")

# make graphs with results
m9_coef2 %>% 
  select(term, odds, odds_lci, odds_uci) %>% 
  knitr::kable(digits = 2)

fig <- m9_coef2 %>% 
  mutate(term = factor(term, levels = m9_coef2$term) %>% fct_rev()) %>% 
  ggplot(aes(odds, term, xmin = odds_lci, xmax = odds_uci)) +
  geom_pointrange() +
  theme_bw() +
  geom_vline(xintercept = 1) +
  labs(x = "Odds", y = "Predictors")

ggsave(plot = fig, "./output/fig/m9_stan.png", dpi = 500)

fig







## Anxiety and depression --------------------------------

### final models based on data exploration above ---------------

m_anxiety_8 <- glmer(anxiety ~ 1 + age0 + I(age0^2) + 
                     female + white + mom_edu_fct + 
                     healthy_very_8 + mom_depression + siblings +
                     bullied_8 + bullied_10 + 
                     school_likes_teacher_always + 
                     school_get_on_with_classmates_always +
                     school_feel_lonely_never +
                     friends_score_avg + 
                     peer_problem_score_avg +
                     romantic_13 + 
                     romantic_17 + 
                     social_cohesion_avg + 
                     parent_networks_mother_social_support_score_avg +
                     childcare_grandparent_looks_after_ch_0 +
                     age0:bullied_8 +
                     age0:bullied_10 +
                     age0:peer_problem_score_avg +
                     age0:social_cohesion_avg +
                     age0:childcare_grandparent_looks_after_ch_0 +
                       (1 + age0 | uniqid),
                   data = alspac_long, family = binomial)

summary(m_anxiety_8)
tidy(m_anxiety_8, exp = TRUE)
glance(m_anxiety_8)

save(m_anxiety_8, file = "./output/models/m_anxiety_8.RData")



m_depress_8 <- glmer(depression ~ 1 + age0 + I(age0^2) + 
                     female + white + mom_edu_fct + 
                     healthy_very_8 + mom_depression + siblings +
                     bullied_8 + bullied_10 + 
                     school_likes_teacher_always + 
                     school_get_on_with_classmates_always +
                     school_feel_lonely_never +
                     friends_score_avg + 
                     peer_problem_score_avg +
                     romantic_13 + 
                     romantic_17 + 
                     social_cohesion_avg + 
                     parent_networks_mother_social_support_score_avg +
                     childcare_grandparent_looks_after_ch_0 +
                     age0:bullied_8 +
                     age0:bullied_10 +
                     age0:school_feel_lonely_never +
                     age0:friends_score_avg +
                     age0:peer_problem_score_avg + 
                       (1 + age0 | uniqid),
                   data = alspac_long, family = binomial)

summary(m_depress_8)
tidy(m_depress_8, exp = TRUE)
glance(m_depress_8)

save(m_depress_8, file = "./output/models/m_depress_8.RData")




rm(m_depress_8)
gc()



## final models anxiety and depression bayesian -------------------------

m_anxiety_8_stan <- stan_glmer(anxiety ~ 1 + age0 + I(age0^2) + 
                       female + white + mom_edu_fct + 
                       healthy_very_8 + mom_depression + siblings +
                       bullied_8 + bullied_10 + 
                       school_likes_teacher_always + 
                       school_get_on_with_classmates_always +
                       school_feel_lonely_never +
                       friends_score_avg + 
                       peer_problem_score_avg +
                       romantic_13 + 
                       romantic_17 + 
                       social_cohesion_avg + 
                       parent_networks_mother_social_support_score_avg +
                       childcare_grandparent_looks_after_ch_0 +
                       age0:bullied_8 +
                       age0:bullied_10 +
                       age0:peer_problem_score_avg +
                       age0:social_cohesion_avg +
                       age0:childcare_grandparent_looks_after_ch_0 +
                       (1 + age0 | uniqid),
                       data = alspac_long, family = binomial(),
                       init_r = 0,
                       iter = 6000)

summary(m_anxiety_8_stan)
tidy(m_anxiety_8_stan, exp = TRUE)
glance(m_anxiety_8_stan)

save(m_anxiety_8_stan, file = "./output/models/m_anxiety_8_stan.RData")

rm(m_anxiety_8_stan)
gc()

m_depress_8_stan <- stan_glmer(depression ~ 1 + age0 + I(age0^2) + 
                       female + white + mom_edu_fct + 
                       healthy_very_8 + mom_depression + siblings +
                       bullied_8 + bullied_10 + 
                       school_likes_teacher_always + 
                       school_get_on_with_classmates_always +
                       school_feel_lonely_never +
                       friends_score_avg + 
                       peer_problem_score_avg +
                       romantic_13 + 
                       romantic_17 + 
                       social_cohesion_avg + 
                       parent_networks_mother_social_support_score_avg +
                       childcare_grandparent_looks_after_ch_0 +
                       age0:bullied_8 +
                       age0:bullied_10 +
                       age0:school_feel_lonely_never +
                       age0:friends_score_avg +
                       age0:peer_problem_score_avg + 
                       (1 + age0 | uniqid),
                       data = alspac_long, family = binomial(),
                       init_r = 0,
                       iter = 6000)

summary(m_depress_8_stan)
tidy(m_depress_8_stan, exp = TRUE)
glance(m_depress_8_stan)

save(m_depress_8_stan, file = "./output/models/m_depress_8_stan.RData")

rm(m_depress_8_stan)
gc()







## grouped models anxiety and depression bayesian -------------------------

m_anxiety_9_stan <- stan_glmer(anxiety ~ 1 + age0 + I(age0^2) + 
                                 female + white + mom_edu_fct + 
                                 healthy_very_8 + mom_depression +
                                 imd2000 +
                                 bullied_8 + bullied_10 + 
                                 school_get_on_with_classmates_always +
                                 school_feel_lonely_never +
                                 romantic_13 + 
                                 romantic_17 + 
                                 friends_score_avg_8_10 + friends_score_avg_12_13 + 
                                 peer_problem_avg_7_10 + peer_problem_avg_12_13 + 
                                 parent_networks_mother_social_support_score_avg_0_6 + 
                                 parent_networks_mother_social_support_score_avg_9 +
                                 parent_networks_mother_social_support_score_avg_12 + 
                                 social_cohesion_avg_0_3 + social_cohesion_avg_5_7 + 
                                 social_cohesion_avg_10 + social_discord_avg_0_3 + 
                                 social_discord_avg_5_7 + social_discord_avg_10 +
                                 age0:school_feel_lonely_never +
                                 age0:romantic_13 +
                                 age0:romantic_17 + 
                                 age0:bullied_10 +
                                 (1 + age0 | uniqid),
                               data = alspac_long, family = binomial(),
                               init_r = 0,
                               iter = 6000)

summary(m_anxiety_9_stan)
tidy(m_anxiety_9_stan, exp = TRUE)
glance(m_anxiety_9_stan)

save(m_anxiety_9_stan, file = "./output/models/m_anxiety_9_stan.RData")

rm(m_anxiety_9_stan)
gc()

m_depress_9_stan <- stan_glmer(depression ~ 1 + age0 + I(age0^2) + 
                                 female + white + mom_edu_fct + 
                                 healthy_very_8 + mom_depression +
                                 imd2000 +
                                 bullied_8 + bullied_10 + 
                                 school_get_on_with_classmates_always +
                                 school_feel_lonely_never +
                                 romantic_13 + 
                                 romantic_17 + 
                                 friends_score_avg_8_10 + friends_score_avg_12_13 + 
                                 peer_problem_avg_7_10 + peer_problem_avg_12_13 + 
                                 parent_networks_mother_social_support_score_avg_0_6 + 
                                 parent_networks_mother_social_support_score_avg_9 +
                                 parent_networks_mother_social_support_score_avg_12 + 
                                 social_cohesion_avg_0_3 + social_cohesion_avg_5_7 + 
                                 social_cohesion_avg_10 + social_discord_avg_0_3 + 
                                 social_discord_avg_5_7 + social_discord_avg_10 +
                                 age0:school_feel_lonely_never +
                                 age0:romantic_13 +
                                 age0:romantic_17 + 
                                 age0:bullied_10 +
                                 (1 + age0 | uniqid),
                               data = alspac_long, family = binomial(),
                               init_r = 0,
                               iter = 6000)

summary(m_depress_9_stan)
tidy(m_depress_9_stan, exp = TRUE)
glance(m_depress_9_stan)

save(m_depress_9_stan, file = "./output/models/m_depress_9_stan.RData")

rm(m_depress_9_stan)
gc()





  



# Admin -------------------------------------------------

library(tidyverse)
library(lavaan)
library(blavaan)
library(lme4)
library(rstanarm)
library(broom.mixed)

# multi-processor set-up
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

# Make table and graphs with results from model 10 ----------

# load model 10
load("./output/models/m_smfq_10_stan.RData")


m10_coef <- broom::tidy(m_smfq_10_stan, conf.int = TRUE)  

summary(m_smfq_10_stan)
tidy(m_smfq_10_stan, exp = TRUE)

  filter()

  m_smfq_10_stan$coefficients %>% 
    names() %>% 
    str_subset("b\\[\\(Intercept.+") %>% 
    length()

# 3274 individuals and 20,542 rows
glance(m_smfq_10_stan)


m10_coef2 <- m10_coef %>% 
  mutate(odds = exp(estimate),
         odds_lci = exp(conf.low),
         odds_uci = exp(conf.high)) %>%
  rename(log_odds = estimate) %>% 
  select(term, odds, odds_lci, odds_uci) 

# write results
write_csv(m10_coef2, "./output/tabs/smfq_m10_stan.csv")

m10_coef2 %>% 
  mutate(sig1 = ifelse((odds_lci > 1 & odds_uci > 1) | 
                         (odds_lci < 1 & odds_uci < 1), 1,0),
         prob = (odds - 1) *100) %>% 
  filter(sig1 == 1) %>% 
  filter(!term %in% c("(Intercept)", "age0", "I(age0^2)")) %>% 
  knitr::kable(digits = 2) 

# Interpretation --------------
# girls have almost 3 times higher chances of depression compared to boys

# if mother has a degree decreases the chances by around 342 of depression
# if mother has depression it increases chances by around 50%
# if they had adverse life events up to age 3 it increases chances by ~21%
# SDQ total difficulties at age 7 increases chances by around 5%
# increase by 1 in average social discord score before 4 increases chances by 36%
# being bullied at age 8 increases chances of depression by 60% 
# being bullied at age 10 increases chances of depression by ~3.5 times though the effect decreases with time (by 6% with each extra year)
# never feeling lonely at school at age 11 decreases chances by ~44%
# an increase by 1 in friends average scores at ages 12-13 increase chance of depression by 12%
# an increase by 1 in peer problem score at ages 12-13 leads to a increase in probably of depression by ~14%

# the rest of the predictors include 1 in the credibility interval so there is no evidence they have an effect on the likelihood of depression
# 

# Graph results -------------------

m10_coef2 %>% 
  mutate(term = factor(term, levels = m10_coef2$term) %>% fct_rev()) %>% 
  ggplot(aes(odds, term, xmin = odds_lci, xmax = odds_uci)) +
  geom_pointrange() +
  theme_bw() +
  geom_vline(xintercept = 1) +
  labs(x = "Odds", y = "Predictors")

ggsave("./output/fig/smfq_m10_stan.png", dpi = 500)

fig

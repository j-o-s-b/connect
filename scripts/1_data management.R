## ---------------------------
## Script name: Wellcome data management
##
## Purpose of script: select variables for analysis and derive analysis variables.
##
## - Rename variables to add people's age and the concept measured. 
## - Derive new variables where needed. 
## - Variables were named based on the domain of social connection, e.g. family, and the concept measured, e.g. whether has enough friends, followed by the age the variable was collected. 
## - Because many variables were used, we split our initial sample into subsets by domain, and then merge our final selected variables together again at the end of the file. 
##
## Author: Joe Crowley
##
## ---------------------------

# Set working directory, load packages

library(tidyverse)


# Load data
alspac <- readRDS("alspac.rds")

# Create uniqid - by combining ID and pregnancy ID. 
alspac <- alspac %>% mutate(uniqid = paste0(cidB4083, qlet), collapse="")

################################################################################
# Neighbourhood domain

################################################################################
# Neighbourhood social cohesion - relevant questions:

#  Q1	Do the other people in your neighborhood visit your home?
#  Q3	Do the other people in your neighborhood look after your children? 
#  Q4	Do the other people in your neighborhood keep to themselves? [reverse]
#  Q5	Do you visit the homes of your neighbours? 
#  Q7	Do you look after your neighbors' children?
#  Q8	Do you keep to yourself? [reverse]

# Reverse numbering of those marked with [reverse], a high score on these is negative, while on the other questions a high score is positive, e.g. a 5 (always) on Question 1, whether people visit your home, would be a sign of strong social connectivity within your neighbourhood. 

# Timepoints:

# Collected from the mother: A, F, G, H, K, M, Q, T
# Partner data not used.

###############################################################################
# Explpore first neighbourhood vars in alspac data
# Age: gestation 8-42 weeks

attributes(alspac$a601)
alspac %>% count(a601)

attributes(alspac$a603)
alspac %>% count(a603)

attributes(alspac$a604)
alspac %>% count(a604)

attributes(alspac$a605)
alspac %>% count(a605)

attributes(alspac$a607)
alspac %>% count(a607)

attributes(alspac$a608)
alspac %>% count(a608)

alspac %>%
  summarise(
    q1 = mean(a601 < 0 | is.na(a601))*100,
    q3 = mean(a603 < 0 | is.na(a603))*100,
    q4 = mean(a604 < 0 | is.na(a604))*100,
    q5 = mean(a605 < 0 | is.na(a605))*100,
    q6 = mean(a607 < 0 | is.na(a607))*100,
    q8 = mean(a608 < 0 | is.na(a608))*100
  )

# Reorder items 4 and 8 - these are negative social experiences, so re-ordering to align with others. 
# Where 1 is negative (low social cohesion), and 5 is positive (high social cohesion). 

# Create separate neighbourhood dataframe with required variables
neighbour <- alspac %>% select(uniqid, a601, a603, a604, a605, a607, a608, 
                               f890, f892, f893, f895,f897, f898,
                               g850, g852, g853, g855, g857, g858,
                               h770, h772, h773, h774, h776, h777,
                               k7000, k7002, k7003, k7010, k7012, k7013, 
                               m2230, m2232, m2233, m2240, m2242, m2243, 
                               q2220, q2222, q2223, q2230, q2232, q2233,
                               t2025, t2027, t2028, t2030, t2032, t2033, 
                               a602, a606, f891, f896, g851, g856, h771, 
                               h775, k7001, k7011, m2231, m2241, q2221, q2231, 
                               t2026, t2031, 
                               g860, 	g488, 	g489, 	g490, 	g491, 	g492, 	g493, 	g494, 	g495,
                               h778, 	h357, 	h358, 	h359, 	h360, 	h361, 	h362, 	h363, 	h364,
                               k7020, 	k5213, 	k5214, 	k5215, 	k5216, 	k5217, 	k5218, 	k5219, 	k5220,
                               m2250, 	m2203, 	m2204, 	m2205, 	m2206, 	m2207, 	m2208, 	m2209, 	m2210,
                               q2240, 	q2203, 	q2204, 	q2205, 	q2206, 	q2207, 	q2208, 	q2209, 	q2210, 	q2203,
                               t2035, 	t2010, 	t2011, 	t2012, 	t2013, 	t2014, 	t2015, 	t2016, 	t2017, 	t2010)

# Set missing values
neighbour <- na_if(neighbour, -9999)
neighbour <- na_if(neighbour, -7)
neighbour <- na_if(neighbour, -1)
neighbour <- na_if(neighbour, 9)

# Reorder variables in earliest wave
neighbour <- neighbour %>%
  mutate(
    a604a = case_when(
      a604 == 1 ~ 5,
      a604 == 2 ~ 4,
      a604 == 3 ~ 3,
      a604 == 4 ~ 2,
      a604 == 5 ~ 1), 
    a608a = case_when(
      a608 == 1 ~ 5,
      a608 == 2 ~ 4,
      a608 == 3 ~ 3,
      a608 == 4 ~ 2,
      a608 == 5 ~ 1))

# Generate neighbourhood social cohesion score (sum score) 
neighbour <- neighbour %>% mutate_at(vars(contains("a6")), as.numeric) # Change all cases to numeric. 
neighbour <- neighbour %>% mutate(social_cohesion_age_0y0m = rowSums(across(c(a601,a603,a604a,a605,a607,a608a))))
sum(is.na(neighbour$social_cohesion_age_0y0m))
sum(is.na(neighbour$a601))
neighbour %>% select(social_cohesion_age_0y0m, a601,a603,a604a,a605,a607,a608a) %>% head()
neighbour %>% select(social_cohesion_age_0y0m, a601,a603,a604a,a605,a607,a608a) %>% tail()


###############################################################################
# Age:  8 months

attributes(alspac$f890)
alspac %>% count(f890)

attributes(alspac$f892)
alspac %>% count(f892)

attributes(alspac$f893)
alspac %>% count(f893)

attributes(alspac$f895)
alspac %>% count(f895)

attributes(alspac$f897)
alspac %>% count(f897)

attributes(alspac$f898)
alspac %>% count(f898)

alspac %>%
  summarise(
    q1 = mean(f890 < 0 | is.na(f890))*100,
    q3 = mean(f892 < 0 | is.na(f892))*100,
    q4 = mean(f893 < 0 | is.na(f893))*100,
    q5 = mean(f895 < 0 | is.na(f895))*100,
    q6 = mean(f897 < 0 | is.na(f897))*100,
    q8 = mean(f898 < 0 | is.na(f898))*100
  )

# Reorder items 4 (f893) and 8 (f898) - these are negative social experiences, so re-ordering to align with others. 
# Where 1 is negative (low social cohesion), and 5 is positive (high social cohesion). 

neighbour <- neighbour %>% mutate_at(vars(contains("f8")), as.numeric) # Change all cases to numeric. 

neighbour <- neighbour %>%
  mutate(
    f893a = case_when(
      f893 == 1 ~ 5,
      f893 == 2 ~ 4,
      f893 == 3 ~ 3,
      f893 == 4 ~ 2,
      f893 == 5 ~ 1), 
    f898a = case_when(
      f898 == 1 ~ 5,
      f898 == 2 ~ 4,
      f898 == 3 ~ 3,
      f898 == 4 ~ 2,
      f898 == 5 ~ 1))

neighbour <- neighbour %>% mutate(social_cohesion_age_0y8m = rowSums(across(c(f890, f892, f893a, f895, f897, f898a))))

###############################################################################
# Age:  21 months
attributes(alspac$g850)
alspac %>% count(g850)

attributes(alspac$g852)
alspac %>% count(g852)

attributes(alspac$g853)
alspac %>% count(g853)

attributes(alspac$g855)
alspac %>% count(g855)

attributes(alspac$g857)
alspac %>% count(g857)

attributes(alspac$g858)
alspac %>% count(g858)

alspac %>%
  summarise(
    q1 = mean(g850 < 0 | is.na(g850))*100,
    q3 = mean(g852 < 0 | is.na(g852))*100,
    q4 = mean(g853 < 0 | is.na(g853))*100,
    q5 = mean(g855 < 0 | is.na(g855))*100,
    q6 = mean(g857 < 0 | is.na(g857))*100,
    q8 = mean(g858 < 0 | is.na(g858))*100
  )

# Reorder items 4 (g853) and 8 (g858) - these are negative social experiences, so re-ordering to align with others. 
# Where 1 is negative (low social cohesion), and 5 is positive (high social cohesion). 

neighbour <- neighbour %>% mutate_at(vars(contains("g8")), as.numeric) # Change all cases to numeric. 

neighbour <- neighbour %>%
  mutate(
    g853a = case_when(
      g853 == 1 ~ 5,
      g853 == 2 ~ 4,
      g853 == 3 ~ 3,
      g853 == 4 ~ 2,
      g853 == 5 ~ 1), 
    g858a = case_when(
      g858 == 1 ~ 5,
      g858 == 2 ~ 4,
      g858 == 3 ~ 3,
      g858 == 4 ~ 2,
      g858 == 5 ~ 1))

neighbour <- neighbour %>% mutate(social_cohesion_age_1y9m = rowSums(across(c(g850, g852, g853a, g855, g857, g858a))))


###############################################################################
# Age:  33 months

attributes(alspac$h770)
alspac %>% count(h770)

attributes(alspac$h772)
alspac %>% count(h772)

attributes(alspac$h773)
alspac %>% count(h773)

attributes(alspac$h774)
alspac %>% count(h774)

attributes(alspac$h776)
alspac %>% count(h776)

attributes(alspac$h777)
alspac %>% count(h777)

alspac %>%
  summarise(
    q1 = mean(h770 < 0 | is.na(h770))*100,
    q3 = mean(h772 < 0 | is.na(h772))*100,
    q4 = mean(h773 < 0 | is.na(h773))*100,
    q5 = mean(h774 < 0 | is.na(h774))*100,
    q6 = mean(h776 < 0 | is.na(h776))*100,
    q8 = mean(h777 < 0 | is.na(h777))*100
  )

# Reorder items 4 (h773) and 8 (h777) - these are negative social experiences, so re-ordering to align with others. 
# Where 1 is negative (low social cohesion), and 5 is positive (high social cohesion). 

neighbour <- neighbour %>% mutate_at(vars(contains("h7")), as.numeric) # Change all cases to numeric. 

neighbour <- neighbour %>%
  mutate(
    h773a = case_when(
      h773 == 1 ~ 5,
      h773 == 2 ~ 4,
      h773 == 3 ~ 3,
      h773 == 4 ~ 2,
      h773 == 5 ~ 1), 
    h777a = case_when(
      h777 == 1 ~ 5,
      h777 == 2 ~ 4,
      h777 == 3 ~ 3,
      h777 == 4 ~ 2,
      h777 == 5 ~ 1))

neighbour <- neighbour %>% mutate(social_cohesion_age_2y9m = rowSums(across(c(h770, h772, h773a, h774, h776, h777a))))



###############################################################################
# Age: 5 years 1 month

attributes(alspac$k7000)
alspac %>% count(k7000)

attributes(alspac$k7002)
alspac %>% count(k7002)

attributes(alspac$k7003)
alspac %>% count(k7003)

attributes(alspac$k7010)
alspac %>% count(k7010)

attributes(alspac$k7012)
alspac %>% count(k7012)

attributes(alspac$k7013)
alspac %>% count(k7013)

alspac %>%
  summarise(
    q1 = mean(k7001 < 0 | is.na(k7001))*100,
    q3 = mean(k7002 < 0 | is.na(k7002))*100,
    q4 = mean(k7003 < 0 | is.na(k7003))*100,
    q5 = mean(k7010 < 0 | is.na(k7010))*100,
    q6 = mean(k7012 < 0 | is.na(k7012))*100,
    q8 = mean(k7013 < 0 | is.na(k7013))*100
  )

# Reorder items 4 (k7013) and 8 (k7003) - these are negative social experiences, so re-ordering to align with others. 
# Where 1 is negative (low social cohesion), and 5 is positive (high social cohesion). 

neighbour <- neighbour %>% mutate_at(vars(contains("k7")), as.numeric) # Change all cases to numeric. 

neighbour <- neighbour %>%
  mutate(
    k7013a = case_when(
      k7013 == 1 ~ 5,
      k7013 == 2 ~ 4,
      k7013 == 3 ~ 3,
      k7013 == 4 ~ 2,
      k7013 == 5 ~ 1), 
    k7003a = case_when(
      k7003 == 1 ~ 5,
      k7003 == 2 ~ 4,
      k7003 == 3 ~ 3,
      k7003 == 4 ~ 2,
      k7003 == 5 ~ 1))

neighbour <- neighbour %>% mutate(social_cohesion_age_5y1m = rowSums(across(c(k7000, k7002, k7003a, k7010, k7012, k7013a	
))))


###############################################################################
# Age: 7 years 1 month

attributes(alspac$m2230)
alspac %>% count(m2230)

attributes(alspac$m2232)
alspac %>% count(m2232)

attributes(alspac$m2233)
alspac %>% count(m2233)

attributes(alspac$m2240)
alspac %>% count(m2240)

attributes(alspac$m2242)
alspac %>% count(m2242)

attributes(alspac$m2243)
alspac %>% count(m2243)

alspac %>%
  summarise(
    q1 = mean(m2230 < 0 | is.na(m2230))*100,
    q3 = mean(m2232 < 0 | is.na(m2232))*100,
    q4 = mean(m2233 < 0 | is.na(m2233))*100,
    q5 = mean(m2240 < 0 | is.na(m2240))*100,
    q6 = mean(m2242 < 0 | is.na(m2242))*100,
    q8 = mean(m2243 < 0 | is.na(m2243))*100
  )

# Reorder items 4 (m2233) and 8 (m2243) - these are negative social experiences, so re-ordering to align with others. 
# Where 1 is negative (low social cohesion), and 5 is positive (high social cohesion). 

neighbour <- neighbour %>% mutate_at(vars(contains("m2")), as.numeric) # Change all cases to numeric. 

neighbour <- neighbour %>%
  mutate(
    m2233a = case_when(
      m2233 == 1 ~ 5,
      m2233 == 2 ~ 4,
      m2233 == 3 ~ 3,
      m2233 == 4 ~ 2,
      m2233 == 5 ~ 1), 
    m2243a = case_when(
      m2243 == 1 ~ 5,
      m2243 == 2 ~ 4,
      m2243 == 3 ~ 3,
      m2243 == 4 ~ 2,
      m2243 == 5 ~ 1))

neighbour <- neighbour %>% mutate(social_cohesion_age_7y1m = rowSums(across(c(m2230, m2232, m2233a, m2240, m2242, m2243a	
))))


###############################################################################
# Age: 10 years 1 month

attributes(alspac$q2220)
alspac %>% count(q2220)

attributes(alspac$q2222)
alspac %>% count(q2222)

attributes(alspac$q2223)
alspac %>% count(q2223)

attributes(alspac$q2230)
alspac %>% count(q2230)

attributes(alspac$q2232)
alspac %>% count(q2232)

attributes(alspac$q2233)
alspac %>% count(q2233)

alspac %>%
  summarise(
    q1 = mean(q2220 < 0 | is.na(q2220))*100,
    q3 = mean(q2222 < 0 | is.na(q2222))*100,
    q4 = mean(q2223 < 0 | is.na(q2223))*100,
    q5 = mean(q2230 < 0 | is.na(q2230))*100,
    q6 = mean(q2232 < 0 | is.na(q2232))*100,
    q8 = mean(q2233 < 0 | is.na(q2233))*100
  )

# Reorder items 4 (q2223) and 8 (q2233) - these are negative social experiences, so re-ordering to align with others. 
# Where 1 is negative (low social cohesion), and 5 is positive (high social cohesion). 

neighbour <- neighbour %>% mutate_at(vars(contains("q2")), as.numeric) # Change all cases to numeric. 

neighbour <- neighbour %>%
  mutate(
    q2223a = case_when(
      q2223 == 1 ~ 5,
      q2223 == 2 ~ 4,
      q2223 == 3 ~ 3,
      q2223 == 4 ~ 2,
      q2223 == 5 ~ 1), 
    q2233a = case_when(
      q2233 == 1 ~ 5,
      q2233 == 2 ~ 4,
      q2233 == 3 ~ 3,
      q2233 == 4 ~ 2,
      q2233 == 5 ~ 1))

neighbour <- neighbour %>% mutate(social_cohesion_age_10y1m = rowSums(across(c(q2220, q2222, q2223a, q2230, q2232, q2233a))))


###############################################################################
# Age: 18 years 6 months

attributes(alspac$t2025)
alspac %>% count(t2025)

attributes(alspac$t2027)
alspac %>% count(t2027)

attributes(alspac$t2028)
alspac %>% count(t2028)

attributes(alspac$t2030)
alspac %>% count(t2030)

attributes(alspac$t2032)
alspac %>% count(t2032)

attributes(alspac$t2033)
alspac %>% count(t2033)

alspac %>%
  summarise(
    q1 = mean(t2025 < 0 | is.na(t2025))*100,
    q3 = mean(t2027 < 0 | is.na(t2027))*100,
    q4 = mean(t2028 < 0 | is.na(t2028))*100,
    q5 = mean(t2030 < 0 | is.na(t2030))*100,
    q6 = mean(t2032 < 0 | is.na(t2032))*100,
    q8 = mean(t2033 < 0 | is.na(t2033))*100
  )

# Reorder items 4 (t2028) and 8 (t2033) - these are negative social experiences, so re-ordering to align with others. 
# Where 1 is negative (low social cohesion), and 5 is positive (high social cohesion). 

neighbour <- neighbour %>% mutate_at(vars(contains("t2")), as.numeric) # Change all cases to numeric. 

neighbour <- neighbour %>%
  mutate(
    t2028a = case_when(
      t2028 == 1 ~ 5,
      t2028 == 2 ~ 4,
      t2028 == 3 ~ 3,
      t2028 == 4 ~ 2,
      t2028 == 5 ~ 1), 
    t2033a = case_when(
      t2033 == 1 ~ 5,
      t2033 == 2 ~ 4,
      t2033 == 3 ~ 3,
      t2033 == 4 ~ 2,
      t2033 == 5 ~ 1))

neighbour <- neighbour %>% mutate(social_cohesion_age_18y6m = rowSums(across(c(t2025, t2027, t2028a, t2030, t2032, t2033a))))


# Missingness on total neighbourhood scores.

neighbour %>% summarise(
  Age_Gestation =  mean(is.na(social_cohesion_age_0y0m))*100,
  Age_0y8m =  mean(is.na(social_cohesion_age_0y8m))*100,
  Age_1y9m =  mean(is.na(social_cohesion_age_1y9m))*100,
  Age_2y9m =  mean(is.na(social_cohesion_age_2y9m))*100,
  Age_5y1m =  mean(is.na(social_cohesion_age_5y1m))*100,
  Age_7y1m =  mean(is.na(social_cohesion_age_7y1m))*100,
  Age_10y1m = mean(is.na(social_cohesion_age_10y1m))*100,
  Age_18y6m = mean(is.na(social_cohesion_age_18y6m))*100)

###############################################################################
#Neighbour discord
###############################################################################

attributes(alspac$a602)	#0 years 0 months
alspac %>% count(a602) 
attributes(alspac$a606)	
alspac %>% count(a606)
attributes(alspac$f891) # 0 years 8 months	
alspac %>% count(f891)
attributes(alspac$f896)	
alspac %>% count(f896)
attributes(alspac$g851)	# 1 year 9 months
alspac %>% count(g851)
attributes(alspac$g856)	
alspac %>% count(g856)
attributes(alspac$h771)	 # 2 years 9 months
alspac %>% count(h771)
attributes(alspac$h775)	
alspac %>% count(h775)
attributes(alspac$k7001)	 # 5 years 1 month
alspac %>% count(k7001)
attributes(alspac$k7011)	
alspac %>% count(k7011)
attributes(alspac$m2231)	 # 7 years 1 month 
alspac %>% count(m2231)
attributes(alspac$m2241)	
alspac %>% count(m2241)
attributes(alspac$q2221)	# 10 years 1 month
alspac %>% count(q2221)
attributes(alspac$q2231)	
alspac %>% count(q2231)
attributes(alspac$t2026)  # 18 years 6 months
alspac %>% count(t2026)
attributes(alspac$t2031)	
alspac %>% count(t2031)
 
# Exclude missing cases on consituent variables, then generate sum score.
# High scores indicate more discord
neighbour <- neighbour %>% mutate(
  a602a =  case_when(a602 > 0 ~ a602),
  f891a =  case_when(f891 > 0 ~ f891),
  g851a =  case_when(g851 > 0 ~ g851),
  h771a =  case_when(h771 > 0 ~ h771),
  k7001a =  case_when(k7001 > 0 ~ k7001),
  m2231a =  case_when(m2231 > 0 ~ m2231),
  q2221a =  case_when(q2221 > 0 ~ q2221),
  t2026a =  case_when(t2026 > 0 ~ t2026),
  a606a =  case_when(a606 > 0 ~ a606),
  f896a =  case_when(f896 > 0 ~ f896),
  g856a =  case_when(g856 > 0 ~ g856),
  h775a =  case_when(h775 > 0 ~ h775),
  k7011a =  case_when(k7011 > 0 ~ k7011),
  m2241a =  case_when(m2241 > 0 ~ m2241),
  q2231a =  case_when(q2231 > 0 ~ q2231),
  t2031a =  case_when(t2031 > 0 ~ t2031),
  social_discord_age_0y0m = a602a + a606a,
  social_discord_age_0y8m  = f891a + f896a,
  social_discord_age_1y9m  = g851a + g856a,
  social_discord_age_2y9m  = h771a + h775a,
  social_discord_age_5y1m  = k7001a + k7011a,
  social_discord_age_7y1m  = m2231a + m2241a,
  social_discord_age_10y1m  = q2221a + q2231a,
  social_discord_age_18y6m  = t2026a + t2031a)

neighbour %>% select(social_discord_age_0y0m, a602a, a606a) %>% glimpse()
neighbour %>% select(social_discord_age_2y9m, h771a, h775a) %>% head()

neighbour %>% count(social_discord_age_0y0m)
neighbour %>% count(social_discord_age_2y9m)

neighbour %>%
  summarise(
    social_discord_age_0y0m_mean = mean(social_discord_age_0y0m, na.rm=TRUE),
    social_discord_age_0y8m_mean = mean(social_discord_age_0y8m, na.rm=TRUE),
    social_discord_age_1y9m_mean = mean(social_discord_age_1y9m, na.rm=TRUE),
    social_discord_age_2y9m_mean = mean(social_discord_age_2y9m, na.rm=TRUE),
    social_discord_age_5y1m_mean = mean(social_discord_age_5y1m, na.rm=TRUE),
    social_discord_age_7y1m_mean = mean(social_discord_age_7y1m, na.rm=TRUE),
    social_discord_age_10y1m_mean = mean(social_discord_age_10y1m, na.rm=TRUE),
    social_discord_age_18y6m_mean = mean(social_discord_age_18y6m, na.rm=TRUE)) %>%
  pivot_longer(cols = starts_with("social"), names_to = "Variable", values_to = "Mean")

neighbour %>%
  summarise(
    social_discord_age_0y0m_min = min(social_discord_age_0y0m, na.rm=TRUE),
    social_discord_age_0y8m_min = min(social_discord_age_0y8m, na.rm=TRUE),
    social_discord_age_1y9mmin = min(social_discord_age_1y9m, na.rm=TRUE),
    social_discord_age_2y9mmin = min(social_discord_age_2y9m, na.rm=TRUE),
    social_discord_age_5y1mmin = min(social_discord_age_5y1m, na.rm=TRUE),
    social_discord_age_7y1mmin = min(social_discord_age_7y1m, na.rm=TRUE),
    social_discord_age_10y1mmin = min(social_discord_age_10y1m, na.rm=TRUE),
    social_discord_age_18y6mmin = min(social_discord_age_18y6m, na.rm=TRUE))%>%
pivot_longer(cols = starts_with("social"), names_to = "Variable", values_to = "Min")

###############################################################################
#Neighbourhood stress score
###############################################################################

#Constituent questions:
#Q9	What is your opinion of your neighborhood?	A, F, G, H, K, M, Q
#Q10	How much of a problem is noise from other people's homes?	G, H, K, M, Q
#Q11	How much of a problem is noise from outside in the street?	G, H, K, M, Q
#Q12	How much of a problem is rubbish or litter dumped in the neighborhood?	G, H, K, M, Q
#Q13	How much of a problem is dog dirt on pavements/walkways?	G, H, K, M, Q
#Q14	How much of a problem is worrying about vandalism?	G, H, K, M, Q
#Q15	How much of a problem is worrying about burglaries?	G, H, K, M, Q
#Q16	How much of a problem is worrying about mugging or attacks?	G, H, K, M, Q
#Q17	How much of a problem is disturbance from teenagers or youths?	G, H, K, M, Q


alspac %>% select(g860, 	g489, 	g491, 	g492, 	g493, 	g494, 	g495, 	h778, 	h357, 	h358, 	h359, 	h360, 	h361, 	h362, 	h363, 	h364, 	k7020, 	k5213, 	k5214, 	k5215, 	k5216, 	k5217, 	k5218, 	k5219, 	k5220, 	m2250, 	m2203, 	m2204, 	m2205, 	m2206, 	m2207, 	m2208, 	m2209, 	m2210, 	q2240, 	q2203, 	q2204, 	q2205, 	q2206, 	q2207, 	q2208, 	q2209, 	q2210, 	q2203, 	t2035, 	t2010, 	t2011, 	t2012, 	t2013, 	t2014, 	t2015, 	t2016, 	t2017, 	t2010) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(g860, 	g489, 	g491, 	g492, 	g493, 	g494, 	g495, 	h778, 	h357, 	h358, 	h359, 	h360, 	h361, 	h362, 	h363, 	h364, 	k7020, 	k5213, 	k5214, 	k5215, 	k5216, 	k5217, 	k5218, 	k5219, 	k5220, 	m2250, 	m2203, 	m2204, 	m2205, 	m2206, 	m2207, 	m2208, 	m2209, 	m2210, 	q2240, 	q2203, 	q2204, 	q2205, 	q2206, 	q2207, 	q2208, 	q2209, 	q2210, 	q2203, 	t2035, 	t2010, 	t2011, 	t2012, 	t2013, 	t2014, 	t2015, 	t2016, 	t2017, 	t2010))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# Q9 is coded 1 - 'very good' to 4 - 'not at all good' for how good the neighbourhood is to live in. 
# The others are the reverse, 3 is not a problem, 1 is a serious problem. 
# Reverse coding for Q9 so high values are good, low values are bad. 
# 4 on q10 onwards is 'no opinion' coded as missing. 

#Reverse coding of Q9. 
neighbour  <- neighbour %>% mutate(
  g860a = case_when(g860 > 0 ~ 5 - g860),
  h778a = case_when(h778 > 0 ~ 5 - h778),
  k7020a = case_when(k7020 > 0 ~ 5 - k7020),
  m2250a = case_when(m2250 > 0 ~ 5 - m2250),
  q2240a = case_when(q2240 > 0 ~ 5 - q2240),
  t2035a = case_when(t2035 > 0 ~ 5 - t2035))

neighbour %>% count(g860, g860a)
neighbour %>% count(h778, h778a)
neighbour %>% count(k7020, k7020a)
neighbour %>% count(m2250, m2250a)
neighbour %>% count(q2240, q2240a)
neighbour %>% count(t2035, t2035a)


for (var in names(neighbour %>% select(g860a, h778a, k7020a, m2250a, q2240a, t2035a))) {
  neighbour %>% count(.data[[var]]) %>% print()
}                                                                                                          

#Remove missing values from other items. 
neighbour <- neighbour %>% mutate(
  g488a = case_when(g488 > 0 & g488 < 4 ~ g488),
  g489a = case_when(g489 > 0 & g489 < 4 ~ g489),
  g490a = case_when(g490 > 0 & g490 < 4 ~ g490),
  g491a = case_when(g491 > 0 & g491 < 4 ~ g491),
  g492a = case_when(g492 > 0 & g492 < 4 ~ g492),
  g493a = case_when(g493 > 0 & g493 < 4 ~ g493),
  g494a = case_when(g494 > 0 & g494 < 4 ~ g494),
  g495a = case_when(g495 > 0 & g495 < 4 ~ g495),
  h357a = case_when(h357 > 0 & h357 < 4 ~ h357),
  h358a = case_when(h358 > 0 & h358 < 4 ~ h358),
  h359a = case_when(h359 > 0 & h359 < 4 ~ h359),
  h360a = case_when(h360 > 0 & h360 < 4 ~ h360),
  h361a = case_when(h361 > 0 & h361 < 4 ~ h361),
  h362a = case_when(h362 > 0 & h362 < 4 ~ h362),
  h363a = case_when(h363 > 0 & h363 < 4 ~ h363),
  h364a = case_when(h364 > 0 & h364 < 4 ~ h364),
  k5213a = case_when(k5213 > 0 & k5213 < 4 ~ k5213),
  k5214a = case_when(k5214 > 0 & k5214 < 4 ~ k5214),
  k5215a = case_when(k5215 > 0 & k5215 < 4 ~ k5215),
  k5216a = case_when(k5216 > 0 & k5216 < 4 ~ k5216),
  k5217a = case_when(k5217 > 0 & k5217 < 4 ~ k5217),
  k5218a = case_when(k5218 > 0 & k5218 < 4 ~ k5218),
  k5219a = case_when(k5219 > 0 & k5219 < 4 ~ k5219),
  k5220a = case_when(k5220 > 0 & k5220 < 4 ~ k5220),
  m2203a = case_when(m2203 > 0 & m2203 < 4 ~ m2203),
  m2204a = case_when(m2204 > 0 & m2204 < 4 ~ m2204),
  m2205a = case_when(m2205 > 0 & m2205 < 4 ~ m2205),
  m2206a = case_when(m2206 > 0 & m2206 < 4 ~ m2206),
  m2207a = case_when(m2207 > 0 & m2207 < 4 ~ m2207),
  m2208a = case_when(m2208 > 0 & m2208 < 4 ~ m2208),
  m2209a = case_when(m2209 > 0 & m2209 < 4 ~ m2209),
  m2210a = case_when(m2210 > 0 & m2210 < 4 ~ m2210),
  q2203a = case_when(q2203 > 0 & q2203 < 4 ~ q2203),
  q2204a = case_when(q2204 > 0 & q2204 < 4 ~ q2204),
  q2205a = case_when(q2205 > 0 & q2205 < 4 ~ q2205),
  q2206a = case_when(q2206 > 0 & q2206 < 4 ~ q2206),
  q2207a = case_when(q2207 > 0 & q2207 < 4 ~ q2207),
  q2208a = case_when(q2208 > 0 & q2208 < 4 ~ q2208),
  q2209a = case_when(q2209 > 0 & q2209 < 4 ~ q2209),
  q2210a = case_when(q2210 > 0 & q2210 < 4 ~ q2210),
  t2010a = case_when(t2010 > 0 & t2010 < 4 ~ t2010),
  t2011a = case_when(t2011 > 0 & t2011 < 4 ~ t2011),
  t2012a = case_when(t2012 > 0 & t2012 < 4 ~ t2012),
  t2013a = case_when(t2013 > 0 & t2013 < 4 ~ t2013),
  t2014a = case_when(t2014 > 0 & t2014 < 4 ~ t2014),
  t2015a = case_when(t2015 > 0 & t2015 < 4 ~ t2015),
  t2016a = case_when(t2016 > 0 & t2016 < 4 ~ t2016),
  t2017a = case_when(t2017 > 0 & t2017 < 4 ~ t2017))

# Check some of derived variables
neighbour %>% count(g488, g488a)
neighbour %>% count(g488, g488a)
neighbour %>% count(q2207, q2207a)


for (var in names(neighbour %>% select(g488a, 	g489a, 	g490a, 	g491a, 	g492a, 	g493a, 	g494a, 	g495a, 	h357a, 	h358a, 	h359a, 	h360a, 	h361a, 	h362a, 	h363a, 	h364a, 	k5213a, 	k5214a, 	k5215a, 	k5216a, 	k5217a, 	k5218a, 	k5219a, 	k5220a, 	m2203a, 	m2204a, 	m2205a, 	m2206a, 	m2207a, 	m2208a, 	m2209a, 	m2210a, 	q2203a, 	q2204a, 	q2205a, 	q2206a, 	q2207a, 	q2208a, 	q2209a, 	q2210a, 	q2203a, 	t2010a, 	t2011a, 	t2012a, 	t2013a, 	t2014a, 	t2015a, 	t2016a, 	t2017a, 	t2010a))) {
  neighbour %>% count(.data[[var]]) %>% print()
}

# Derive stress score
neighbour <- neighbour %>%
  mutate(
    neighbour_stress_score_age_1y9m  =  g860a + g488a + g489a + g490a + g491a + g492a + g493a + g494a + g495,
    neighbour_stress_score_age_2y9m  =  h778a + h357a + h358a + h359a + h360a + h361a + h362a + h363a + h364,
    neighbour_stress_score_age_5y1m  =  k7020a + k5213a + k5214a + k5215a + k5216a + k5217a + k5218a + k5219a + k5220,
    neighbour_stress_score_age_7y1m  =  m2250a + m2203a + m2204a + m2205a + m2206a + m2207a + m2208a + m2209a + m2210,
    neighbour_stress_score_age_10y1m  =  q2240a + q2203a + q2204a + q2205a + q2206a + q2207a + q2208a + q2209a + q2210a,
    neighbour_stress_score_age_18y6m  =  t2035a + t2010a + t2011a + t2012a + t2013a + t2014a + t2015a + t2016a + t2017a)

# Check stress score
neighbour %>% select(neighbour_stress_score_age_1y9m,  g860a, g488a, g489a, g490a, g491a, g492a, g493a, g494a, g495) %>% head()
neighbour %>% select(neighbour_stress_score_age_10y1m, q2240a, q2203a, q2204a, q2205a, q2206a, q2207a, q2208a, q2209a, q2210a) %>% head()

# Review missingness level
neighbour %>%
  summarise(
    age1y9m   = mean(is.na(neighbour_stress_score_age_1y9m  ))*100,
    age2y9m   = mean(is.na(neighbour_stress_score_age_2y9m  ))*100,
    age5y1m   = mean(is.na(neighbour_stress_score_age_5y1m  ))*100,
    age7y1m   = mean(is.na(neighbour_stress_score_age_7y1m  ))*100,
    age10y1m   = mean(is.na(neighbour_stress_score_age_10y1m  ))*100,
    age18y6m   = mean(is.na(neighbour_stress_score_age_18y6m  ))*100)

###############################################################################
# School connection domain
###############################################################################

#### Review the data

#### Enjoys school

alspac %>% count(kk459) #age 4 years 6 months
attributes(alspac$kk459)

alspac %>% count(km4241) #age 5 years 5 months
attributes(alspac$km4241)

alspac %>% count(kp1131) #age 6 years 5 months
attributes(alspac$kp1131)

alspac %>% count(kr621) #age 7 years 9 months
attributes(alspac$kr621)

alspac %>% count(ku181) #age 9 years 7 months
attributes(alspac$ku181)

alspac %>% count(kv9101) #age 10 years 8 months
attributes(alspac$kv9101)

alspac %>% count(kw7051) #age 11 years 8 months
attributes(alspac$kw7051)

alspac %>% count(tb9101) #age 14 years 2 months
attributes(alspac$tb9101)

#### Looks forward to seeing teacher - exclude, only one timepoint

alspac %>% count(kw7050) #age 11 years 2 months
attributes(alspac$kw7050)

#### Likes teacher: kk464, km4246, kp1136, kr626

alspac %>% count(kk464) #age 4 years 6 months
attributes(alspac$kk464)
alspac %>% count(km4246) #age 5 years 5 months
attributes(alspac$km4246)
alspac %>% count(kp1136) #age 6 years 5 months
attributes(alspac$kp1136)
alspac %>% count(kr626) #age 7 years 9 months
attributes(alspac$kr626)

#### Afraid of teacher

#Issue with variable, no 'no' responses. Do not import. 
alspac %>% count(ku307) #age 10 years 8 months
attributes(alspac$ku307)

#### Talks about school friends

alspac %>% count(kk462) #age 4 years 6 months
attributes(alspac$kk462)
alspac %>% count(km4244) #age 5 years 5 months
attributes(alspac$km4244)
alspac %>% count(kp1134) #age 6 years 5 months
attributes(alspac$kp1134)
alspac %>% count(kr624) #age 7 years 9 months
attributes(alspac$kr624)
alspac %>% count(kv9104) #age 10 years 8 months
attributes(alspac$kv9104)
alspac %>% count(tb9104) #age 14 years 2 months
attributes(alspac$tb9104)

#### Select school variables, create separate dataframe for them

school_connection <- alspac %>%
  select(uniqid, kk459, km4241, kp1131, kr621, ku181, kv9101, kw7050, tb9101,
         kk464, km4246, kp1136, kr626,
         kk462, km4244, kp1134, kr624, kv9104, tb9104, 
         ccc250, 	ccd120, 	ccd145, 	ccd170, 	ccd130, 	ku187, 	ku184, 	ccj105, 	ccj115, 	ccj123, 	ccj133, 	ccj145, 	ccj152, 	ccj101, 	kw7057, 	ccr102, 	ccr106, 	ccr109, 	ccr116, 	ccr117, 	ccxa111, ku307)

# Remove missing values
school_connection <- school_connection %>%
  mutate(
    school_talk_friends_age_4y6m = case_when(kk462 > 0 ~ kk462),
    school_talk_friends_age_5y5m = case_when(km4244 > 0 ~ km4244),
    school_talk_friends_age_6y5m = case_when(kp1134 > 0 ~ kp1134),
    school_talk_friends_age_7y9m = case_when(kr624 > 0 ~ kr624),
    school_talk_friends_age_10y8m = case_when(kv9104 > 0 ~ kv9104),
    school_talk_friends_age_14y2m = case_when(tb9104 > 0 ~ tb9104),
    school_likes_teacher_age_4y6m = case_when(kk464 > 0 ~ kk464),
    school_likes_teacher_age_5y5m = case_when(km4246 > 0 ~ km4246),
    school_likes_teacher_age_6y5m = case_when(kp1136 > 0 ~ kp1136),
    school_likes_teacher_age_7y9m = case_when(kr626 > 0 ~ kr626),
    school_looks_forward_see_teacher_age_10y8m = case_when(kw7050 > 0 ~ kw7050)
  )

# Compare old variables with new ones
school_connection %>% count(school_talk_friends_age_4y6m, kk462)
school_connection %>% count(school_talk_friends_age_5y5m, km4244)
school_connection %>% count(school_talk_friends_age_6y5m, kp1134)
school_connection %>% count(school_talk_friends_age_7y9m, kr624)
school_connection %>% count(school_talk_friends_age_10y8m, kv9104)
school_connection %>% count(school_talk_friends_age_14y2m, tb9104)
school_connection %>% count(school_likes_teacher_age_4y6m, kk464)
school_connection %>% count(school_likes_teacher_age_5y5m, km4246)
school_connection %>% count(school_likes_teacher_age_6y5m, kp1136)
school_connection %>% count(school_likes_teacher_age_7y9m, kr626)
school_connection %>% count(school_looks_forward_see_teacher_age_10y8m, kw7050)

###############################################################################
### Additional variables in the school domain. 

# Review the data
# Selected items added to creation of school_connection dataframe above.
alspac %>% count(ccc250)	
attributes(alspac$ccc250)

alspac %>% count(ccd120)	
attributes(alspac$ccd120)

alspac %>% count(ccd145)	
attributes(alspac$ccd145)

alspac %>% count(ccd170)	
attributes(alspac$ccd170)

alspac %>% count(ku250)	#Only 1 valid value (n=250), all other cases missing. Exclude. 
attributes(alspac$ku250)

alspac %>% count(ccd130)	
attributes(alspac$ccd130)

alspac %>% count(ku187)	
attributes(alspac$ku187)

alspac %>% count(ku184)	
attributes(alspac$ku184)

alspac %>% count(ccj105)
attributes(alspac$ccj105)

alspac %>% count(ccj115)	
attributes(alspac$ccj115)

alspac %>% count(ccj123)	
attributes(alspac$ccj123)

alspac %>% count(ccj133)
attributes(alspac$ccj133)

alspac %>% count(ccj145)
attributes(alspac$ccj145)

alspac %>% count(ccj152)
attributes(alspac$ccj152)

alspac %>% count(ccj101)
attributes(alspac$ccj101)

alspac %>% count(kw7057)
attributes(alspac$kw7057)

alspac %>% count(ccr102)
attributes(alspac$ccr102)

alspac %>% count(ccr106)
attributes(alspac$ccr106)

alspac %>% count(ccr109)
attributes(alspac$ccr109)

alspac %>% count(ccr116)
attributes(alspac$ccr116)

alspac %>% count(ccr117)
attributes(alspac$ccr117)

alspac %>% count(ccxa111)	
attributes(alspac$ccxa111)

# Rename varaibles, and removing missng values. 
school_connection <- school_connection %>%
  mutate(
    school_left_out_age_8y1m = case_when(ccc250 > 0 ~ ccc250),
    school_everybodys_friend_age_8y7m = case_when(ccd120 > 0  ~ ccd120),
    school_classmates_not_all_their_friends_age_8y7m = case_when(ccd145 > 0  ~ ccd145),
    school_all_pupils_are_friends_age_8y7m = case_when(ccd170 > 0  ~ ccd170),
    school_some_classmates_mean_age_8y7m = case_when(ccd130 > 0  ~ ccd130),
    school_likes_schoolmates_age_10y8m = case_when(ku187 > 0  ~ ku187),
    school_frightened_by_schoolmates_age_10y8m = case_when(ku184 > 0  ~ ku184),
    school_schoolmates_accept_them_age_11y2m = case_when(ccj105 > 0  ~ ccj105),
    school_feel_lonely_age_11y2m = case_when(ccj115 > 0  ~ ccj115),
    school_people_trust_them_age_11y2m = case_when(ccj123 > 0  ~ ccj123),
    school_feel_popular_age_11y2m = case_when(ccj133 > 0  ~ ccj133),
    school_get_on_with_classmates_age_11y2m = case_when(ccj145 > 0 ~ ccj145),
    school_people_depend_on_them_age_11y2m = case_when(ccj152 > 0  ~ ccj152),
    school_likes_schoolmates_age_11y8m = case_when(kw7057 > 0  ~ kw7057),
    school_schoolmates_accept_them_age_14y1m = case_when(ccr102 > 0  ~ ccr102),
    school_feel_lonely_age_14y1m = case_when(ccr106 > 0 ~ ccr106),
    school_people_trust_them_age_14y1m = case_when(ccr109 > 0 ~ ccr109),
    school_get_on_well_with_classmates_age_14y1m = case_when(ccr116 > 0 ~ ccr116),
    school_people_depend_on_them_age_14y1m = case_when(ccr117 > 0 ~ ccr117),
    school_get_on_with_classmates_age_16y = case_when(ccxa111 > 0 ~ ccxa111))

# Compare old to new variables    
school_connection %>% count(school_left_out_age_8y1m , ccc250)
school_connection %>% count(school_everybodys_friend_age_8y7m , ccd120)
school_connection %>% count(school_classmates_not_all_their_friends_age_8y7m , ccd145)
school_connection %>% count(school_all_pupils_are_friends_age_8y7m , ccd170)
school_connection %>% count(school_some_classmates_mean_age_8y7m , ccd130)
school_connection %>% count(school_likes_schoolmates_age_10y8m , ku187)
school_connection %>% count(school_frightened_by_schoolmates_age_10y8m , ku184)
school_connection %>% count(school_schoolmates_accept_them_age_11y2m , ccj105)
school_connection %>% count(school_feel_lonely_age_11y2m , ccj115)
school_connection %>% count(school_people_trust_them_age_11y2m , ccj123)
school_connection %>% count(school_feel_popular_age_11y2m , ccj133)
school_connection %>% count(school_get_on_with_classmates_age_11y2m , ccj145)
school_connection %>% count(school_people_depend_on_them_age_11y2m , ccj152)
school_connection %>% count(school_likes_schoolmates_age_11y8m , kw7057)
school_connection %>% count(school_schoolmates_accept_them_age_14y1m , ccr102)
school_connection %>% count(school_feel_lonely_age_14y1m , ccr106)
school_connection %>% count(school_people_trust_them_age_14y1m , ccr109)
school_connection %>% count(school_get_on_well_with_classmates_age_14y1m , ccr116)
school_connection %>% count(school_people_depend_on_them_age_14y1m , ccr117)
school_connection %>% count(school_get_on_with_classmates_age_16y , ccxa111)
# Don't know retained as a valid option - high level of don't know responses. 

################################################################################
  ### Life events domain
################################################################################

# Select variables
life_events <- alspac %>% select(uniqid,
  kd500b, 	kd501b, 	kd502b, 	kd503b, 	kd504b, 	kd505b, 	kd506b, 	kd507b, 	kd508b, 	kd509b, 	kd510b, 	kd511b, 	kd512b, 	kd513b, 	kd514b, 	kf450a, 	kf451a, 	kf452a, 	kf453a, 	kf454a, 	kf455a, 	kf456a, 	kf457a, 	kf458a, 	kf459a, 	kf460a, 	kf461a, 	kf462a, 	kf463a, 	kf464a, 	kj462a, 	kj463a, 	kj464a, 	kj465a, 	kj466a, 	kj467a, 	kj468a, 	kj469a, 	kj470a, 	kj471a, 	kj472a, 	kj473a, 	kj474a, 	kl470, 	kl471, 	kl472, 	kl473, 	kl474, 	kl475, 	kl476, 	kl477, 	kl478, 	kl479, 	kl480, 	kl481, 	kl482, 	kl483, 	kl484, 	kl485, 	kn4000, 	kn4001, 	kn4002, 	kn4003, 	kn4004, 	kn4005, 	kn4006, 	kn4007, 	kn4008, 	kn4009, 	kn4010, 	kn4011, 	kn4012, 	kn4013, 	kn4014, 	kn4015, 	kq376a, 	kt5000, 	kt5001, 	kt5002, 	kt5003, 	kt5004, 	kt5005, 	kt5006, 	kt5007, 	kt5008, 	kt5009, 	kt5010, 	kt5011, 	kt5012, 	kt5013, 	kt5014, 	kt5015, 	kt5016, 	ccs2000, 	ccs2010, 	ccs2020, 	ccs2030, 	ccs2040, 	ccs2050, 	ccs2060, 	ccs2070, 	ccs2080, 	ccs2090, 	ccs2100, 	ccs2110, 	ccs2120, 	ccs2130, 	ccs2140, 	ccs2150, 	ccs2160, 	ccs2170, 	ccs2180, 	ccs2190, 	ccs2210, 	ccs2220, 	YPB6000, 	YPB6010, 	YPB6020, 	YPB6030, 	YPB6040, 	YPB6050, 	YPB6060, 	YPB6070, 	YPB6080, 	YPB6090, 	YPB6100, 	YPB6110, 	YPB6120, 	YPB6130, 	YPB6140, 	YPB6150, 	YPB6160, 	YPB6170, 	YPB6180, 	YPB6190, 	YPB6210, 	YPB6220, 	YPB6230, 	YPC2150, 	YPC2160, 	YPC2170, 	YPC2180, 	YPC2190, 	YPC2200, 	YPC2210, 	YPC2220, 	YPC2230, 	YPC2240, 	YPC2250, 	YPC2260, 	YPC2270, 	YPC2280, 	YPC2290, 	YPC2300, 	YPC2310, 	YPC2320, 	YPC2330, 	YPC2340, 	YPC2350, 	YPC2360, 	YPC2380, 	YPC2390, 	YPC2400, 	YPC2410, 	YPD1000, 	YPD1010, 	YPD1020, 	YPD1030, 	YPD1040, 	YPD1050, 	YPD1060, 	YPD1070, 	YPD1080, 	YPD1090, 	YPD1100, 	YPD1110, 	YPD1120, 	YPD1130, 	YPD1140, 	YPD1150, 	YPD1160, 	YPD1170, 	YPD1180, 	YPD1190, 	YPD1200, 	YPD1210, 	YPD1230, 	YPD1240, 	YPD1250, 	YPD1260
)

# Varlist of one variable from each wave: 
varlist <- c("kd500b", "kf450a", "kj462a","kl470", "kn4000", "kq376a", "kt5000", "kt5001", "ccs2000", "YPB6000", "YPC2150", "YPD1000")
varlist

# Check structure of these variables. 
alspac %>% select(varlist) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(varlist))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# Move all variables to a binary. Some have multiple response categories, some are yes/no. 
# Note: 0 is a valid value in some years, in others it is 1. 

# Derive new variables
life_events <- life_events %>%
  mutate(
    life_event_child_taken_into_care_age_1y_6m = case_when(kd500b > 0 ~ kd500b),
    life_event_control_pet_died_age_1y_6m = case_when(kd501b > 0 ~ kd501b),
    life_event_moved_home_age_1y_6m = case_when(kd502b > 0 ~ kd502b),
    life_event_control_had_fright_age_1y_6m = case_when(kd503b > 0 ~ kd503b),
    life_event_control_physically_hurt_by_someone_age_1y_6m = case_when(kd504b > 0 ~ kd504b),
    life_event_control_sexually_abused_age_1y_6m = case_when(kd505b > 0 ~ kd505b),
    life_event_separated_from_mum_for_over_a_wk_age_1y_6m = case_when(kd506b > 0 ~ kd506b),
    life_event_separated_from_dad_for_over_a_wk_age_1y_6m = case_when(kd507b > 0 ~ kd507b),
    life_event_acquired_a_new_parent_age_1y_6m = case_when(kd508b > 0 ~ kd508b),
    life_event_had_a_new_sibling_age_1y_6m = case_when(kd509b > 0 ~ kd509b),
    life_event_control_admitted_to_hospital_age_1y_6m = case_when(kd510b > 0 ~ kd510b),
    life_event_changed_carer_age_1y_6m = case_when(kd511b > 0 ~ kd511b),
    life_event_separated_from_someone_else_age_1y_6m = case_when(kd512b > 0 ~ kd512b),
    life_event_started_nursery_age_1y_6m = case_when(kd513b > 0 ~ kd513b),
    life_event_control_had_other_upsetting_event_age_1y_6m = case_when(kd514b > 0 ~ kd514b),
    life_event_child_taken_into_care_age_2y_6m = case_when(kf450a > 0 ~ kf450a),
    life_event_pet_died_age_2y_6m = case_when(kf451a > 0 ~ kf451a),
    life_event_moved_home_age_2y_6m = case_when(kf452a > 0 ~ kf452a),
    life_event_had_fright_age_2y_6m = case_when(kf453a > 0 ~ kf453a),
    life_event_physically_hurt_by_someone_age_2y_6m = case_when(kf454a > 0 ~ kf454a),
    life_event_sexually_abused_age_2y_6m = case_when(kf455a > 0 ~ kf455a),
    life_event_separated_from_mum_for_over_a_wk_age_2y_6m = case_when(kf456a > 0 ~ kf456a),
    life_event_separated_from_dad_for_over_a_wk_age_2y_6m = case_when(kf457a > 0 ~ kf457a),
    life_event_acquired_a_new_parent_age_2y_6m = case_when(kf458a > 0 ~ kf458a),
    life_event_had_a_new_sibling_age_2y_6m = case_when(kf459a > 0 ~ kf459a),
    life_event_control_admitted_to_hospital_age_2y_6m = case_when(kf460a > 0 ~ kf460a),
    life_event_changed_carer_age_2y_6m = case_when(kf461a > 0 ~ kf461a),
    life_event_separated_from_someone_else_age_2y_6m = case_when(kf462a > 0 ~ kf462a),
    life_event_started_nursery_age_2y_6m = case_when(kf463a > 0 ~ kf463a),
    life_event_control_had_other_upsetting_event_age_2y_6m = case_when(kf464a > 0 ~ kf464a),
    life_event_moved_home_age_3y_6m = case_when(kj462a > 0 ~ kj462a),
    life_event_control_had_fright_age_3y_6m = case_when(kj463a > 0 ~ kj463a),
    life_event_control_physically_hurt_by_someone_age_3y_6m = case_when(kj464a > 0 ~ kj464a),
    life_event_control_sexually_abused_age_3y_6m = case_when(kj465a > 0 ~ kj465a),
    life_event_separated_from_mum_for_over_a_wk_age_3y_6m = case_when(kj466a > 0 ~ kj466a),
    life_event_separated_from_dad_for_over_a_wk_age_3y_6m = case_when(kj467a > 0 ~ kj467a),
    life_event_acquired_a_new_parent_age_3y_6m = case_when(kj468a > 0 ~ kj468a),
    life_event_had_a_new_sibling_age_3y_6m = case_when(kj469a > 0 ~ kj469a),
    life_event_control_admitted_to_hospital_age_3y_6m = case_when(kj470a > 0 ~ kj470a),
    life_event_changed_carer_age_3y_6m = case_when(kj471a > 0 ~ kj471a),
    life_event_separated_from_someone_else_age_3y_6m = case_when(kj472a > 0 ~ kj472a),
    life_event_started_nursery_age_3y_6m = case_when(kj473a > 0 ~ kj473a),
    life_event_control_had_other_upsetting_event_age_3y_6m = case_when(kj474a > 0 ~ kj474a),
    life_event_child_taken_into_care_age_4y_9m = as_factor(case_when(kl470 > 0 & kl470 < 5 ~ 'Yes', kl470 == 5 ~ 'No')),
    life_event_control_pet_died_age_4y_9m = as_factor(case_when(kl471 > 0 & kl471 < 5 ~ 'Yes', kl471 == 5 ~ 'No')),
    life_event_control_moved_home_age_4y_9m = as_factor(case_when(kl472 > 0 & kl472 < 5 ~ 'Yes', kl472 == 5 ~ 'No')),
    life_event_control_had_fright_age_4y_9m = as_factor(case_when(kl473 > 0 & kl473 < 5 ~ 'Yes', kl473 == 5 ~ 'No')),
    life_event_control_physically_hurt_by_someone_age_4y_9m = as_factor(case_when(kl474 > 0 & kl474 < 5 ~ 'Yes', kl474 == 5 ~ 'No')),
    life_event_control_sexually_abused_age_4y_9m = as_factor(case_when(kl475 > 0 & kl475 < 5 ~ 'Yes', kl475 == 5 ~ 'No')),
    life_event_separated_from_mum_for_over_a_wk_age_4y_9m = as_factor(case_when(kl476 > 0 & kl476 < 5 ~ 'Yes', kl476 == 5 ~ 'No')),
    life_event_separated_from_dad_for_over_a_wk_age_4y_9m = as_factor(case_when(kl477 > 0 & kl477 < 5 ~ 'Yes', kl477 == 5 ~ 'No')),
    life_event_acquired_a_new_parent_age_4y_9m = as_factor(case_when(kl478 > 0 & kl478 < 5 ~ 'Yes', kl478 == 5 ~ 'No')),
    life_event_had_a_new_sibling_age_4y_9m = as_factor(case_when(kl479 > 0 & kl479 < 5 ~ 'Yes', kl479 == 5 ~ 'No')),
    life_event_control_admitted_to_hospital_age_4y_9m = as_factor(case_when(kl480 > 0 & kl480 < 5 ~ 'Yes', kl480 == 5 ~ 'No')),
    life_event_changed_carer_age_4y_9m = as_factor(case_when(kl481 > 0 & kl481 < 5 ~ 'Yes', kl481 == 5 ~ 'No')),
    life_event_separated_from_someone_else_age_4y_9m = as_factor(case_when(kl482 > 0 & kl482 < 5 ~ 'Yes', kl482 == 5 ~ 'No')),
    life_event_started_nursery_age_4y_9m = as_factor(case_when(kl483 > 0 & kl483 < 5 ~ 'Yes', kl483 == 5 ~ 'No')),
    life_event_started_school_age_4y_9m = as_factor(case_when(kl484 > 0 & kl484 < 5 ~ 'Yes', kl484 == 5 ~ 'No')),
    life_event_control_had_other_upsetting_event_age_4y_9m = as_factor(case_when(kl485 > 0 & kl485 < 5 ~ 'Yes', kl485 == 5 ~ 'No')),
    life_event_child_taken_into_care_age_5y_9m = as_factor(case_when(kn4000 > 0 & kn4000 < 5 ~ 'Yes', kn4000 == 5 ~ 'No')),
    life_event_control_pet_died_age_5y_9m = as_factor(case_when(kn4001 > 0 & kn4001 < 5 ~ 'Yes', kn4001 == 5 ~ 'No')),
    life_event_moved_home_age_5y_9m = as_factor(case_when(kn4002 > 0 & kn4002 < 5 ~ 'Yes', kn4002 == 5 ~ 'No')),
    life_event_control_had_fright_age_5y_9m = as_factor(case_when(kn4003 > 0 & kn4003 < 5 ~ 'Yes', kn4003 == 5 ~ 'No')),
    life_event_control_physically_hurt_by_someone_age_5y_9m = as_factor(case_when(kn4004 > 0 & kn4004 < 5 ~ 'Yes', kn4004 == 5 ~ 'No')),
    life_event_control_sexually_abused_age_5y_9m = as_factor(case_when(kn4005 > 0 & kn4005 < 5 ~ 'Yes', kn4005 == 5 ~ 'No')),
    life_event_separated_from_mum_for_over_a_wk_age_5y_9m = as_factor(case_when(kn4006 > 0 & kn4006 < 5 ~ 'Yes', kn4006 == 5 ~ 'No')),
    life_event_separated_from_dad_for_over_a_wk_age_5y_9m = as_factor(case_when(kn4007 > 0 & kn4007 < 5 ~ 'Yes', kn4007 == 5 ~ 'No')),
    life_event_acquired_a_new_parent_age_5y_9m = as_factor(case_when(kn4008 > 0 & kn4008 < 5 ~ 'Yes', kn4008 == 5 ~ 'No')),
    life_event_had_a_new_sibling_age_5y_9m = as_factor(case_when(kn4009 > 0 & kn4009 < 5 ~ 'Yes', kn4009 == 5 ~ 'No')),
    life_event_control_admitted_to_hospital_age_5y_9m = as_factor(case_when(kn4010 > 0 & kn4010 < 5 ~ 'Yes', kn4010 == 5 ~ 'No')),
    life_event_changed_carer_age_5y_9m = as_factor(case_when(kn4011 > 0 & kn4011 < 5 ~ 'Yes', kn4011 == 5 ~ 'No')),
    life_event_separated_from_someone_else_age_5y_9m = as_factor(case_when(kn4012 > 0 & kn4012 < 5 ~ 'Yes', kn4012 == 5 ~ 'No')),
    life_event_started_nursery_age_5y_9m = as_factor(case_when(kn4013 > 0 & kn4013 < 5 ~ 'Yes', kn4013 == 5 ~ 'No')),
    life_event_started_school_age_5y_9m = as_factor(case_when(kn4014 > 0 & kn4014 < 5 ~ 'Yes', kn4014 == 5 ~ 'No')),
    life_event_control_had_other_upsetting_event_age_5y_9m = as_factor(case_when(kn4015 > 0 & kn4015 < 5 ~ 'Yes', kn4015 == 5 ~ 'No')),
    life_event_lost_best_friend_age_6y_9m = case_when(kq376a > 0 ~ kq376a),
    life_event_control_child_taken_into_care_age_8y_7m = as_factor(case_when(kt5000 > 0 & kt5000 < 5 ~ 'Yes', kt5000 == 5 ~ 'No')),
    life_event_control_pet_died_age_8y_7m = as_factor(case_when(kt5001 > 0 & kt5001 < 5 ~ 'Yes', kt5001 == 5 ~ 'No')),
    life_event_control_moved_home_age_8y_7m = as_factor(case_when(kt5002 > 0 & kt5002 < 5 ~ 'Yes', kt5002 == 5 ~ 'No')),
    life_event_control_had_fright_age_8y_7m = as_factor(case_when(kt5003 > 0 & kt5003 < 5 ~ 'Yes', kt5003 == 5 ~ 'No')),
    life_event_control_physically_hurt_by_someone_age_8y_7m = as_factor(case_when(kt5004 > 0 & kt5004 < 5 ~ 'Yes', kt5004 == 5 ~ 'No')),
    life_event_control_sexually_abused_age_8y_7m = as_factor(case_when(kt5005 > 0 & kt5005 < 5 ~ 'Yes', kt5005 == 5 ~ 'No')),
    life_event_control_death_in_family_age_8y_7m = as_factor(case_when(kt5006 > 0 & kt5006 < 5 ~ 'Yes', kt5006 == 5 ~ 'No')),
    life_event_control_separated_from_mum_for_over_a_wk_age_8y_7m = as_factor(case_when(kt5007 > 0 & kt5007 < 5 ~ 'Yes', kt5007 == 5 ~ 'No')),
    life_event_control_separated_from_dad_for_over_a_wk_age_8y_7m = as_factor(case_when(kt5008 > 0 & kt5008 < 5 ~ 'Yes', kt5008 == 5 ~ 'No')),
    life_event_control_acquired_a_new_parent_age_8y_7m = as_factor(case_when(kt5009 > 0 & kt5009 < 5 ~ 'Yes', kt5009 == 5 ~ 'No')),
    life_event_control_had_a_new_sibling_age_8y_7m = as_factor(case_when(kt5010 > 0 & kt5010 < 5 ~ 'Yes', kt5010 == 5 ~ 'No')),
    life_event_control_admitted_to_hospital_age_8y_7m = as_factor(case_when(kt5011 > 0 & kt5011 < 5 ~ 'Yes', kt5011 == 5 ~ 'No')),
    life_event_control_changed_carer_age_8y_7m = as_factor(case_when(kt5012 > 0 & kt5012 < 5 ~ 'Yes', kt5012 == 5 ~ 'No')),
    life_event_control_separated_from_someone_else_age_8y_7m = as_factor(case_when(kt5013 > 0 & kt5013 < 5 ~ 'Yes', kt5013 == 5 ~ 'No')),
    life_event_control_started_school_age_8y_7m = as_factor(case_when(kt5014 > 0 & kt5014 < 5 ~ 'Yes', kt5014 == 5 ~ 'No')),
    life_event_lost_best_friend_age_8y_7m = as_factor(case_when(kt5015 > 0 & kt5015 < 5 ~ 'Yes', kt5015 == 5 ~ 'No')),
    life_event_control_had_other_upsetting_event_age_8y_7m = as_factor(case_when(kt5016 > 0 & kt5016 < 5 ~ 'Yes', kt5016 == 5 ~ 'No')),
    life_event_moved_home_age_16y_6m = case_when(ccs2000 > 0 ~ ccs2000),
    life_event_had_a_new_sibling_age_16y_6m = case_when(ccs2010 > 0 ~ ccs2010),
    life_event_had_a_new_step_sibling_age_16y_6m = case_when(ccs2020 > 0 ~ ccs2020),
    life_event_started_school_age_16y_6m = case_when(ccs2030 > 0 ~ ccs2030),
    life_event_control_family_serious_illness_or_injury_age_16y_6m = case_when(ccs2040 > 0 ~ ccs2040),
    life_event_control_parental_divorce_age_16y_6m = case_when(ccs2050 > 0 ~ ccs2050),
    life_event_control_death_in_immediate_family_age_16y_6m = case_when(ccs2060 > 0 ~ ccs2060),
    life_event_grandparet_died_age_16y_6m = case_when(ccs2070 > 0 ~ ccs2070),
    life_event_close_friend_died_age_16y_6m = case_when(ccs2080 > 0 ~ ccs2080),
    life_event_sibling_left_home_age_16y_6m = case_when(ccs2090 > 0 ~ ccs2090),
    life_event_close_friend_illness_or_injury_age_16y_6m = case_when(ccs2100 > 0 ~ ccs2100),
    life_event_control_parent_in_trouble_with_police_age_16y_6m = case_when(ccs2110 > 0 ~ ccs2110),
    life_event_parents_partner_moved_in_age_16y_6m = case_when(ccs2120 > 0 ~ ccs2120),
    life_event_control_special_recognition_for_schoolwork_age_16y_6m = case_when(ccs2130 > 0 ~ ccs2130),
    life_event_control_serious_injury_or_illness_age_16y_6m = case_when(ccs2140 > 0 ~ ccs2140),
    life_event_control_done_badly_at_schoolwork_age_16y_6m = case_when(ccs2150 > 0 ~ ccs2150),
    life_event_control_special_recognition_for_other_activities_age_16y_6m = case_when(ccs2160 > 0 ~ ccs2160),
    life_event_friend_moved_away_age_16y_6m = case_when(ccs2170 > 0 ~ ccs2170),
    life_event_control_pet_died_age_16y_6m = case_when(ccs2180 > 0 ~ ccs2180),
    life_event_parent_lost_job_age_16y_6m = case_when(ccs2190 > 0 ~ ccs2190),
    life_event_control_YP_become_pregnant_age_16y_6m = case_when(ccs2210 > 0 ~ ccs2210),
    life_event_control_YP_become_a_parent_age_16y_6m = case_when(ccs2220 > 0 ~ ccs2220), 
    life_event_control_took_an_exam_age_22y = as_factor(case_when(YPB6000 > 0 & YPB6000 < 5 ~ 'Yes', YPB6000 == 5 ~ 'No')),
    life_event_left_home_age_22y = as_factor(case_when(YPB6010 > 0 & YPB6010 < 5 ~ 'Yes', YPB6010 == 5 ~ 'No')),
    life_event_became_pregnant_age_22y = as_factor(case_when(YPB6020 > 0 & YPB6020 < 5 ~ 'Yes', YPB6020 == 5 ~ 'No')),
    life_event_had_a_baby_age_22y = as_factor(case_when(YPB6030 > 0 & YPB6030 < 5 ~ 'Yes', YPB6030 == 5 ~ 'No')),
    life_event_control_lost_job_age_22y = as_factor(case_when(YPB6040 > 0 & YPB6040 < 5 ~ 'Yes', YPB6040 == 5 ~ 'No')),
    life_event_control_graduated_from_university_age_22y = as_factor(case_when(YPB6050 > 0 & YPB6050 < 5 ~ 'Yes', YPB6050 == 5 ~ 'No')),
    life_event_control_started_a_new_job_age_22y = as_factor(case_when(YPB6060 > 0 & YPB6060 < 5 ~ 'Yes', YPB6060 == 5 ~ 'No')),
    life_event_got_engaged_age_22y = as_factor(case_when(YPB6070 > 0 & YPB6070 < 5 ~ 'Yes', YPB6070 == 5 ~ 'No')),
    life_event_married_including_civil_partnership_age_22y = as_factor(case_when(YPB6080 > 0 & YPB6080 < 5 ~ 'Yes', YPB6080 == 5 ~ 'No')),
    life_event_were_divorced_age_22y = as_factor(case_when(YPB6090 > 0 & YPB6090 < 5 ~ 'Yes', YPB6090 == 5 ~ 'No')),
    life_event_control_admitted_to_hospital_age_22y = as_factor(case_when(YPB6100 > 0 & YPB6100 < 5 ~ 'Yes', YPB6100 == 5 ~ 'No')),
    life_event_control_trouble_with_law_age_22y = as_factor(case_when(YPB6110 > 0 & YPB6110 < 5 ~ 'Yes', YPB6110 == 5 ~ 'No')),
    life_event_control_had_problems_at_work_age_22y = as_factor(case_when(YPB6120 > 0 & YPB6120 < 5 ~ 'Yes', YPB6120 == 5 ~ 'No')),
    life_event_control_house_or_car_was_burgled_age_22y = as_factor(case_when(YPB6130 > 0 & YPB6130 < 5 ~ 'Yes', YPB6130 == 5 ~ 'No')),
    life_event_control_pet_died_age_22y = as_factor(case_when(YPB6140 > 0 & YPB6140 < 5 ~ 'Yes', YPB6140 == 5 ~ 'No')),
    life_event_parent_died_age_22y = as_factor(case_when(YPB6150 > 0 & YPB6150 < 5 ~ 'Yes', YPB6150 == 5 ~ 'No')),
    life_event_friend_died_age_22y = as_factor(case_when(YPB6160 > 0 & YPB6160 < 5 ~ 'Yes', YPB6160 == 5 ~ 'No')),
    life_event_relative_died_not_parent_age_22y = as_factor(case_when(YPB6170 > 0 & YPB6170 < 5 ~ 'Yes', YPB6170 == 5 ~ 'No')),
    life_event_control_became_homeless_age_22y = as_factor(case_when(YPB6180 > 0 & YPB6180 < 5 ~ 'Yes', YPB6180 == 5 ~ 'No')),
    life_event_control_major_financial_problems_age_22y = as_factor(case_when(YPB6190 > 0 & YPB6190 < 5 ~ 'Yes', YPB6190 == 5 ~ 'No')),
    life_event_control_had_abortion__age_22y = as_factor(case_when(YPB6210 > 0 & YPB6210 < 5 ~ 'Yes', YPB6210 == 5 ~ 'No')),
    life_event_parents_divorced_age_22y = as_factor(case_when(YPB6220 > 0 & YPB6220 < 5 ~ 'Yes', YPB6220 == 5 ~ 'No')),
    life_event_control_promoted_at_work_age_22y = as_factor(case_when(YPB6230 > 0 & YPB6230 < 5 ~ 'Yes', YPB6230 == 5 ~ 'No')),
    life_event_control_took_an_exam_age_23y = as_factor(case_when(YPC2150 > 0 & YPC2150 < 5 ~ 'Yes', YPC2150 == 0 ~ 'No')),
    life_event_left_home_age_23y = as_factor(case_when(YPC2160 > 0 & YPC2160 < 5 ~ 'Yes', YPC2160 == 0 ~ 'No')),
    life_event_became_pregnant_age_23y = as_factor(case_when(YPC2170 > 0 & YPC2170 < 5 ~ 'Yes', YPC2170 == 0 ~ 'No')),
    life_event_had_a_baby_age_23y = as_factor(case_when(YPC2180 > 0 & YPC2180 < 5 ~ 'Yes', YPC2180 == 0 ~ 'No')),
    life_event_control_lost_job_age_23y = as_factor(case_when(YPC2190 > 0 & YPC2190 < 5 ~ 'Yes', YPC2190 == 0 ~ 'No')),
    life_event_control_graduated_from_university_age_23y = as_factor(case_when(YPC2200 > 0 & YPC2200 < 5 ~ 'Yes', YPC2200 == 0 ~ 'No')),
    life_event_control_started_a_new_job_age_23y = as_factor(case_when(YPC2210 > 0 & YPC2210 < 5 ~ 'Yes', YPC2210 == 0 ~ 'No')),
    life_event_got_engaged_age_23y = as_factor(case_when(YPC2220 > 0 & YPC2220 < 5 ~ 'Yes', YPC2220 == 0 ~ 'No')),
    life_event_married_including_civil_partnership_age_23y = as_factor(case_when(YPC2230 > 0 & YPC2230 < 5 ~ 'Yes', YPC2230 == 0 ~ 'No')),
    life_event_were_divorced_age_23y = as_factor(case_when(YPC2240 > 0 & YPC2240 < 5 ~ 'Yes', YPC2240 == 0 ~ 'No')),
    life_event_control_admitted_to_hospital_age_23y = as_factor(case_when(YPC2250 > 0 & YPC2250 < 5 ~ 'Yes', YPC2250 == 0 ~ 'No')),
    life_event_control_trouble_with_law_age_23y = as_factor(case_when(YPC2260 > 0 & YPC2260 < 5 ~ 'Yes', YPC2260 == 0 ~ 'No')),
    life_event_control_problems_at_work_age_23y = as_factor(case_when(YPC2270 > 0 & YPC2270 < 5 ~ 'Yes', YPC2270 == 0 ~ 'No')),
    life_event_control_car_or_house_burgled_age_23y = as_factor(case_when(YPC2280 > 0 & YPC2280 < 5 ~ 'Yes', YPC2280 == 0 ~ 'No')),
    life_event_control_pet_died_age_23y = as_factor(case_when(YPC2290 > 0 & YPC2290 < 5 ~ 'Yes', YPC2290 == 0 ~ 'No')),
    life_event_parent_died_age_23y = as_factor(case_when(YPC2300 > 0 & YPC2300 < 5 ~ 'Yes', YPC2300 == 0 ~ 'No')),
    life_event_friend_died_age_23y = as_factor(case_when(YPC2310 > 0 & YPC2310 < 5 ~ 'Yes', YPC2310 == 0 ~ 'No')),
    life_event_child_died_age_23y = as_factor(case_when(YPC2320 > 0 & YPC2320 < 5 ~ 'Yes', YPC2320 == 0 ~ 'No')),
    life_event_control_miscarriage_age_23y = as_factor(case_when(YPC2330 > 0 & YPC2330 < 5 ~ 'Yes', YPC2330 == 0 ~ 'No')),
    life_event_relative_died_not_parent_age_23y = as_factor(case_when(YPC2340 > 0 & YPC2340 < 5 ~ 'Yes', YPC2340 == 0 ~ 'No')),
    life_event_control_became_homeless_age_23y = as_factor(case_when(YPC2350 > 0 & YPC2350 < 5 ~ 'Yes', YPC2350 == 0 ~ 'No')),
    life_event_control_major_financial_problems_age_23y = as_factor(case_when(YPC2360 > 0 & YPC2360 < 5 ~ 'Yes', YPC2360 == 0 ~ 'No')),
    life_event_control_had_abortion__age_23y = as_factor(case_when(YPC2380 > 0 & YPC2380 < 5 ~ 'Yes', YPC2380 == 0 ~ 'No')),
    life_event_parents_divorced_age_23y = as_factor(case_when(YPC2390 > 0 & YPC2390 < 5 ~ 'Yes', YPC2390 == 0 ~ 'No')),
    life_event_control_promoted_at_work_age_23y = as_factor(case_when(YPC2400 > 0 & YPC2400 < 5 ~ 'Yes', YPC2400 == 0 ~ 'No')),
    life_event_moved_home_age_23y = as_factor(case_when(YPC2410 > 0 & YPC2410 < 5 ~ 'Yes', YPC2410 == 0 ~ 'No')),
    life_event_control_took_an_exam_age_24y = as_factor(case_when(YPD1000 > 0 & YPD1000 < 5 ~ 'Yes', YPD1000 == 0 ~ 'No')),
    life_event_left_home_age_24y = as_factor(case_when(YPD1010 > 0 & YPD1010 < 5 ~ 'Yes', YPD1010 == 0 ~ 'No')),
    life_event_became_pregnant_age_24y = as_factor(case_when(YPD1020 > 0 & YPD1020 < 5 ~ 'Yes', YPD1020 == 0 ~ 'No')),
    life_event_had_a_baby_age_24y = as_factor(case_when(YPD1030 > 0 & YPD1030 < 5 ~ 'Yes', YPD1030 == 0 ~ 'No')),
    life_event_control_lost_job_age_24y = as_factor(case_when(YPD1040 > 0 & YPD1040 < 5 ~ 'Yes', YPD1040 == 0 ~ 'No')),
    life_event_control_graduated_from_university_age_24y = as_factor(case_when(YPD1050 > 0 & YPD1050 < 5 ~ 'Yes', YPD1050 == 0 ~ 'No')),
    life_event_control_started_a_new_job_age_24y = as_factor(case_when(YPD1060 > 0 & YPD1060 < 5 ~ 'Yes', YPD1060 == 0 ~ 'No')),
    life_event_got_engaged_age_24y = as_factor(case_when(YPD1070 > 0 & YPD1070 < 5 ~ 'Yes', YPD1070 == 0 ~ 'No')),
    life_event_married_including_civil_partnership_age_24y = as_factor(case_when(YPD1080 > 0 & YPD1080 < 5 ~ 'Yes', YPD1080 == 0 ~ 'No')),
    life_event_were_divorced_age_24y = as_factor(case_when(YPD1090 > 0 & YPD1090 < 5 ~ 'Yes', YPD1090 == 0 ~ 'No')),
    life_event_control_admitted_to_hospital_age_24y = as_factor(case_when(YPD1100 > 0 & YPD1100 < 5 ~ 'Yes', YPD1100 == 0 ~ 'No')),
    life_event_control_trouble_with_law_age_24y = as_factor(case_when(YPD1110 > 0 & YPD1110 < 5 ~ 'Yes', YPD1110 == 0 ~ 'No')),
    life_event_control_problems_at_work_age_24y = as_factor(case_when(YPD1120 > 0 & YPD1120 < 5 ~ 'Yes', YPD1120 == 0 ~ 'No')),
    life_event_control_car_or_house_burgled_age_24y = as_factor(case_when(YPD1130 > 0 & YPD1130 < 5 ~ 'Yes', YPD1130 == 0 ~ 'No')),
    life_event_control_pet_died_age_24y = as_factor(case_when(YPD1140 > 0 & YPD1140 < 5 ~ 'Yes', YPD1140 == 0 ~ 'No')),
    life_event_parent_died_age_24y = as_factor(case_when(YPD1150 > 0 & YPD1150 < 5 ~ 'Yes', YPD1150 == 0 ~ 'No')),
    life_event_friend_died_age_24y = as_factor(case_when(YPD1160 > 0 & YPD1160 < 5 ~ 'Yes', YPD1160 == 0 ~ 'No')),
    life_event_child_died_age_24y = as_factor(case_when(YPD1170 > 0 & YPD1170 < 5 ~ 'Yes', YPD1170 == 0 ~ 'No')),
    life_event_control_miscarriage_age_24y = as_factor(case_when(YPD1180 > 0 & YPD1180 < 5 ~ 'Yes', YPD1180 == 0 ~ 'No')),
    life_event_relative_died_not_parent_age_24y = as_factor(case_when(YPD1190 > 0 & YPD1190 < 5 ~ 'Yes', YPD1190 == 0 ~ 'No')),
    life_event_control_became_homeless_age_24y = as_factor(case_when(YPD1200 > 0 & YPD1200 < 5 ~ 'Yes', YPD1200 == 0 ~ 'No')),
    life_event_control_major_financial_problems_age_24y = as_factor(case_when(YPD1210 > 0 & YPD1210 < 5 ~ 'Yes', YPD1210 == 0 ~ 'No')),
    life_event_control_had_abortion__age_24y = as_factor(case_when(YPD1230 > 0 & YPD1230 < 5 ~ 'Yes', YPD1230 == 0 ~ 'No')),
    life_event_parents_divorced_age_24y = as_factor(case_when(YPD1240 > 0 & YPD1240 < 5 ~ 'Yes', YPD1240 == 0 ~ 'No')),
    life_event_control_promoted_at_work_age_24y = as_factor(case_when(YPD1250 > 0 & YPD1250 < 5 ~ 'Yes', YPD1250 == 0 ~ 'No')),
    life_event_moved_home_age_24y = as_factor(case_when(YPD1260 > 0 & YPD1260 < 5 ~ 'Yes', YPD1260 == 0 ~ 'No')))


# Check one DV from each wave of variables
life_events %>% count(life_event_child_taken_into_care_age_1y_6m, kd500b)
life_events %>% count(life_event_child_taken_into_care_age_2y_6m, kf450a)
life_events %>% count(life_event_moved_home_age_3y_6m, kj462a)
life_events %>% count(life_event_control_sexually_abused_age_4y_9m, kl475)
life_events %>% count(life_event_control_physically_hurt_by_someone_age_4y_9m, kl474)
life_events %>% count(life_event_child_taken_into_care_age_5y_9m, kn4000)
life_events %>% count(life_event_lost_best_friend_age_6y_9m, kq376a)
life_events %>% count(life_event_control_child_taken_into_care_age_8y_7m, kt5000)
life_events %>% count(life_event_moved_home_age_16y_6m, ccs2000)
life_events %>% count(life_event_control_took_an_exam_age_22y, YPB6000)
life_events %>% count(life_event_control_took_an_exam_age_23y, YPC2150)
life_events %>% count(life_event_control_took_an_exam_age_24y, YPD1000)

# Review added variable names
data.frame(var.names=names(life_events)) %>% filter(grepl('_age', var.names))

###############################################################################
# SDQ - peer problems score
# Set up as its own domain. 

# Review variables
attributes(alspac$kq348e)
alspac %>% count(kq348e) %>% print(n = 40)

attributes(alspac$ku709b)
alspac %>% count(ku709b) %>% print(n = 40)

attributes(alspac$kw6604b)
alspac %>% count(kw6604b) %>% print(n = 40)

attributes(alspac$ta7025d)
alspac %>% count(ta7025d) %>% print(n = 40)

attributes(alspac$tc4025d)
alspac %>% count(tc4025d) %>% print(n = 40)

attributes(alspac$j555e)
alspac %>% count(j555e) %>% print(n = 40) # Inconsistent - exclude.
#Could derive from individual items

attributes(alspac$n8365d)
alspac %>% count(n8365d) %>% print(n = 40)

### Select variables
peer_problems <- alspac %>%
  select(uniqid, kq348e, ku709b, kw6604b, ta7025d, tc4025d, n8365d)

### Remove missing cases
peer_problems <- peer_problems %>%
  mutate(
    peer_problem_score_age_6y9m = case_when(kq348e >= 0 ~ kq348e),
    peer_problem_score_age_8y1m = case_when(n8365d >= 0 ~ n8365d),
    peer_problem_score_age_9y7m = case_when(ku709b >= 0 ~ ku709b),
    peer_problem_score_age_11y8m = case_when(kw6604b >= 0 ~ kw6604b),
    peer_problem_score_age_13y1m = case_when(ta7025d >= 0 ~ ta7025d),
    peer_problem_score_age_16y6m = case_when(tc4025d >= 0 ~ tc4025d))

# Check new variables
peer_problems %>% count(peer_problem_score_age_6y9m, kq348e)
peer_problems %>% count(peer_problem_score_age_8y1m, n8365d)
peer_problems %>% count(peer_problem_score_age_9y7m, ku709b)
peer_problems %>% count(peer_problem_score_age_11y8m, kw6604b)
peer_problems %>% count(peer_problem_score_age_13y1m , ta7025d)
peer_problems %>% count(peer_problem_score_age_16y6m, tc4025d)

###############################################################################
# SDQ - pro social scpre
# Imported as its own domain

# Pro-rated version missing at wave J. 
prosocial <- c("kq348a","n8365e", "kw6600b", "ku705b", "ta7025e", "tc4025e")
pro_social <- alspac %>% 
  select(uniqid, kq348a, n8365e, kw6600b, ku705b, ta7025e, tc4025e)
for (var in names(pro_social %>% select(prosocial))) {
  pro_social %>% count(.data[[var]]) %>% print()
}

# Remove missing cases
pro_social <- pro_social %>%
  mutate(
    pro_social_score_age_6y9m = case_when(kq348a >= 0 ~ kq348a),
    pro_social_score_age_8y1m = case_when(n8365e >= 0 ~ n8365e),
    pro_social_score_age_9y7m = case_when(ku705b >= 0 ~ ku705b),
    pro_social_score_age_11y8m = case_when(kw6600b >= 0 ~ kw6600b),
    pro_social_score_age_13y1m = case_when(ta7025e >= 0 ~ ta7025e),
    pro_social_score_age_16y6m = case_when(tc4025e >= 0 ~ tc4025e))

# Compare old with new variables
pro_social %>% count(pro_social_score_age_6y9m, kq348a)
pro_social %>% count(pro_social_score_age_8y1m, n8365e)
pro_social %>% count(pro_social_score_age_9y7m, ku705b)
pro_social %>% count(pro_social_score_age_11y8m, kw6600b)
pro_social %>% count(pro_social_score_age_13y1m, ta7025e)
pro_social %>% count(pro_social_score_age_16y6m, tc4025e)

################################################################################
### Outcomes ###

# Our two main outcomes are the DAWBA and SDQ scores

# Review variables

#DAWBA Depression age 7
attributes(alspac$kr882)
alspac %>% count(kr882)

#DAWBA Anxiety age 7
attributes(alspac$kr881)
alspac %>% count(kr881)

#DAWBA Depression age 10
attributes(alspac$kv8618)
alspac %>% count(kv8618)

#DAWBA Anxiety age 10
attributes(alspac$kv8617)
alspac %>% count(kv8617)

#DAWBA Depression age 14
attributes(alspac$tb8619)
alspac %>% count(tb8619)

#DAWBA Anxiety age 14
attributes(alspac$tb8618)
alspac %>% count(tb8618)

#DAWBA Depression age 15
attributes(alspac$fh6892)
alspac %>% count(fh6892)

#DAWBA Anxiety age 15
attributes(alspac$fh6893)
alspac %>% count(fh6893)

#CIS-R Depression age 17
attributes(alspac$FJCI1001)
alspac %>% count(FJCI1001)

#CIS-R Anxiety age 17
attributes(alspac$FJCI501)
alspac %>% count(FJCI501)

#CIS-R Depression age 24 #check if depression disorder present
attributes(alspac$FKDQ1000)
alspac %>% count(FKDQ1000)

attributes(alspac$FKDQ1010)
alspac %>% count(FKDQ1010)

attributes(alspac$FKDQ1020)
alspac %>% count(FKDQ1020)
#DV - any episode

#CIS-R Anxiety age 24 #check whether anxiety disorder present
attributes(alspac$FKDQ1030)
alspac %>% count(FKDQ1030)

#DAWBA-CISR missing within wave (inc. NA)

#depression
alspac %>%
  summarise(
    age7 = mean(kr882 < 0 | is.na(kr882))*100,
    age10 = mean(kv8618 < 0 | is.na(kv8618))*100,
    age13 = mean(tb8619 < 0 | is.na(tb8619))*100,
    age15 = mean(fh6892 < 0 | is.na(fh6892))*100,
    age17 = mean(FJCI1001 < 0 | is.na(FJCI1001))*100
  )

#anxiety
alspac %>%
  summarise(
    age7 = mean(kr881 < 0| is.na(kr881))*100,
    age10 = mean(kv8617 < 0 | is.na(kv8617))*100,
    age13 = mean(tb8618 < 0 | is.na(tb8618))*100,
    age15 = mean(fh6893 < 0 | is.na(fh6893))*100,
    age17 = mean(FJCI501 < 0 | is.na(FJCI501))*100
  )

################################################################################
### Create a depression dataset
depression <- alspac %>% select(uniqid, kr882, kv8618, tb8619, fh6892, FJCI1001, FKDQ1000, FKDQ1010, FKDQ1020)

# Remove missing values from depression variables
depression <- depression %>%
  mutate(
    depression_age_7y9m = case_when(
      kr882 >= 0 ~ kr882),
    depression_age_10y8m = case_when(
      kv8618 >= 0 ~ kv8618),
    depression_age_13y10m = case_when(
      tb8619 >= 0 ~ tb8619),
    depression_age_15y6m = case_when(
      fh6892 == 1 ~ 1,
      fh6892 == 2 ~ 0),
    depression_age_17y6m = case_when(
      FJCI1001 >= 0 ~ FJCI1001),
    depression_age_24y = case_when(
      FKDQ1000 == 1 | FKDQ1010 == 1 | FKDQ1020 == 1 ~ 1,
      FKDQ1000 == 0 & FKDQ1010 == 0 & FKDQ1020 == 0 ~ 0)
  )

depression %>% count(depression_age_7y9m, kr882)
depression %>% count(depression_age_10y8m, kv8618)
depression %>% count(depression_age_13y10m, tb8619)
depression %>% count(depression_age_15y6m, fh6892)
depression %>% count(depression_age_17y6m, FJCI1001)
depression %>% count(depression_age_24y, FKDQ1000, FKDQ1010, FKDQ1020)

################################################################################
### Create an anxiety dataset
anxiety <- alspac %>% select(uniqid, kr881, kv8617, tb8618, fh6893, FJCI501, FKDQ1030, FKDQ1050, FKDQ1060, FKDQ1070)

# Remove missing values from anxiety variables
anxiety <- anxiety %>%
  mutate(
    anxiety_age_7y9m = case_when(
      kr881 >= 0 ~ kr881),
    anxiety_age_10y8m = case_when(
      kv8617 >= 0 ~ kv8617),
    anxiety_age_13y10m = case_when(
      tb8618 >= 0 ~ tb8618),
    anxiety_age_15y6m = case_when(
      fh6893 == 1 ~ 1,
      fh6893 == 2 ~ 0),
    anxiety_age_17y6m = case_when(
      FJCI501 >= 2 ~ 1,
      FJCI501 >= 0 ~ 0),
    anxiety_age_24y = case_when(
      FKDQ1030 == 1 | FKDQ1050 == 1 | FKDQ1060 == 1 | FKDQ1070 == 1 ~ 1,
      FKDQ1030 == 0 & FKDQ1050 == 0 & FKDQ1060 == 0 & FKDQ1070 == 0 ~ 0)
  )
# Note: our anxiety variables is a composite of several sub-domains of anxiety. 
# Yes if any of these are yes, no if all are no. 

# Check anxiety variables. 
anxiety %>% count(anxiety_age_7y9m, kr881)
anxiety %>% count(anxiety_age_10y8m, kv8617)
anxiety %>% count(anxiety_age_13y10m, tb8618)
anxiety %>% count(anxiety_age_15y6m, fh6893)
anxiety %>% count(anxiety_age_17y6m, FJCI501)
anxiety %>% count(anxiety_age_24y, FKDQ1030, FKDQ1050, FKDQ1060, FKDQ1070)

################################################################################
#### SMFQ

# Note: in some waves SMFQ was unavailable. Derived where not available. 

#Age 10
attributes(alspac$fddp130)
alspac %>% count(fddp130) %>% print(n = 40)

#Age 12
#Score not available
attributes(alspac$ff6500)			
alspac %>% count(ff6500)

attributes(alspac$ff6502)			
alspac %>% count(ff6502)

attributes(alspac$ff6503)			
alspac %>% count(ff6503)

attributes(alspac$ff6504)			
alspac %>% count(ff6504)

attributes(alspac$ff6505)			
alspac %>% count(ff6505)

attributes(alspac$ff6506)			
alspac %>% count(ff6506)

attributes(alspac$ff6508)			
alspac %>% count(ff6508)

attributes(alspac$ff6509)			
alspac %>% count(ff6509)

attributes(alspac$ff6511)			
alspac %>% count(ff6511)

attributes(alspac$ff6512)			
alspac %>% count(ff6512)

attributes(alspac$ff6513)			
alspac %>% count(ff6513)

attributes(alspac$ff6514)			
alspac %>% count(ff6514)

attributes(alspac$ff6515)			
alspac %>% count(ff6515)

#Age 13
attributes(alspac$fg7226)
alspac %>% count(fg7226) %>% print(n = 40)

#Age 16
#Score not available
attributes(alspac$ccs4500)	
alspac %>% count(ccs4500)

attributes(alspac$ccs4502)
alspac %>% count(ccs4502)

attributes(alspac$ccs4503)	
alspac %>% count(ccs4503)

attributes(alspac$ccs4504)	
alspac %>% count(ccs4504)

attributes(alspac$ccs4505)
alspac %>% count(ccs4505)

attributes(alspac$ccs4506)	
alspac %>% count(ccs4506)

attributes(alspac$ccs4508)	
alspac %>% count(ccs4508)

attributes(alspac$ccs4509)
alspac %>% count(ccs4509)

attributes(alspac$ccs4511)
alspac %>% count(ccs4511)

attributes(alspac$ccs4512)	
alspac %>% count(ccs4512)

attributes(alspac$ccs4513)
alspac %>% count(ccs4513)

attributes(alspac$ccs4514)
alspac %>% count(ccs4514)

attributes(alspac$ccs4515)	
alspac %>% count(ccs4515)


#Age 17
attributes(alspac$CCXD917)
alspac %>% count(CCXD917) %>% print(n = 40)

#Age 18
attributes(alspac$cct2715)
alspac %>% count(cct2715) %>% print(n = 40)

#Age 21
#Score not available
attributes(alspac$YPA2000)	
alspac %>% count(YPA2000)

attributes(alspac$YPA2010)
alspac %>% count(YPA2010)

attributes(alspac$YPA2020)
alspac %>% count(YPA2020)

attributes(alspac$YPA2030)	
alspac %>% count(YPA2030)

attributes(alspac$YPA2040)
alspac %>% count(YPA2040)

attributes(alspac$YPA2050)
alspac %>% count(YPA2050)

attributes(alspac$YPA2060)
alspac %>% count(YPA2060)

attributes(alspac$YPA2070)	
alspac %>% count(YPA2070)

attributes(alspac$YPA2080)	
alspac %>% count(YPA2080)

attributes(alspac$YPA2090)	
alspac %>% count(YPA2090)

attributes(alspac$YPA2100)
alspac %>% count(YPA2100)

attributes(alspac$YPA2110)	
alspac %>% count(YPA2110)

attributes(alspac$YPA2120)
alspac %>% count(YPA2120)


#Age 22
attributes(alspac$YPB5180)
alspac %>% count(YPB5180) %>% print(n = 40)

#Age 23
#Score not available
attributes(alspac$YPC1650)	
alspac %>% count(YPC1650)

attributes(alspac$YPC1651)	
alspac %>% count(YPC1651)

attributes(alspac$YPC1653)	
alspac %>% count(YPC1653)

attributes(alspac$YPC1654)	
alspac %>% count(YPC1654)

attributes(alspac$YPC1655)	
alspac %>% count(YPC1655)

attributes(alspac$YPC1656)	
alspac %>% count(YPC1656)

attributes(alspac$YPC1658)	
alspac %>% count(YPC1658)

attributes(alspac$YPC1659)	
alspac %>% count(YPC1659)

attributes(alspac$YPC1660)	
alspac %>% count(YPC1660)

attributes(alspac$YPC1662)	
alspac %>% count(YPC1662)

attributes(alspac$YPC1663)	
alspac %>% count(YPC1663)

attributes(alspac$YPC1665)	
alspac %>% count(YPC1665)

attributes(alspac$YPC1667)	
alspac %>% count(YPC1667)

################################################################################
# Select SMFQ variables, create a separate dataframe

depression_smfq <- alspac %>% select(uniqid, fddp130, fg7226, CCXD917, cct2715, YPB5180, 
                                     ff6500, 	ff6501, 	ff6502, 	ff6503, 	ff6504, 	ff6505, 	ff6506, 	ff6507, 	ff6508, 	ff6509, 	ff6510, 	ff6511, 	ff6512, 	ff6513, 	ff6514, 	ff6515,
                                     ccs4500, 	ccs4501, 	ccs4502, 	ccs4503, 	ccs4504, 	ccs4505, 	ccs4506, 	ccs4507, 	ccs4508, 	ccs4509, 	ccs4510, 	ccs4511, 	ccs4512, 	ccs4513, 	ccs4514, 	ccs4515, 	ccs4516,
                                     YPA2000, 	YPA2010, 	YPA2020, 	YPA2030, 	YPA2040, 	YPA2050, 	YPA2060, 	YPA2070, 	YPA2080, 	YPA2090, 	YPA2100, 	YPA2110, 	YPA2120,
                                     YPC1650, 	YPC1651, 	YPC1653, 	YPC1654, 	YPC1655, 	YPC1656, 	YPC1658, 	YPC1659, 	YPC1660, 	YPC1662, 	YPC1663, 	YPC1665, 	YPC1667)

# Derive SMFQ score at age 12 years 6 months 

# Remove missing cases, reverse order of variable coding
depression_smfq  <- depression_smfq %>%   mutate(ff6500b= case_when(ff6500 > 0 ~ 3 - ff6500))
depression_smfq  <- depression_smfq %>%   mutate(ff6502b= case_when(ff6502 > 0 ~ 3 - ff6502))
depression_smfq  <- depression_smfq %>%   mutate(ff6503b= case_when(ff6503 > 0 ~ 3 - ff6503))
depression_smfq  <- depression_smfq %>%   mutate(ff6504b= case_when(ff6504 > 0 ~ 3 - ff6504))
depression_smfq  <- depression_smfq %>%   mutate(ff6505b= case_when(ff6505 > 0 ~ 3 - ff6505))
depression_smfq  <- depression_smfq %>%   mutate(ff6506b= case_when(ff6506 > 0 ~ 3 - ff6506))
depression_smfq  <- depression_smfq %>%   mutate(ff6508b= case_when(ff6508 > 0 ~ 3 - ff6508))
depression_smfq  <- depression_smfq %>%   mutate(ff6509b= case_when(ff6509 > 0 ~ 3 - ff6509))
depression_smfq  <- depression_smfq %>%   mutate(ff6511b= case_when(ff6511 > 0 ~ 3 - ff6511))
depression_smfq  <- depression_smfq %>%   mutate(ff6512b= case_when(ff6512 > 0 ~ 3 - ff6512))
depression_smfq  <- depression_smfq %>%   mutate(ff6513b= case_when(ff6513 > 0 ~ 3 - ff6513))
depression_smfq  <- depression_smfq %>%   mutate(ff6514b= case_when(ff6514 > 0 ~ 3 - ff6514))
depression_smfq  <- depression_smfq %>%   mutate(ff6515b= case_when(ff6515 > 0 ~ 3 - ff6515))

# Check reverse coded variable
depression_smfq %>% count(ff6500b, ff6500)

# Derive SMFQ score
depression_smfq <- depression_smfq %>% mutate(depression_smfq_score_age_12y6m=ff6500b + 	ff6502b + 	ff6503b + 	ff6504b + 	ff6505b + 	ff6506b + 	ff6508b + 	ff6509b + 	ff6511b + 	ff6512b + 	ff6513b + 	ff6514b + 	ff6515b)

# Check SMFQ score
depression_smfq %>% select(depression_smfq_score_age_12y6m, ff6500b, ff6502b, ff6503b, ff6504b, ff6505b, ff6506b,	ff6508b, ff6509b, ff6511b, ff6512b, ff6513b, ff6514b, ff6515b) %>% head()

# Review SMFQ score
depression_smfq %>% summarize(
      mean = mean(depression_smfq_score_age_12y6m, na.rm=TRUE),
      max = max(depression_smfq_score_age_12y6m, na.rm=TRUE),
      min = min(depression_smfq_score_age_12y6m, na.rm=TRUE),
      sd = sd(depression_smfq_score_age_12y6m, na.rm=TRUE))

# Derive SMFQ score at age 16 years 6 months

# Remove missing cases, reverse order of variable coding
depression_smfq  <- depression_smfq %>%   mutate(ccs4500b= case_when(ccs4500 > 0 ~ 3 - ccs4500))
depression_smfq  <- depression_smfq %>%   mutate(ccs4502b= case_when(ccs4502 > 0 ~ 3 - ccs4502))
depression_smfq  <- depression_smfq %>%   mutate(ccs4503b= case_when(ccs4503 > 0 ~ 3 - ccs4503))
depression_smfq  <- depression_smfq %>%   mutate(ccs4504b= case_when(ccs4504 > 0 ~ 3 - ccs4504))
depression_smfq  <- depression_smfq %>%   mutate(ccs4505b= case_when(ccs4505 > 0 ~ 3 - ccs4505))
depression_smfq  <- depression_smfq %>%   mutate(ccs4506b= case_when(ccs4506 > 0 ~ 3 - ccs4506))
depression_smfq  <- depression_smfq %>%   mutate(ccs4508b= case_when(ccs4508 > 0 ~ 3 - ccs4508))
depression_smfq  <- depression_smfq %>%   mutate(ccs4509b= case_when(ccs4509 > 0 ~ 3 - ccs4509))
depression_smfq  <- depression_smfq %>%   mutate(ccs4511b= case_when(ccs4511 > 0 ~ 3 - ccs4511))
depression_smfq  <- depression_smfq %>%   mutate(ccs4512b= case_when(ccs4512 > 0 ~ 3 - ccs4512))
depression_smfq  <- depression_smfq %>%   mutate(ccs4513b= case_when(ccs4513 > 0 ~ 3 - ccs4513))
depression_smfq  <- depression_smfq %>%   mutate(ccs4514b= case_when(ccs4514 > 0 ~ 3 - ccs4514))
depression_smfq  <- depression_smfq %>%   mutate(ccs4515b= case_when(ccs4515 > 0 ~ 3 - ccs4515))

# Check reverse coded variable
depression_smfq %>% count(ccs4500b, ccs4500)

# Derive SMFQ score
depression_smfq <- depression_smfq %>% mutate(depression_smfq_score_age_16y6m = ccs4500b + 	ccs4502b + 	ccs4503b + 	ccs4504b + 	ccs4505b + 	ccs4506b + 	ccs4508b + 	ccs4509b + 	ccs4511b + 	ccs4512b + 	ccs4513b + 	ccs4514b + 	ccs4515b)

# Check SMFQ score
depression_smfq %>% select(depression_smfq_score_age_16y6m, ccs4500b, ccs4502b, 	ccs4503b, ccs4504b, 	ccs4505b, 	ccs4506b, 	ccs4508b, 	ccs4509b, ccs4511b, 	ccs4512b, 	ccs4513b,	ccs4514b, 	ccs4515b) %>% head()

# Review SMFQ score
depression_smfq %>% summarize(
  mean = mean(depression_smfq_score_age_16y6m, na.rm=TRUE),
  max = max(depression_smfq_score_age_16y6m, na.rm=TRUE),
  min = min(depression_smfq_score_age_16y6m, na.rm=TRUE),
  sd = sd(depression_smfq_score_age_16y6m, na.rm=TRUE))

# Derive SMFQ score at Age 21 years 

# Remove missing cases, reverse order of variable coding
depression_smfq  <- depression_smfq %>%   mutate(YPA2000b= case_when(YPA2000 > 0 ~ 3 - YPA2000))
depression_smfq  <- depression_smfq %>%   mutate(YPA2010b= case_when(YPA2010 > 0 ~ 3 - YPA2010))
depression_smfq  <- depression_smfq %>%   mutate(YPA2020b= case_when(YPA2020 > 0 ~ 3 - YPA2020))
depression_smfq  <- depression_smfq %>%   mutate(YPA2030b= case_when(YPA2030 > 0 ~ 3 - YPA2030))
depression_smfq  <- depression_smfq %>%   mutate(YPA2040b= case_when(YPA2040 > 0 ~ 3 - YPA2040))
depression_smfq  <- depression_smfq %>%   mutate(YPA2050b= case_when(YPA2050 > 0 ~ 3 - YPA2050))
depression_smfq  <- depression_smfq %>%   mutate(YPA2060b= case_when(YPA2060 > 0 ~ 3 - YPA2060))
depression_smfq  <- depression_smfq %>%   mutate(YPA2070b= case_when(YPA2070 > 0 ~ 3 - YPA2070))
depression_smfq  <- depression_smfq %>%   mutate(YPA2080b= case_when(YPA2080 > 0 ~ 3 - YPA2080))
depression_smfq  <- depression_smfq %>%   mutate(YPA2090b= case_when(YPA2090 > 0 ~ 3 - YPA2090))
depression_smfq  <- depression_smfq %>%   mutate(YPA2100b= case_when(YPA2100 > 0 ~ 3 - YPA2100))
depression_smfq  <- depression_smfq %>%   mutate(YPA2110b= case_when(YPA2110 > 0 ~ 3 - YPA2110))
depression_smfq  <- depression_smfq %>%   mutate(YPA2120b= case_when(YPA2120 > 0 ~ 3 - YPA2120))

# Check reverse coded variable
depression_smfq %>% count(YPA2000b, YPA2000)

# Derive SMFQ score
depression_smfq <- depression_smfq %>% mutate(depression_smfq_score_age_21y = YPA2000b + 	YPA2010b + 	YPA2020b + 	YPA2030b + 	YPA2040b + 	YPA2050b + 	YPA2060b + 	YPA2070b + 	YPA2080b + 	YPA2090b + 	YPA2100b + 	YPA2110b + 	YPA2120b)

# Check SMFQ score
depression_smfq %>% select(depression_smfq_score_age_21y, YPA2000b, 	YPA2010b, 	YPA2020b, 	YPA2030b, 	YPA2040b, 	YPA2050b, 	YPA2060b, 	YPA2070b, 	YPA2080b, 	YPA2090b, 	YPA2100b, 	YPA2110b, 	YPA2120b) %>% head()
depression_smfq %>% filter(!is.na(depression_smfq_score_age_21y)) %>% select(depression_smfq_score_age_21y, YPA2000b, 	YPA2010b, 	YPA2020b, 	YPA2030b, 	YPA2040b, 	YPA2050b, 	YPA2060b, 	YPA2070b, 	YPA2080b, 	YPA2090b, 	YPA2100b, 	YPA2110b, 	YPA2120b) %>% head()

# Review SMFQ score
depression_smfq %>% summarize(
  mean = mean(depression_smfq_score_age_21y, na.rm=TRUE),
  max = max(depression_smfq_score_age_21y, na.rm=TRUE),
  min = min(depression_smfq_score_age_21y, na.rm=TRUE),
  sd = sd(depression_smfq_score_age_21y, na.rm=TRUE))

#Age 23 years

#Amended variable coding system - coding of base variables has changed. 
depression_smfq  <- depression_smfq %>%   mutate(YPC1650b= case_when(YPC1650 > 0 ~ -1 + YPC1650))
depression_smfq  <- depression_smfq %>%   mutate(YPC1651b= case_when(YPC1651 > 0 ~ -1 + YPC1651))
depression_smfq  <- depression_smfq %>%   mutate(YPC1653b= case_when(YPC1653 > 0 ~ -1 + YPC1653))
depression_smfq  <- depression_smfq %>%   mutate(YPC1654b= case_when(YPC1654 > 0 ~ -1 + YPC1654))
depression_smfq  <- depression_smfq %>%   mutate(YPC1655b= case_when(YPC1655 > 0 ~ -1 + YPC1655))
depression_smfq  <- depression_smfq %>%   mutate(YPC1656b= case_when(YPC1656 > 0 ~ -1 + YPC1656))
depression_smfq  <- depression_smfq %>%   mutate(YPC1658b= case_when(YPC1658 > 0 ~ -1 + YPC1658))
depression_smfq  <- depression_smfq %>%   mutate(YPC1659b= case_when(YPC1659 > 0 ~ -1 + YPC1659))
depression_smfq  <- depression_smfq %>%   mutate(YPC1660b= case_when(YPC1660 > 0 ~ -1 + YPC1660))
depression_smfq  <- depression_smfq %>%   mutate(YPC1662b= case_when(YPC1662 > 0 ~ -1 + YPC1662))
depression_smfq  <- depression_smfq %>%   mutate(YPC1663b= case_when(YPC1663 > 0 ~ -1 + YPC1663))
depression_smfq  <- depression_smfq %>%   mutate(YPC1665b= case_when(YPC1665 > 0 ~ -1 + YPC1665))
depression_smfq  <- depression_smfq %>%   mutate(YPC1667b= case_when(YPC1667 > 0 ~ -1 + YPC1667))

# Check reverse coded variable
depression_smfq %>% count(YPC1650b, YPC1650)

# Derive SMFQ score
depression_smfq <- depression_smfq %>% mutate(depression_smfq_score_age_23y = YPC1650b + 	YPC1651b + 	YPC1653b + 	YPC1654b + 	YPC1655b + 	YPC1656b + 	YPC1658b + 	YPC1659b + 	YPC1660b + 	YPC1662b + 	YPC1663b + 	YPC1665b + 	YPC1667b)

# check SMFQ score
depression_smfq %>% select(depression_smfq_score_age_23y, YPC1650b, 	YPC1651b, 	YPC1653b, 	YPC1654b, 	YPC1655b, 	YPC1656b, 	YPC1658b, 	YPC1659b, 	YPC1660b, 	YPC1662b, 	YPC1663b, 	YPC1665b, 	YPC1667b)

# Review SMFQ score
depression_smfq %>% summarize(
  mean = mean(depression_smfq_score_age_23y, na.rm=TRUE),
  max = max(depression_smfq_score_age_23y, na.rm=TRUE),
  min = min(depression_smfq_score_age_23y, na.rm=TRUE),
  sd = sd(depression_smfq_score_age_23y, na.rm=TRUE))

### Derive binary variable for presence of depression from SMFQ scores
depression_smfq <- depression_smfq %>%
  mutate(
    depression_smfq_age_10y = case_when(fddp130 < 11 & fddp130 >= 0 ~ 0, fddp130 >= 11 ~ 1),
    depression_smfq_age_12y6m = case_when(depression_smfq_score_age_12y6m < 11 & depression_smfq_score_age_12y6m >= 0 ~ 0, depression_smfq_score_age_12y6m >= 11 ~ 1),
    depression_smfq_age_13y6m = case_when(fg7226 < 11 & fg7226 >= 0 ~ 0, fg7226 >= 11 ~ 1),
    depression_smfq_age_16y6m = case_when(depression_smfq_score_age_16y6m < 11 & depression_smfq_score_age_16y6m >= 0 ~ 0, depression_smfq_score_age_16y6m >= 11 ~ 1),
    depression_smfq_age_17y6m = case_when(CCXD917 < 11 & CCXD917 >= 0 ~ 0, CCXD917 >= 11 ~ 1),
    depression_smfq_age_18y7m = case_when(cct2715 < 11 & cct2715 >= 0 ~ 0, cct2715 >= 11 ~ 1),
    depression_smfq_age_21y = case_when(depression_smfq_score_age_21y < 11 & depression_smfq_score_age_21y >= 0 ~ 0, depression_smfq_score_age_21y >= 11 ~ 1),
    depression_smfq_age_22y = case_when(YPB5180 < 11 & YPB5180 >= 0 ~ 0, YPB5180 >= 11 ~ 1), 
    depression_smfq_age_23y = case_when(depression_smfq_score_age_23y < 11 & depression_smfq_score_age_23y >= 0 ~ 0, depression_smfq_score_age_23y >= 11 ~ 1))

# Check binary variables
depression_smfq %>% count(depression_smfq_age_10y, fddp130) %>% print(n=40)
depression_smfq %>% count(depression_smfq_age_12y6m, depression_smfq_score_age_12y6m) %>% print(n=40)
depression_smfq %>% count(depression_smfq_age_13y6m, fg7226) %>% print(n=40)
depression_smfq %>% count(depression_smfq_age_16y6m, depression_smfq_score_age_16y6m) %>% print(n=40)
depression_smfq %>% count(depression_smfq_age_17y6m, CCXD917) %>% print(n=40)
depression_smfq %>% count(depression_smfq_age_18y7m, cct2715) %>% print(n=40)
depression_smfq %>% count(depression_smfq_age_21y, depression_smfq_score_age_21y) %>% print(n=40)
depression_smfq %>% count(depression_smfq_age_22y, YPB5180) %>% print(n=40)
depression_smfq %>% count(depression_smfq_age_23y, depression_smfq_score_age_23y) %>% print(n=40)

################################################################################
# Time point comparison of available data on different outcomes:

tibble(
  Age = c(         7,        10,         12,          13,         15,           16,         17,       18,       21,        22,      23,       24),
  DAWBA_CISR = c("Yes",    "Yes",      "No",       "Yes",       "Yes",        "No",      "Yes",      "No",    "No",        "No",   "No",    "Yes"),
  SMFQ = c(       "No",    "Yes",  "Derive",       "Yes",       "No",     "Derive",     "Yes",     "Yes",    "Derive",   "Yes",   "Derive", "No")
) %>%
  mutate(both_available = case_when(
    DAWBA_CISR == "Yes" & SMFQ %in% c("Yes", "Derive") ~ "Yes",
    TRUE ~ ""
  ))

################################################################################
### Bullying domain ###

alspac %>% count(f8fp470) #Bullying, Child is overt victim: F8
alspac %>% count(f8fp475) #Bullying, Child is relational victim: F8
attributes(alspac$f8fp470)
attributes(alspac$f8fp475)

alspac %>% count(fdfp470) #Bullying, Child is overt victim: F10
alspac %>% count(fdfp475) #Bullying, Child is relational victim: F10
attributes(alspac$fdfp470)
attributes(alspac$fdfp475)

alspac %>% count(ccc290)
attributes(alspac$ccc290)

alspac %>% count(ta7018)
attributes(alspac$ta7018)

alspac %>% count(ccl201) #Child teased by other children, 12y1m
attributes(alspac$ccl201)

alspac %>% count(ccl216) #Bullying by siblings, 12y1m
attributes(alspac$ccl216)

alspac %>% count(kl801) #Child teased by children, 4y8m
attributes(alspac$kl801)

alspac %>% count(kl791) #Teased by siblings, 4y8m
attributes(alspac$kl791)

bullying <- alspac %>%
  select(uniqid, f8fp470, f8fp475, fdfp470, fdfp475, ccc290, ta7018, ccl201, ccl216, kl801, kl791)

glimpse(bullying)

bullying <- bullying %>%
  mutate(
    bullying_victim_age_8y = case_when(
      f8fp470 == 1 | f8fp475 == 1 ~ 1,
      f8fp470 == 2 & f8fp475 == 2 ~ 0),
    bullying_victim_age_10y = case_when(
      fdfp470 == 1 | fdfp475 == 1 ~ 1,
      fdfp470 == 2 & fdfp475 == 2 ~ 0),
    bullying_often_bullied_age_13y1m = case_when(
      ta7018 > 0 ~ ta7018),
    bullying_frequency_bullied_age_8y1m = case_when(
      ccc290 > 0 ~ ccc290), 
    ccl201b = case_when(ccl201 > 0 ~ ccl201),
    ccl216b = case_when(ccl216 > 0 ~ ccl216),
    bullying_experienced_age_12y1m = case_when(
      ccl216b < 5 | ccl201b < 5 ~ 1,
      ccl216b == 5 | ccl201b == 5 ~ 0), 
    kl791b = case_when(kl791 > 0 & kl791 < 9 ~ kl791),
    kl801b = case_when(kl801 > 0 & kl801 < 9 ~ kl801), 
    bullying_experienced_age_4y8m = case_when(
      kl801b > 1 | kl791b > 1 ~ 1,
      kl801b == 1 | kl791b == 1 ~ 0))

bullying %>% count(bullying_victim_age_8y, f8fp470, f8fp475)
bullying %>% count(bullying_victim_age_10, fdfp470, fdfp475)
bullying %>% count(bullying_often_bullied_age_13y1m, ta7018)
bullying %>% count(bullying_frequency_bullied_age_8y1m, ccc290)

bullying %>% count(ccl201b, ccl201)
bullying %>% count(ccl216b, ccl216)
bullying %>% filter(ccl216b < 5 | ccl201b < 5) %>% count(bullying_experienced_age_12y1m)
bullying %>% filter(ccl216b == 5 | ccl201b == 5) %>% count(bullying_experienced_age_12y1m)
bullying %>% count(bullying_experienced_age_12y1m)

bullying %>% count(kl801b, kl801)
bullying %>% count(kl791b, kl791)
bullying %>% count(bullying_experienced_age_4y8m)
bullying %>% filter(kl791b == 1 | kl801b == 1) %>% count(bullying_experienced_age_4y8m)
bullying %>% filter(kl791b > 1 | kl801b > 1) %>% count(bullying_experienced_age_4y8m)

glimpse(bullying)

################################################################################
### Friends domain ###

# Review data:

#Happy with number of friends

alspac %>% count(f8fs110) #age 8
attributes(alspac$f8fs110)

alspac %>% count(fdfs110) #age 10
attributes(alspac$fdfs110)

alspac %>% count(ff5401) #age 12
attributes(alspac$ff5401)

alspac %>% count(fg4120) #age 13.5
attributes(alspac$fg4120)

alspac %>% count(FJPC100) #age 17
attributes(alspac$FJPC100)

#Friends understand child

alspac %>% count(f8fs112) #age 8
attributes(alspac$f8fs112)

alspac %>% count(fdfs117) #age 10
attributes(alspac$fdfs117)

alspac %>% count(ff5409) #age 12
attributes(alspac$ff5409)

alspac %>% count(fg4128) #age 13.5
attributes(alspac$fg4128)

alspac %>% count(FJPC200) #age 17
attributes(alspac$FJPC200)

#Overall happiness with friends

alspac %>% count(f8fs114) #age 8
attributes(alspac$f8fs114)

alspac %>% count(fdfs119) #age 10
attributes(alspac$fdfs119)

alspac %>% count(ff5411) #age 12
attributes(alspac$ff5411)

alspac %>% count(fg4130) #age 13.5
attributes(alspac$fg4130)

alspac %>% count(FJPC350) #age 17
attributes(alspac$FJPC350)

#Talk to your friends about their problems

alspac %>% count(f8fs113) #age 8
attributes(alspac$f8fs113)

alspac %>% count(fdfs118) #age 10
attributes(alspac$fdfs118)

alspac %>% count(ff5410) #age 12
attributes(alspac$ff5410)

alspac %>% count(fg4129) #age 13.5
attributes(alspac$fg4129)

alspac %>% count(FJPC150) #age 17
attributes(alspac$FJPC150)

#How often see friends outside of school 

alspac %>% count(f8fs111) #age 8
attributes(alspac$f8fs111)

alspac %>% count(fdfs115) #age 10
attributes(alspac$fdfs115)

alspac %>% count(ff5407) #age 12
attributes(alspac$ff5407)

alspac %>% count(fg4126) #age 13.5
attributes(alspac$fg4126)

alspac %>% count(FJPC250) #age 17
attributes(alspac$FJPC250)

### Close friends

# Original proposal was a binary measure of any close friends vs none. 
# Too few in 'none' group, so keep as a categorised

alspac %>% count(pg4140)	#3y 9m - Study child had at least one good friend
attributes(alspac$pg4140) # Low frequency, and question phrased differently, advise inconsistent. 

alspac %>% count(tc4010)
attributes(alspac$tc4010)

alspac %>% count(n8350)	#8y 1m
attributes(alspac$n8350) # Study child had at least one good friend 

alspac %>% count(fefs010)	%>% print(n=40) # Age 11 years, child clinic dataset
attributes(alspac$fefs010)

alspac %>% count(ff5402) %>% print(n=40)	# Age 12y6m, child cinlic dataset
attributes(alspac$ff5402)

alspac %>% count(fg4121)	 %>% print(n=40) # Age 13y 6m, child clinic dataset
attributes(alspac$fg4121)

alspac %>% count(FJPC050)	 %>% print(n=40)	# TF4, clinic dataset from child, age 17y 6m
attributes(alspac$FJPC050)

alspac %>% count(cct2501)	# from child, age 18y 7m
attributes(alspac$cct2501)
#Inconsistent - exclude. 

alspac %>% count(TD0150)	
attributes(alspac$TD0150) # from mother, age 19 years
#Inconsistent - exclude. 

alspac %>% count(FKFR1000)	# F24, clinic, age 24
attributes(alspac$FKFR1000)


#Other variables

alspac %>% count(kd390) #age 1
attributes(alspac$kd390)

alspac %>% count(kn3124) #age 5
attributes(alspac$kn3124)

################################################################################
### Create friends dataset
friends <- alspac %>%
  select(uniqid, kd390, kn3124, f8fs110, fdfs110, ff5401, fg4120, FJPC100,  f8fs112, fdfs117,
         ff5409, fg4128, FJPC200, f8fs114, fdfs119, ff5411, fg4130, FJPC350,  f8fs113, fdfs118,
         ff5410, fg4129, FJPC150, f8fs111, fdfs115, ff5407, fg4126, FJPC250, 
         pg4140, 	n8350, fefs010, 	fefs010a, 	ff5402, 	fg4121, 	FJPC050, 	cct2501, 	TD0150, 	FKFR1000, tc4010)

friends <- friends %>%
  mutate(
    friends_plays_with_kids_age_1y6m = case_when(kd390 > 0 ~ kd390),
    friends_freq_visits_age_5y9m = case_when(kn3124 > 0 ~ kn3124),
    friends_happy_num_friends_age_8y = case_when(f8fs110 > 0 ~ f8fs110),
    friends_happy_num_friends_age_10y = case_when(fdfs110 > 0 ~ fdfs110),
    friends_happy_num_friends_age_12y6m = case_when(ff5401 > 0 ~ ff5401),
    friends_happy_num_friends_age_13y6m = case_when(fg4120 > 0 ~ fg4120),
    friends_happy_num_friends_age_17y6m = case_when(FJPC100 > 0 ~ FJPC100),
    friends_understand_age_8y = case_when(f8fs112 > 0 ~ f8fs112),
    friends_understand_age_10y = case_when(fdfs117 > 0 ~ fdfs117),
    friends_understand_age_12y6m = case_when(ff5409 > 0 ~ ff5409),
    friends_understand_age_13y6m = case_when(fg4128 > 0 ~ fg4128),
    friends_understand_age_17y6m = case_when(FJPC200 > 0 ~ FJPC200),
    friends_overall_happy_age_8y = case_when(f8fs114 > 0 ~ f8fs114),
    friends_overall_happy_age_10y = case_when(fdfs119 > 0 ~ fdfs119),
    friends_overall_happy_age_12y6m = case_when(ff5411 > 0 ~ ff5411),
    friends_overall_happy_age_13y6m = case_when(fg4130 > 0 ~ fg4130),
    friends_overall_happy_age_17y6m = case_when(FJPC350 > 0 ~ FJPC350),
    friends_talk_problems_age_8y = case_when(f8fs113 > 0 ~ f8fs113),
    friends_talk_problems_age_10y = case_when(fdfs118 > 0 ~ fdfs118),
    friends_talk_problems_age_12y6m = case_when(ff5410 > 0 ~ ff5410),
    friends_talk_problems_age_13y6m = case_when(fg4129 > 0 ~ fg4129),
    friends_talk_problems_age_17y6m = case_when(FJPC150 > 0 ~ FJPC150),
    friends_freq_see_outside_school_age_8y = case_when(f8fs111 > 0 ~ f8fs111),
    friends_freq_see_outside_school_age_10y = case_when(fdfs115 > 0 ~ fdfs115),
    friends_freq_see_outside_school_age_12y6m = case_when(ff5407 > 0 ~ ff5407),
    friends_freq_see_outside_school_age_13y6m = case_when(fg4126 > 0 ~ fg4126),
    friends_freq_see_outside_school_age_17y6m = case_when(FJPC250 > 0 ~ FJPC250)
  )

# Check friends DVs
friends %>% count(friends_plays_with_kids_age_1y6m, kd390)
friends %>% count(friends_freq_visits_age_5y9m, kn3124)

# Happy with num of friends
friends %>% count(friends_happy_num_friends_age_8y, f8fs110)
friends %>% count(friends_happy_num_friends_age_10y, fdfs110)
friends %>% count(friends_happy_num_friends_age_12y6m, ff5401)
friends %>% count(friends_happy_num_friends_age_13y6m, fg4120)
friends %>% count(friends_happy_num_friends_age_17y6m, FJPC100)

# Whether friends understand child
friends %>% count(friends_understand_age_8y, f8fs112)
friends %>% count(friends_understand_age_10y, fdfs117)
friends %>% count(friends_understand_age_12y6m, ff5409)
friends %>% count(friends_understand_age_13y6m, fg4128)
friends %>% count(friends_understand_age_17y6m, FJPC200)

# Whether overall happy with friends
friends %>% count(friends_overall_happy_age_8y, f8fs114)
friends %>% count(friends_overall_happy_age_10y, fdfs119)
friends %>% count(friends_overall_happy_age_12y6m, ff5411)
friends %>% count(friends_overall_happy_age_13y6m, fg4130)
friends %>% count(friends_overall_happy_age_17y6m, FJPC350)

# Whether would talk about problems with friends
friends %>% count(friends_talk_problems_age_8y, f8fs113)
friends %>% count(friends_talk_problems_age_10y, fdfs118)
friends %>% count(friends_talk_problems_age_12y6m, ff5410)
friends %>% count(friends_talk_problems_age_13y6m, fg4129)
friends %>% count(friends_talk_problems_age_17y6m, FJPC150)

# frequency see friends outside school
friends %>% count(friends_freq_see_outside_school_age_8y, f8fs111)
friends %>% count(friends_freq_see_outside_school_age_10y, fdfs115)
friends %>% count(friends_freq_see_outside_school_age_12y6m, ff5407)
friends %>% count(friends_freq_see_outside_school_age_13y6m, fg4126)
friends %>% count(friends_freq_see_outside_school_age_17y6m, FJPC250)


# Create numeric versions of friend score vars and remove extraneous values.

friends <- friends %>%
  mutate(
    friends_happy_num_friends_age_8y_numeric  = as.numeric(friends_happy_num_friends_age_8y),
    friends_happy_num_friends_age_10y_numeric  = as.numeric(friends_happy_num_friends_age_10y),
    friends_happy_num_friends_age_12y6m_numeric  = as.numeric(friends_happy_num_friends_age_12y6m),
    friends_happy_num_friends_age_13y6m_numeric  = as.numeric(friends_happy_num_friends_age_13y6m),
    friends_happy_num_friends_age_17y6m_numeric  = as.numeric(friends_happy_num_friends_age_17y6m),
    friends_understand_age_8y_numeric  = as.numeric(friends_understand_age_8y),
    friends_understand_age_10y_numeric  = as.numeric(friends_understand_age_10y),
    friends_understand_age_12y6m_numeric  = as.numeric(friends_understand_age_12y6m),
    friends_understand_age_13y6m_numeric  = as.numeric(friends_understand_age_13y6m),
    friends_understand_age_17y6m_numeric  = as.numeric(friends_understand_age_17y6m),
    friends_overall_happy_age_8y_numeric  = as.numeric(friends_overall_happy_age_8y),
    friends_overall_happy_age_10y_numeric  = as.numeric(friends_overall_happy_age_10y),
    friends_overall_happy_age_12y6m_numeric  = as.numeric(friends_overall_happy_age_12y6m),
    friends_overall_happy_age_13y6m_numeric  = as.numeric(friends_overall_happy_age_13y6m),
    friends_overall_happy_age_17y6m_numeric  = as.numeric(friends_overall_happy_age_17y6m),
    friends_talk_problems_age_8y_numeric  = as.numeric(friends_talk_problems_age_8y),
    friends_talk_problems_age_10y_numeric  = as.numeric(friends_talk_problems_age_10y),
    friends_talk_problems_age_12y6m_numeric  = as.numeric(friends_talk_problems_age_12y6m),
    friends_talk_problems_age_13y6m_numeric  = as.numeric(friends_talk_problems_age_13y6m),
    friends_talk_problems_age_17y6m_numeric  = as.numeric(friends_talk_problems_age_17y6m),
    friends_freq_see_outside_school_age_8y_numeric  = as.numeric(friends_freq_see_outside_school_age_8y),
    friends_freq_see_outside_school_age_10y_numeric  = as.numeric(friends_freq_see_outside_school_age_10y),
    friends_freq_see_outside_school_age_12y6m_numeric  = as.numeric(friends_freq_see_outside_school_age_12y6m),
    friends_freq_see_outside_school_age_13y6m_numeric  = as.numeric(friends_freq_see_outside_school_age_13y6m),
    friends_freq_see_outside_school_age_17y6m_numeric  = as.numeric(friends_freq_see_outside_school_age_17y6m))

#Check variables and remove extraneous value - age 8 
friends %>% count(friends_happy_num_friends_age_8y_numeric , friends_happy_num_friends_age_8y)
friends %>% count(friends_happy_num_friends_age_10y_numeric , friends_happy_num_friends_age_10y)
friends %>% count(friends_happy_num_friends_age_12y6m_numeric , friends_happy_num_friends_age_12y6m)
friends %>% count(friends_happy_num_friends_age_13y6m_numeric , friends_happy_num_friends_age_13y6m)
friends %>% count(friends_happy_num_friends_age_17y6m_numeric , friends_happy_num_friends_age_17y6m)

#Drop don't know at age 8
friends <- friends %>% mutate(friends_happy_num_friends_age_8y_numeric = case_when(friends_happy_num_friends_age_8y_numeric < 5 ~ friends_happy_num_friends_age_8y_numeric))
friends %>% count(friends_happy_num_friends_age_8y_numeric , friends_happy_num_friends_age_8y)

# Move no friends to 'unhappy' and don't know to NA at age 10.
friends <- friends %>% mutate(friends_happy_num_friends_age_10y_numeric = case_when(
  friends_happy_num_friends_age_10y_numeric == 5 ~ 4,
  friends_happy_num_friends_age_10y_numeric < 5 ~ friends_happy_num_friends_age_10y_numeric))
friends %>% count(friends_happy_num_friends_age_10y_numeric , friends_happy_num_friends_age_10y)

# Move no friends to 'unhappy' at age 12.
friends <- friends %>% mutate(friends_happy_num_friends_age_12y6m_numeric = case_when(
  friends_happy_num_friends_age_12y6m_numeric == 5 ~ 4,
  friends_happy_num_friends_age_12y6m_numeric < 5 ~ friends_happy_num_friends_age_12y6m_numeric))
friends %>% count(friends_happy_num_friends_age_12y6m_numeric , friends_happy_num_friends_age_12y6m)

# Move no friends to 'unhappy' at age 13.
friends <- friends %>% mutate(friends_happy_num_friends_age_13y6m_numeric = case_when(
  friends_happy_num_friends_age_13y6m_numeric == 5 ~ 4,
  friends_happy_num_friends_age_13y6m_numeric < 5 ~ friends_happy_num_friends_age_13y6m_numeric))
friends %>% count(friends_happy_num_friends_age_13y6m_numeric , friends_happy_num_friends_age_13y6m)

# Move no friends to 'unhappy' at age 13, and don't know to NA.
friends <- friends %>% mutate(friends_happy_num_friends_age_17y6m_numeric = case_when(
  friends_happy_num_friends_age_17y6m_numeric == 5 ~ 4,
  friends_happy_num_friends_age_17y6m_numeric < 5 ~ friends_happy_num_friends_age_17y6m_numeric))
friends %>% count(friends_happy_num_friends_age_17y6m_numeric , friends_happy_num_friends_age_17y6m)

# Friends understand age.
friends %>% count(friends_understand_age_8y_numeric , friends_understand_age_8y)
friends %>% count(friends_understand_age_10y_numeric , friends_understand_age_10y)
friends %>% count(friends_understand_age_12y6m_numeric , friends_understand_age_12y6m)
friends %>% count(friends_understand_age_13y6m_numeric , friends_understand_age_13y6m)
friends %>% count(friends_understand_age_17y6m_numeric , friends_understand_age_17y6m)

friends <- friends %>% mutate(
  friends_understand_age_8y_numeric = case_when(friends_understand_age_8y_numeric < 5 ~ friends_understand_age_8y_numeric),
  friends_understand_age_10y_numeric = case_when(friends_understand_age_10y_numeric < 5 ~ friends_understand_age_10y_numeric),
  friends_understand_age_17y6m_numeric = case_when(friends_understand_age_17y6m_numeric < 5 ~ friends_understand_age_17y6m_numeric))
friends %>% count(friends_understand_age_8y_numeric , friends_understand_age_8y)
friends %>% count(friends_understand_age_10y_numeric , friends_understand_age_10y)
friends %>% count(friends_understand_age_17y6m_numeric , friends_understand_age_17y6m)

# Overall happy with friends
friends %>% count(friends_overall_happy_age_8y_numeric , friends_overall_happy_age_8y)
friends %>% count(friends_overall_happy_age_10y_numeric , friends_overall_happy_age_10y)
friends %>% count(friends_overall_happy_age_12y6m_numeric , friends_overall_happy_age_12y6m)
friends %>% count(friends_overall_happy_age_13y6m_numeric , friends_overall_happy_age_13y6m)
friends %>% count(friends_overall_happy_age_17y6m_numeric , friends_overall_happy_age_17y6m)

friends <- friends %>% mutate(
  friends_overall_happy_age_8y_numeric = case_when(friends_overall_happy_age_8y_numeric < 5 ~ friends_overall_happy_age_8y_numeric),
  friends_overall_happy_age_10y_numeric = case_when(friends_overall_happy_age_10y_numeric < 5 ~ friends_overall_happy_age_10y_numeric),
  friends_overall_happy_age_17y6m_numeric = case_when(
    friends_overall_happy_age_17y6m_numeric == 5 ~ 4,  
    friends_overall_happy_age_17y6m_numeric < 5 ~ friends_overall_happy_age_17y6m_numeric))
friends %>% count(friends_overall_happy_age_8y_numeric , friends_overall_happy_age_8y)
friends %>% count(friends_overall_happy_age_10y_numeric , friends_overall_happy_age_10y)
friends %>% count(friends_overall_happy_age_17y6m_numeric , friends_overall_happy_age_17y6m)

# Talk about problems with friends
friends %>% count(friends_talk_problems_age_8y_numeric , friends_talk_problems_age_8y)
friends %>% count(friends_talk_problems_age_10y_numeric , friends_talk_problems_age_10y)
friends %>% count(friends_talk_problems_age_12y6m_numeric , friends_talk_problems_age_12y6m)
friends %>% count(friends_talk_problems_age_13y6m_numeric , friends_talk_problems_age_13y6m)
friends %>% count(friends_talk_problems_age_17y6m_numeric , friends_talk_problems_age_17y6m)
friends <- friends %>% mutate(
  friends_talk_problems_age_8y_numeric = case_when(friends_talk_problems_age_8y_numeric < 5 ~ friends_talk_problems_age_8y_numeric),
  friends_talk_problems_age_10y_numeric = case_when(friends_talk_problems_age_10y_numeric < 5 ~ friends_talk_problems_age_10y_numeric),
  friends_talk_problems_age_17y6m_numeric = case_when(friends_talk_problems_age_17y6m_numeric < 5 ~ friends_talk_problems_age_17y6m_numeric))
friends %>% count(friends_talk_problems_age_8y_numeric , friends_talk_problems_age_8y)
friends %>% count(friends_talk_problems_age_10y_numeric , friends_talk_problems_age_10y)
friends %>% count(friends_talk_problems_age_17y6m_numeric , friends_talk_problems_age_17y6m)

# Frequency sees friends outside school
friends %>% count(friends_freq_see_outside_school_age_8y_numeric , friends_freq_see_outside_school_age_8y)
friends %>% count(friends_freq_see_outside_school_age_10y_numeric , friends_freq_see_outside_school_age_10y)
friends %>% count(friends_freq_see_outside_school_age_12y6m_numeric , friends_freq_see_outside_school_age_12y6m)
friends %>% count(friends_freq_see_outside_school_age_13y6m_numeric , friends_freq_see_outside_school_age_13y6m)
friends %>% count(friends_freq_see_outside_school_age_17y6m_numeric , friends_freq_see_outside_school_age_17y6m)
friends <- friends %>% mutate(
  friends_freq_see_outside_school_age_8y_numeric = case_when(friends_freq_see_outside_school_age_8y_numeric < 5 ~ friends_freq_see_outside_school_age_8y_numeric),
  friends_freq_see_outside_school_age_10y_numeric = case_when(
    friends_freq_see_outside_school_age_10y_numeric == 1 ~ 1, 
    friends_freq_see_outside_school_age_10y_numeric == 2 | friends_freq_see_outside_school_age_10y_numeric == 3 ~ 2, 
    friends_freq_see_outside_school_age_10y_numeric == 4 ~ 3, 
    friends_freq_see_outside_school_age_10y_numeric == 5 | friends_freq_see_outside_school_age_10y_numeric == 6 ~ 4),
  friends_freq_see_outside_school_age_12y6m_numeric = case_when(
    friends_freq_see_outside_school_age_12y6m_numeric == 1 ~ 1, 
    friends_freq_see_outside_school_age_12y6m_numeric == 2 | friends_freq_see_outside_school_age_12y6m_numeric == 3 ~ 2, 
    friends_freq_see_outside_school_age_12y6m_numeric == 4 ~ 3, 
    friends_freq_see_outside_school_age_12y6m_numeric == 5 | friends_freq_see_outside_school_age_12y6m_numeric == 6 ~ 4),
friends_freq_see_outside_school_age_13y6m_numeric = case_when(
  friends_freq_see_outside_school_age_13y6m_numeric == 1 ~ 1, 
  friends_freq_see_outside_school_age_13y6m_numeric == 2 | friends_freq_see_outside_school_age_13y6m_numeric == 3 ~ 2, 
  friends_freq_see_outside_school_age_13y6m_numeric == 4 ~ 3, 
  friends_freq_see_outside_school_age_13y6m_numeric == 5 | friends_freq_see_outside_school_age_13y6m_numeric == 6 ~ 4),
friends_freq_see_outside_school_age_17y6m_numeric = case_when(
  friends_freq_see_outside_school_age_17y6m_numeric == 1 ~ 1, 
  friends_freq_see_outside_school_age_17y6m_numeric == 2 | friends_freq_see_outside_school_age_17y6m_numeric == 3 ~ 2, 
  friends_freq_see_outside_school_age_17y6m_numeric == 4 ~ 3, 
  friends_freq_see_outside_school_age_17y6m_numeric == 5 | friends_freq_see_outside_school_age_17y6m_numeric == 6 ~ 4))
friends %>% count(friends_freq_see_outside_school_age_8y_numeric , friends_freq_see_outside_school_age_8y)
friends %>% count(friends_freq_see_outside_school_age_10y_numeric , friends_freq_see_outside_school_age_10y)
friends %>% count(friends_freq_see_outside_school_age_12y6m_numeric , friends_freq_see_outside_school_age_12y6m)
friends %>% count(friends_freq_see_outside_school_age_13y6m_numeric , friends_freq_see_outside_school_age_13y6m)
friends %>% count(friends_freq_see_outside_school_age_17y6m_numeric , friends_freq_see_outside_school_age_17y6m)

# Generate friends score. 
friends <- friends %>% 
  mutate(
    friends_score_age_8y = rowSums(across(c(friends_happy_num_friends_age_8y_numeric, friends_understand_age_8y_numeric, friends_overall_happy_age_8y_numeric, friends_talk_problems_age_8y_numeric, friends_freq_see_outside_school_age_8y_numeric))), 
    friends_score_age_10y = rowSums(across(c(friends_happy_num_friends_age_10y_numeric, friends_understand_age_10y_numeric, friends_overall_happy_age_10y_numeric, friends_talk_problems_age_10y_numeric, friends_freq_see_outside_school_age_10y_numeric))),
    friends_score_age_12y6m = rowSums(across(c(friends_happy_num_friends_age_12y6m_numeric, friends_understand_age_12y6m_numeric, friends_overall_happy_age_12y6m_numeric, friends_talk_problems_age_12y6m_numeric, friends_freq_see_outside_school_age_12y6m_numeric))),
    friends_score_age_13y6m = rowSums(across(c(friends_happy_num_friends_age_13y6m_numeric, friends_understand_age_13y6m_numeric, friends_overall_happy_age_13y6m_numeric, friends_talk_problems_age_13y6m_numeric, friends_freq_see_outside_school_age_13y6m_numeric))),
    friends_score_age_17y6m = rowSums(across(c(friends_happy_num_friends_age_17y6m_numeric, friends_understand_age_17y6m_numeric, friends_overall_happy_age_17y6m_numeric, friends_talk_problems_age_17y6m_numeric, friends_freq_see_outside_school_age_17y6m_numeric)))
  )
friends %>% select(friends_score_age_8y, friends_happy_num_friends_age_8y_numeric, friends_understand_age_8y_numeric, friends_overall_happy_age_8y_numeric, friends_talk_problems_age_8y_numeric, friends_freq_see_outside_school_age_8y_numeric) %>% head()
friends %>% count(friends_score_age_8y)


# Review friends score variables
friends %>% count(friends_plays_with_kids_age_1y6m, kd390)
friends %>% count(friends_freq_visits_age_5y9m, kn3124)
friends %>% count(friends_happy_num_friends_age_8y, f8fs110) #age 8
friends %>% count(friends_happy_num_friends_age_10y, fdfs110) #age 10
friends %>% count(friends_happy_num_friends_age_12y6m, ff5401) #age 12
friends %>% count(friends_happy_num_friends_age_13y6m, fg4120) #age 13.5
friends %>% count(friends_happy_num_friends_age_17y6m, FJPC100) #age 17
friends %>% count(friends_understand_age_8y, f8fs112) #age 8
friends %>% count(friends_understand_age_10y, fdfs117) #age 10
friends %>% count(friends_understand_age_12y6m, ff5409) #age 12
friends %>% count(friends_understand_age_13y6m, fg4128) #age 13.5
friends %>% count(friends_understand_age_17y6m, FJPC200) #age 17
friends %>% count(friends_overall_happy_age_8y, f8fs114) #age 8
friends %>% count(friends_overall_happy_age_10y, fdfs119) #age 10
friends %>% count(friends_overall_happy_age_12y6m, ff5411) #age 12
friends %>% count(friends_overall_happy_age_13y6m, fg4130) #age 13.5
friends %>% count(friends_overall_happy_age_17y6m, FJPC350) #age 17
friends %>% count(friends_talk_problems_age_8y, f8fs113) #age 8
friends %>% count(friends_talk_problems_age_10y, fdfs118) #age 10
friends %>% count(friends_talk_problems_age_12y6m, ff5410) #age 12
friends %>% count(friends_talk_problems_age_13y6m, fg4129) #age 13.5
friends %>% count(friends_talk_problems_age_17y6m, FJPC150) #age 17
friends %>% count(friends_freq_see_outside_school_age_8y, f8fs111) #age 8
friends %>% count(friends_freq_see_outside_school_age_10y, fdfs115) #age 10
friends %>% count(friends_freq_see_outside_school_age_12y6m, ff5407) #age 12
friends %>% count(friends_freq_see_outside_school_age_13y6m, fg4126) #age 13.5
friends %>% count(friends_freq_see_outside_school_age_17y6m, FJPC250) #age 17

glimpse(friends)

## Add number of close friend variables 

friends <- friends %>%  mutate(friends_num_close_friend_age_17y6m = case_when(FJPC050 > 0 ~ as_factor(FJPC050)), 
                               friends_num_close_friend_age_24y = case_when(FKFR1000 >= 0 ~ as_factor(FKFR1000)))

detach(package:tidyverse)
library(plyr)
friends$friends_num_close_friend_age_17y6m <- revalue(friends$friends_num_close_friend_age_17y6m, c("0"="0","1"="1","2-4"="2-4", "5-9"="5-9", "10-13"="10-14", "15-19"="15-19", "20+"="20+"))
detach(package:plyr)
library(tidyverse)

friends %>% count(friends_num_close_friend_age_17y6m, FJPC050)  
friends %>% count(friends_num_close_friend_age_24y, FKFR1000)  

friends<-friends %>%
  mutate(
    ff5402a = as.numeric(as.character(ff5402)), 
    fg4121a = as.numeric(as.character(fg4121)))

friends %>% count(ff5402a, ff5402) %>% print(n=50)
friends %>% count(fg4121a, fg4121) %>% print(n=50)

friends <- friends %>%     
 mutate(  
    friends_num_close_friend_age_11y = case_when(
      fefs010 ==0 ~ "0",
      fefs010 ==1 ~ "1", 
      fefs010 >= 2 & fefs010 <=4 ~ "2-4", 
      fefs010 >= 5 & fefs010 <=9 ~ "5-9", 
      fefs010 >= 10 & fefs010 <= 14 ~ "10-14", 
      fefs010 >= 15 & fefs010 <= 19 ~ "15-19", 
      fefs010 >= 20 ~ "20+")
    %>% fct_relevel("0", "1", "2-4", "5-9", "10-14", "15-19", "20+")
 )
friends %>% count(friends_num_close_friend_age_11y)

friends <- friends %>%     
  mutate( 
    friends_num_close_friend_age_12y6m = case_when(
      ff5402a == 0 ~ "0",
      ff5402a == 1 ~ "1", 
      ff5402a >= 2 & ff5402a <= 4 ~ "2-4", 
      ff5402a >= 5 & ff5402a <= 9 ~ "5-9", 
      ff5402a >= 10 & ff5402a <= 14 ~ "10-14", 
      ff5402a >= 15 & ff5402a <= 19 ~ "15-19", 
      ff5402a >= 20 ~ "20+")
    %>% fct_relevel("0", "1", "2-4", "5-9", "10-14", "15-19", "20+")
  )
friends %>% count(friends_num_close_friend_age_12y6m)

friends <- friends %>%     
  mutate( 
    friends_num_close_friend_age_13y6m = case_when(
      fg4121a == 0 ~ "0",
      fg4121a == 1 ~ "1", 
      fg4121a >= 2 & fg4121a <= 4 ~ "2-4", 
      fg4121a >= 5 & fg4121a <= 9 ~ "5-9", 
      fg4121a >= 10 & fg4121a <= 14 ~ "10-14", 
      fg4121a >= 15 & fg4121a <= 19 ~ "15-19", 
      fg4121a >= 20 ~ "20+")
    %>% fct_relevel("0", "1", "2-4", "5-9", "10-14", "15-19", "20+")
  )
friends %>% count(friends_num_close_friend_age_13y6m)

friends %>% count(friends_num_close_friend_age_11y, fefs010)  %>% print(n=50)
friends %>% count(friends_num_close_friend_age_12y6m, ff5402)  %>% print(n=50)
friends %>% count(friends_num_close_friend_age_13y6m, fg4121)  %>% print(n=50)

friends %>% count(pg4140) %>% print(n=50)

friends <- friends %>%  mutate(friends_num_close_friend_inconsistent_age_18y7m = case_when(cct2501 > 0 & cct2501 < 7 ~ as_factor(cct2501)), 
                               friends_num_close_friend_inconsistent_age_19y = case_when(TD0150 > 0 ~ as_factor(TD0150)), 
                               friends_whether_any_close_friend_age_3y9m = case_when(pg4140 > 0 ~ as_factor(pg4140)), 
                               friends_whether_any_close_friend_age_8y1m = case_when(n8350 > 0 ~ as_factor(n8350)), 
                               friends_whether_any_close_friend_age_16y0m = case_when(tc4010 > 0 &  tc4010 < 9 ~ as_factor(tc4010)))
friends %>% count(friends_num_close_friend_inconsistent_age_18y7m, cct2501)  %>% print(n=50)
friends %>% count(friends_num_close_friend_inconsistent_age_19y, TD0150)  %>% print(n=50)
friends %>% count(friends_whether_any_close_friend_age_3y9m, pg4140)  %>% print(n=50)
friends %>% count(friends_whether_any_close_friend_age_8y1m, n8350)  %>% print(n=50)
friends %>% count(friends_whether_any_close_friend_age_16y0m, tc4010)  %>% print(n=50)


###############################################################################
### Help from religious leaders or other members of their religion
################################################################################
# Import religion variables
# Related to parent's religious networks, not child's. 

parents_religion <- alspac %>% select(uniqid, d817, d818,  pb156, pb157, k6248, k6249, ph6248, ph6249, l7050, l7051, pj7050, pj7051)

# Review variables

parents_religion %>% count(d817)	# age 12 weeks gestation
attributes(parents_religion$d817)

parents_religion %>% count(d818)	
attributes(parents_religion$d818)

parents_religion %>% count(pb156)	# age 18 weeks gestation
attributes(parents_religion$pb156)

parents_religion %>% count(pb157)	
attributes(parents_religion$pb157)

parents_religion %>% count(k6248)	
attributes(parents_religion$k6248)

parents_religion %>% count(k6249)	
attributes(parents_religion$k6249)

parents_religion %>% count(ph6248)	
attributes(parents_religion$ph6248)

parents_religion %>% count(ph6249)	
attributes(parents_religion$ph6249)

parents_religion %>% count(l7050)	
attributes(parents_religion$l7050)

parents_religion %>% count(l7051)	
attributes(parents_religion$l7051)

parents_religion %>% count(pj7050)	
attributes(parents_religion$pj7050)

parents_religion %>% count(pj7051)	
attributes(parents_religion$pj7051)

# Derive parent's religion variables, remove NAs

parents_religion <- parents_religion %>% mutate(
  religion_support_mother_age_gest = case_when(d817 == 1 |  d818 == 1 ~ 1, d817 == 2 | d818 == 2 ~ 0),
  religion_support_partner_age_gest = case_when(pb156 == 1 |  pb157 == 1 ~ 1, pb156 == 2 | pb157 == 2 ~ 0),
  religion_support_mother_age_5y1m = case_when(k6248 == 1 |  k6249 == 1 ~ 1, k6248 == 2 | k6249 == 2 ~ 0),
  religion_support_partner_age_5y1m = case_when(ph6248 == 1 |  ph6249 == 1 ~ 1, ph6248 == 2 | ph6249 == 2 ~ 0),
  religion_support_mother_age_6y1m = case_when(l7050 == 1 |  l7051 == 1 ~ 1, l7050 == 2 | l7051 == 2 ~ 0),
  religion_support_partner_age_6y1m = case_when(pj7050 == 1 |  pj7051 == 1 ~ 1, pj7050 == 2 | pj7051 == 2 ~ 0))

# Check derived variables
parents_religion %>% count(religion_support_mother_age_gest )
parents_religion %>% count(religion_support_partner_age_gest )
parents_religion %>% count(religion_support_mother_age_5y1m )
parents_religion %>% count(religion_support_partner_age_5y1m )
parents_religion %>% count(religion_support_mother_age_6y1m )
parents_religion %>% count(religion_support_partner_age_6y1m )


###############################################################################
### Social networks of the parent

    ## Including:
    ## The support score - D, E, F, G, L, K, P, S
    ## The social network score - D, G, T, PB, PE, FA

################################################################################

# Import variables into separate data frame.
parent_networks <- alspac %>% select(uniqid, d800, e610, f920, g226, 
                                     l7020, 	l7024, 	k8020, 	k8024, 	p4020, 	p4024, 	s6020, 	s6024, 
                                     l7021, 	l7022, 	l7023, 	l7025, 	l7026, 	l7027, 	l7028, 	l7029, 	
                                     k8021, 	k8022, 	k8023, 	k8025, 	k8026, 	k8027, 	k8028, 	k8029, 	
                                     p4021, 	p4022, 	p4023, 	p4025, 	p4026, 	p4027, 	p4028, 	p4029, 
                                     s6021, 	s6022, 	s6023, 	s6025, 	s6026, 	s6027, 	s6028, 	s6029, 
                                     d780,	pb120,	pe230,	g240,	g241,	t2050,	fa2050,
                                     pb140, pc340, pd750, pe210, 
                                     ph8020, ph8024, pm4020, pm4024, pq6020, pq6024, 
                                     ph8021, 	ph8023, 	ph8025, 	ph8026, 	ph8027, 	ph8028, 	ph8029, 	pm4021, 	pm4022, 	pm4023, 	pm4025, 	pm4026, 	pm4027, 	pm4028, 	pm4029, 	pq6021, 	pq6022, 	pq6023, 	pq6025, 	pq6026, 	pq6027, 	pq6028, 	pq6029)
                                     

### The social support score (of the parent) ###################################################

# Pre-derived scores: 
alspac %>% count(d800) %>% print(n=50)
attributes(alspac$d800)

alspac %>% count(e610) %>% print(n=50)
attributes(alspac$e610)

alspac %>% count(f920) %>% print(n=50)
attributes(alspac$f920)

alspac %>% count(g226) %>% print(n=50)
attributes(alspac$g226)

# Items composing pre-derived scores:
# Recode no on to share feelings with and feels worried partner may leave: 
    #(1=0) (2=1) (3=2) (4=3)

alspac %>% select(l7020, 	l7024, 	k8020, 	k8024, 	p4020, 	p4024, 	s6020, 	s6024) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(l7020, 	l7024, 	k8020, 	k8024, 	p4020, 	p4024, 	s6020, 	s6024))) {
  alspac %>% count(.data[[var]]) %>% print()
}

parent_networks <- parent_networks %>% mutate(
  l7020a = case_when(l7020 > 0 & l7020 < 7 ~ -1 + l7020),
  l7024a = case_when(l7024 > 0 & l7024 < 7 ~ -1 + l7024),
  k8020a = case_when(k8020 > 0 & k8020 < 7 ~ -1 + k8020),
  k8024a = case_when(k8024 > 0 & k8024 < 7 ~ -1 + k8024),
  p4020a = case_when(p4020 > 0 & p4020 < 7 ~ -1 + p4020),
  p4024a = case_when(p4024 > 0 & p4024 < 7 ~ -1 + p4024),
  s6020a = case_when(s6020 > 0 & s6020 < 7 ~ -1 + s6020),
  s6024a = case_when(s6024 > 0 & s6024 < 7 ~ -1 + s6024))

parent_networks %>% count(l7020a, l7020)
parent_networks %>% count(l7024a, l7024)
parent_networks %>% count(k8020a, k8020)
parent_networks %>% count(k8024a, k8024)
parent_networks %>% count(p4020a, p4020)
parent_networks %>% count(p4024a, p4024)
parent_networks %>% count(s6020a, s6020)
parent_networks %>% count(s6024a, s6024)

alspac %>% select(l7021, 	l7022, 	l7023, 	l7025, 	l7026, 	l7027, 	l7028, 	l7029, 	k8021, 	k8022, 	k8023, 	k8025, 	k8026, 	k8027, 	k8028, 	k8029, 	p4021, 	p4022, 	p4023, 	p4025, 	p4026, 	p4027, 	p4028, 	p4029, 	s6021, 	s6022, 	s6023, 	s6025, 	s6026, 	s6027, 	s6028, 	s6029) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(l7021, 	l7022, 	l7023, 	l7025, 	l7026, 	l7027, 	l7028, 	l7029, 	k8021, 	k8022, 	k8023, 	k8025, 	k8026, 	k8027, 	k8028, 	k8029, 	p4021, 	p4022, 	p4023, 	p4025, 	p4026, 	p4027, 	p4028, 	p4029, 	s6021, 	s6022, 	s6023, 	s6025, 	s6026, 	s6027, 	s6028, 	s6029))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# Reverse coding of score items
parent_networks <- parent_networks %>% mutate(
  l7021a = case_when(l7021 > 0  & l7021 < 5 ~  4 - l7021),
  l7022a = case_when(l7022 > 0  & l7022 < 5 ~  4 - l7022),
  l7023a = case_when(l7023 > 0  & l7023 < 5 ~  4 - l7023),
  l7025a = case_when(l7025 > 0  & l7025 < 5 ~  4 - l7025),
  l7026a = case_when(l7026 > 0  & l7026 < 5 ~  4 - l7026),
  l7027a = case_when(l7027 > 0  & l7027 < 5 ~  4 - l7027),
  l7028a = case_when(l7028 > 0  & l7028 < 5 ~  4 - l7028),
  l7029a = case_when(l7029 > 0  & l7029 < 5 ~  4 - l7029),
  k8021a = case_when(k8021 > 0  & k8021 < 5 ~  4 - k8021),
  k8022a = case_when(k8022 > 0  & k8022 < 5 ~  4 - k8022),
  k8023a = case_when(k8023 > 0  & k8023 < 5 ~  4 - k8023),
  k8025a = case_when(k8025 > 0  & k8025 < 5 ~  4 - k8025),
  k8026a = case_when(k8026 > 0  & k8026 < 5 ~  4 - k8026),
  k8027a = case_when(k8027 > 0  & k8027 < 5 ~  4 - k8027),
  k8028a = case_when(k8028 > 0  & k8028 < 5 ~  4 - k8028),
  k8029a = case_when(k8029 > 0  & k8029 < 5 ~  4 - k8029),
  p4021a = case_when(p4021 > 0  & p4021 < 5 ~  4 - p4021),
  p4022a = case_when(p4022 > 0  & p4022 < 5 ~  4 - p4022),
  p4023a = case_when(p4023 > 0  & p4023 < 5 ~  4 - p4023),
  p4025a = case_when(p4025 > 0  & p4025 < 5 ~  4 - p4025),
  p4026a = case_when(p4026 > 0  & p4026 < 5 ~  4 - p4026),
  p4027a = case_when(p4027 > 0  & p4027 < 5 ~  4 - p4027),
  p4028a = case_when(p4028 > 0  & p4028 < 5 ~  4 - p4028),
  p4029a = case_when(p4029 > 0  & p4029 < 5 ~  4 - p4029),
  s6021a = case_when(s6021 > 0  & s6021 < 5 ~  4 - s6021),
  s6022a = case_when(s6022 > 0  & s6022 < 5 ~  4 - s6022),
  s6023a = case_when(s6023 > 0  & s6023 < 5 ~  4 - s6023),
  s6025a = case_when(s6025 > 0  & s6025 < 5 ~  4 - s6025),
  s6026a = case_when(s6026 > 0  & s6026 < 5 ~  4 - s6026),
  s6027a = case_when(s6027 > 0  & s6027 < 5 ~  4 - s6027),
  s6028a = case_when(s6028 > 0  & s6028 < 5 ~  4 - s6028),
  s6029a = case_when(s6029 > 0  & s6029 < 5 ~  4 - s6029))

# Derive score 
parent_networks  <- parent_networks %>% mutate(
  parent_networks_mother_social_support_score_age_gest = case_when(d800 >= 0 ~ d800),
  parent_networks_mother_social_support_score_age_0y2m = case_when(e610 >= 0 ~ e610),
  parent_networks_mother_social_support_score_age_0y8m = case_when(f920 >= 0 ~ f920),
  parent_networks_mother_social_support_score_age_1y9m = case_when(g226 >= 0 ~ g226), 
  parent_networks_mother_social_support_score_age_6y1m =l7020a + 	l7021a + 	l7022a + 	l7023a + 	l7024a + 	l7025a + 	l7026a + 	l7027a + 	l7028a + 	l7029a,
  parent_networks_mother_social_support_score_age_5y1m = k8020a + 	k8021a + 	k8022a + 	k8023a + 	k8024a + 	k8025a + 	k8026a + 	k8027a + 	k8028a + 	k8029a,
  parent_networks_mother_social_support_score_age_9y1m = p4020a + 	p4021a + 	p4022a + 	p4023a + 	p4024a + 	p4025a + 	p4026a + 	p4027a + 	p4028a + 	p4029a,
  parent_networks_mother_social_support_score_age_12y1m = s6020a + 	s6021a + 	s6022a + 	s6023a + 	s6024a + 	s6025a + 	s6026a + 	s6027a + 	s6028a + 	s6029a)

parent_networks %>% count(d800, parent_networks_mother_social_support_score_age_gest) %>% print(n=50)
parent_networks %>% count(e610, parent_networks_mother_social_support_score_age_0y2m) %>% print(n=50)
parent_networks %>% count(f920, parent_networks_mother_social_support_score_age_0y8m) %>% print(n=50)
parent_networks %>% count(g226, parent_networks_mother_social_support_score_age_1y9m) %>% print(n=50)

glimpse(parent_networks)

################################################################################
#### Social support score of the partner

# Recode no on to share feelings with and feels worried partner may leave: 
#(1=0) (2=1) (3=2) (4=3)

reverse_order_vars <- c("ph8020", "ph8024", "pm4020", "pm4024", "pq6020", "pq6024")
non_reverse_order_vars <- c("ph8021",	"ph8023",	"ph8025",	"ph8026",	"ph8027",	"ph8028",	"ph8029",	"pm4021",	"pm4022",	"pm4023",	"pm4025",	"pm4026",	"pm4027",	"pm4028",	"pm4029",	"pq6021",	"pq6022",	"pq6023",	"pq6025",	"pq6026",	"pq6027",	"pq6028",	"pq6029")

# Review items
alspac %>% select(reverse_order_vars) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(reverse_order_vars))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# Remove NAs, reverse order of variables
parent_networks <- parent_networks %>% mutate(
  ph8020a = case_when(ph8020 > 0 & ph8020 < 7 ~ -1 + ph8020),
  ph8024a = case_when(ph8024 > 0 & ph8024 < 7 ~ -1 + ph8024),
  pm4020a = case_when(pm4020 > 0 & pm4020 < 7 ~ -1 + pm4020),
  pm4024a = case_when(pm4024 > 0 & pm4024 < 7 ~ -1 + pm4024),
  pq6020a = case_when(pq6020 > 0 & pq6020 < 7 ~ -1 + pq6020),
  pq6024a = case_when(pq6024 > 0 & pq6024 < 7 ~ -1 + pq6024))

parent_networks %>% count(ph8020a , ph8020)
parent_networks %>% count(ph8024a , ph8024)
parent_networks %>% count(pm4020a , pm4020)
parent_networks %>% count(pm4024a , pm4024)
parent_networks %>% count(pq6020a , pq6020)
parent_networks %>% count(pq6024a , pq6024)

alspac %>% select(non_reverse_order_vars) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(non_reverse_order_vars))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# Recode other variables: (1=3)(2=2)(3=1)(4=0).

parent_networks <- parent_networks %>% mutate(
  ph8021a = case_when(ph8021 > 0 & ph8021 < 5 ~ 4 - ph8021),
  ph8023a = case_when(ph8023 > 0 & ph8023 < 5 ~ 4 - ph8023),
  ph8025a = case_when(ph8025 > 0 & ph8025 < 5 ~ 4 - ph8025),
  ph8026a = case_when(ph8026 > 0 & ph8026 < 5 ~ 4 - ph8026),
  ph8027a = case_when(ph8027 > 0 & ph8027 < 5 ~ 4 - ph8027),
  ph8028a = case_when(ph8028 > 0 & ph8028 < 5 ~ 4 - ph8028),
  ph8029a = case_when(ph8029 > 0 & ph8029 < 5 ~ 4 - ph8029),
  pm4021a = case_when(pm4021 > 0 & pm4021 < 5 ~ 4 - pm4021),
  pm4022a = case_when(pm4022 > 0 & pm4022 < 5 ~ 4 - pm4022),
  pm4023a = case_when(pm4023 > 0 & pm4023 < 5 ~ 4 - pm4023),
  pm4025a = case_when(pm4025 > 0 & pm4025 < 5 ~ 4 - pm4025),
  pm4026a = case_when(pm4026 > 0 & pm4026 < 5 ~ 4 - pm4026),
  pm4027a = case_when(pm4027 > 0 & pm4027 < 5 ~ 4 - pm4027),
  pm4028a = case_when(pm4028 > 0 & pm4028 < 5 ~ 4 - pm4028),
  pm4029a = case_when(pm4029 > 0 & pm4029 < 5 ~ 4 - pm4029),
  pq6021a = case_when(pq6021 > 0 & pq6021 < 5 ~ 4 - pq6021),
  pq6022a = case_when(pq6022 > 0 & pq6022 < 5 ~ 4 - pq6022),
  pq6023a = case_when(pq6023 > 0 & pq6023 < 5 ~ 4 - pq6023),
  pq6025a = case_when(pq6025 > 0 & pq6025 < 5 ~ 4 - pq6025),
  pq6026a = case_when(pq6026 > 0 & pq6026 < 5 ~ 4 - pq6026),
  pq6027a = case_when(pq6027 > 0 & pq6027 < 5 ~ 4 - pq6027),
  pq6028a = case_when(pq6028 > 0 & pq6028 < 5 ~ 4 - pq6028),
  pq6029a = case_when(pq6029 > 0 & pq6029 < 5 ~ 4 - pq6029))

parent_networks %>% count(ph8021a , ph8021)
parent_networks %>% count(ph8023a , ph8023)
parent_networks %>% count(ph8025a , ph8025)
parent_networks %>% count(ph8026a , ph8026)
parent_networks %>% count(ph8027a , ph8027)
parent_networks %>% count(ph8028a , ph8028)
parent_networks %>% count(ph8029a , ph8029)
parent_networks %>% count(pm4021a , pm4021)
parent_networks %>% count(pm4022a , pm4022)
parent_networks %>% count(pm4023a , pm4023)
parent_networks %>% count(pm4025a , pm4025)
parent_networks %>% count(pm4026a , pm4026)
parent_networks %>% count(pm4027a , pm4027)
parent_networks %>% count(pm4028a , pm4028)
parent_networks %>% count(pm4029a , pm4029)
parent_networks %>% count(pq6021a , pq6021)
parent_networks %>% count(pq6022a , pq6022)
parent_networks %>% count(pq6023a , pq6023)
parent_networks %>% count(pq6025a , pq6025)
parent_networks %>% count(pq6026a , pq6026)
parent_networks %>% count(pq6027a , pq6027)
parent_networks %>% count(pq6028a , pq6028)
parent_networks %>% count(pq6029a , pq6029)

# Derive parent networks score
parent_networks  <- parent_networks %>% mutate(
  parent_networks_partner_social_support_score_inconsistent_age_5y1m = ph8020a + ph8021a + ph8023a + ph8024a + ph8025a + ph8026a + ph8027a + ph8028a + ph8029a,
  parent_networks_partner_social_support_score_age_9y2m = pm4020a + pm4021a + pm4022a + pm4023a + pm4024a + pm4025a + pm4026a + pm4027a + pm4028a + pm4029a,
  parent_networks_partner_social_support_score_age_12y1m = pq6020a + pq6021a + pq6022a + pq6023a + pq6024a + pq6025a + pq6026a + pq6027a + pq6028a + pq6029a)

# Add existing derived variables  
alspac %>% select(pb140, pc340, pd750, pe210) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(pb140, pc340, pd750, pe210))) {
  alspac %>% count(.data[[var]]) %>% print(n=40)

# Rename existing parent network score variables (those already present in the data)
parent_networks  <- parent_networks %>% mutate(
  parent_networks_partner_social_support_score_age_gest = case_when(pb140 >= 0 ~ pb140),
  parent_networks_partner_social_support_score_age_0y2m = case_when(pc340 >= 0 ~ pc340),
  parent_networks_partner_social_support_score_age_0y8m = case_when(pd750 >= 0 ~ pd750),
  parent_networks_partner_social_support_score_age_1y9m = case_when(pe210 >= 0 ~ pe210))


### The social network score (of the parents) ###################################################

# Review variables
alspac %>% select(d780,	pb120,	pe230,	g240,	g241,	t2050,	fa2050) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(d780,	pb120,	pe230,	g240,	g241,	t2050,	fa2050))) {
  alspac %>% count(.data[[var]]) %>% print(n=50)
}

# rename variables, remove NAs
parent_networks  <- parent_networks %>% mutate(
  parent_networks_mother_social_networks_score_age_gest = case_when(d780 >= 0 ~ d780),
  parent_networks_mother_social_networks_score_age_1y9m = case_when(g240 >= 0 ~ g240),
  parent_networks_mother_social_networks_score_age_18y6m = case_when(t2050 >= 0 ~ t2050),
  parent_networks_partner_social_networks_score_age_gest = case_when(pb120 >= 0 ~ pb120),
  parent_networks_partner_social_networks_score_age_1y9m = case_when(pe230 >= 0 ~ pe230),
  parent_networks_partner_social_networks_score_age_19y8m = case_when(fa2050 >= 0 ~ fa2050))

# Check new variables
parent_networks %>% count(parent_networks_mother_social_networks_score_age_gest, d780) %>% print(n=50)
parent_networks %>% count(parent_networks_mother_social_networks_score_age_1y9m, g240) %>% print(n=50)
parent_networks %>% count(parent_networks_mother_social_networks_score_age_18y6m, t2050) %>% print(n=50)
parent_networks %>% count(parent_networks_partner_social_networks_score_age_gest, pb120) %>% print(n=50)
parent_networks %>% count(parent_networks_partner_social_networks_score_age_1y9m, pe230) %>% print(n=50)
parent_networks %>% count(parent_networks_partner_social_networks_score_age_19y8m, fa2050) %>% print(n=50)




################################################################################
### romantic relationships ###

# Create separate dataset
romantic <- alspac %>% select(uniqid, fg4190, tc1210, FJPC1150, FJPC1000, FKFR1120)

# Review original variables
romantic %>% count(fg4190)	
attributes(romantic$fg4190)

romantic %>% count(tc1210)	
attributes(romantic$tc1210)

romantic %>% count(FJPC1000)	
attributes(romantic$FJPC1000)

romantic %>% count(FJPC1150)	
attributes(romantic$FJPC1150)

#Romantic variable, but about partner's satisfaction with their relationship. Exclude. 
alspac %>% count(fa7119)	
attributes(alspac$fa7119)

romantic %>% count(FKFR1120)	
attributes(romantic$FKFR1120)

# Remove NAs and recode to binary variables
romantic <- romantic %>%
  mutate(
    romantic_in_relationship_or_out_with_someone_age_13y6m = case_when(fg4190 == 2 ~ 0, fg4190 == 1 ~ 1),
    romantic_in_relationship_age_16y6m = case_when(tc1210 == 3 ~ 0, tc1210 > 0 ~ 1),
    romantic_in_relationship_age_17y6m = case_when(FJPC1000 == 2 ~ 0, FJPC1000 == 1 ~ 1),
    romantic_emotionally_close_age_17y6m = case_when(FJPC1150 > 0 ~ FJPC1150),
    romantic_in_relationship_age_24y = case_when(FKFR1120 == -3 ~ 0, FKFR1120 == -4 | FKFR1120 > 0 ~ 1))

# Check derived variables
romantic %>% count(romantic_in_relationship_or_out_with_someone_age_13y6m, fg4190)
romantic %>% count(romantic_in_relationship_age_16y6m, tc1210)
romantic %>% count(romantic_in_relationship_age_17y6m, FJPC1000)
romantic %>% count(romantic_emotionally_close_age_17y6m, FJPC1150)
romantic %>% count(romantic_in_relationship_age_24y, FKFR1120)

################################################################################
#Identify variables around bond with parent
################################################################################

### Maternal bonding variables:

alspac %>% count(f117) #age 8 months
attributes(alspac$f117)

alspac %>% count(h766) #age 2 years 9 months
attributes(alspac$h766)

### Paternal bonding variables:

alspac %>% count(pd117) #age 8 months
attributes(alspac$pd117)

# No paternal bonding variable at PE. 
# Derived instead from enjoyment and confidence variables. 
# This matches derivation from PD. 

alspac %>% count(pe172) %>% print(n=40) #age 1 year 9 months
attributes(alspac$pe172)

alspac %>% count(pe175) %>% print(n=40)
attributes(alspac$pe175)

### Paternal bonding instrument: 

alspac %>% count(YPG2522) #age 27 years
attributes(alspac$YPG2522)

alspac %>% count(YPG2524) #age 27 years
attributes(alspac$YPG2524)

alspac %>% count(YPG2526) #age 27 years
attributes(alspac$YPG2526)

alspac %>% count(YPG2528) #age 27 years
attributes(alspac$YPG2528)


# Affection score (reported by mother about her partner)

attributes(alspac$d369) # 12 weeks gest
alspac %>% count(d369) %>% print(n = 40)

attributes(alspac$f593) # 8 months. affection score seems very different from pre-birth to 8 months.
alspac %>% count(f593) %>% print(n = 40)

# Affection score (reported by partner about mother)

attributes(alspac$pa369) # 8 months12 weeks gest
alspac %>% count(pa369) %>% print(n = 40)

attributes(alspac$pd593) # 8 months
alspac %>% count(pd593) %>% print(n = 40)

# Aggression score (reported by mother about her partner)

attributes(alspac$d372) # 12 weeks gest
alspac %>% count(d372) %>% print(n = 40)

attributes(alspac$f596) # 8 months
alspac %>% count(f596) %>% print(n = 40)

# Aggression score (reported by partner about mother)

attributes(alspac$pa372) # 12 weeks gestation
alspac %>% count(pa372) %>% print(n = 40)

attributes(alspac$pd596) # 8 months
alspac %>% count(pd596) %>% print(n = 40)

### Parent child relationship quality (PCRQ) measure 

alspac %>% select(ccf104, 	ccf111, 	ccf118, 	ccf125, 	ccf133, 	ccf141, 	ccf149, 	ccf157, 	ccf160, 	ccf165) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(ccf104, 	ccf111, 	ccf118, 	ccf125, 	ccf133, 	ccf141, 	ccf149, 	ccf157, 	ccf160, 	ccf165, 
))) {
  alspac %>% count(.data[[var]]) %>% print()
}

### Mother's satisfaction with partner
attributes(alspac$h572) # 8 months
alspac %>% count(h572) %>% print(n = 40)

mothers_satisfaction_with_partner <- c("l6130", 	"l6131", 	"l6132", 	"l6133", 	"l6134", 	"l6135", 	"l6136", 	"p3130", 	"p3131", 	"p3132", 	"p3133", 	"p3134", 	"p3135", 	"p3136", 	"s3130", 	"s3131", 	"s3132", 	"s3133", 	"s3134", 	"s3135", 	"s3136")

alspac %>% select(mothers_satisfaction_with_partner) %>% lapply(., attributes)
for (var in names(alspac %>% select(mothers_satisfaction_with_partner))) {alspac %>% count(.data[[var]]) %>% print()}
alspac %>% count(l6000, l6130) %>% print(n = 40)
alspac %>% count(p3000, p3130) %>% print(n = 40)
alspac %>% count(s3000, s3130) %>% print(n = 40)

### Warmth and authority scores (DV available in waves H, but not L, P, S, or T)
#Note - cannot derive variable at wave T, no variable for whether respondent has a partner. 
attributes(alspac$h629) # 2 years 9 months
alspac %>% count(h629) %>% print(n = 50)

attributes(alspac$h630) # 2 years 9 months
alspac %>% count(h630) %>% print(n = 50)

warmth_and_authority_scores <- c("l6200", 	"l6201", 	"l6202", 	"l6203", 	"l6204", 	"l6205", 	"l6206", 	"l6207", 	"l6208", 	"l6209", 	"l6210", 	"l6211", 	"l6212", 	"l6213", 	"l6214", 	"l6215", 	"l6216", 	"l6217", 	"l6218", 	"l6219", 	"l6220", 	"l6221", 	"l6222", 	"l6223", 	"p3200", 	"p3201", 	"p3202", 	"p3203", 	"p3204", 	"p3205", 	"p3206", 	"p3207", 	"p3208", 	"p3209", 	"p3210", 	"p3211", 	"p3212", 	"p3213", 	"p3214", 	"p3215", 	"p3216", 	"p3217", 	"p3218", 	"p3219", 	"p3220", 	"p3221", 	"p3222", 	"p3223", 	"s3200", 	"s3201", 	"s3202", 	"s3203", 	"s3204", 	"s3205", 	"s3206", 	"s3207", 	"s3208", 	"s3209", 	"s3210", 	"s3211", 	"s3212", 	"s3213", 	"s3214", 	"s3215", 	"s3216", 	"s3217", 	"s3218", 	"s3219", 	"s3220", 	"s3221", 	"s3222", 	"s3223", 	"t1100", 	"t1101", 	"t1102", 	"t1103", 	"t1104", 	"t1105", 	"t1106", 	"t1107", 	"t1108", 	"t1109", 	"t1110", 	"t1111", 	"t1112", 	"t1113", 	"t1114", 	"t1115", 	"t1116", 	"t1117", 	"t1118", 	"t1119", 	"t1120", 	"t1121", 	"t1122", 	"t1123")
alspac %>% select(warmth_and_authority_scores) %>% lapply(., attributes)
for (var in names(alspac %>% select(warmth_and_authority_scores))) {alspac %>% count(.data[[var]]) %>% print()}

### Parenting scores 

attributes(alspac$ke021)
alspac %>% count(ke021) %>% print(n=50) # Overall parenting score age 2y

attributes(alspac$kg240)
alspac %>% count(kg240) %>% print(n=50) # Partner parenting score - age 3y2m

attributes(alspac$kg260)
alspac %>% count(kg260) %>% print(n=50) # Mother parenting score - age 3y2m

attributes(alspac$kq597)
alspac %>% count(kq597) %>% print(n=73) # Female parenting score - age 6y9m

attributes(alspac$kq622)
alspac %>% count(kq622) %>% print(n=75) # Male parenting score - age 6y9m


# Partner parenting score - waves when not derived. 
parenting_score_item_var_names <- names(alspac %>% select(kd391, 	kd392, 	kd393, 	kd394, 	kd395, 	kd396, 	kd397, 	kd398, 	kd399, 	kd400, 	kd405, 	kd406, 	kd407, 	kd408, 	kd409, 	kd410, 	kd411, 	kd412, 	kd413, 	kd414, 	kb554, 	kb555, 	kb556, 	kb558, 	kb560, 	kb562, 	kb564, 	kb566, 	kb568, 	kb570, 	kb571, 	kb572, 	kb573, 	kb574, 	kb575, 	kb576, 	kb577, 	kb578, 	kb579, 	kb580, 	kq600, 	kq601, 	kq602, 	kq603, 	kq604, 	kq605, 	kq606, 	kq607, 	kq608, 	kq609, 	kq610, 	kq611))
alspac %>% select(parenting_score_item_var_names) %>% lapply(., attributes)
for (var in names(alspac %>% select(parenting_score_item_var_names))) {alspac %>% count(.data[[var]]) %>% print()}

# Don't use KB data, item response categories differ to later waves and questions are inconsistent. 
# Can be generated for KD. 
# Import KQ items as well for sense check. 
# Not derived yet. 

### Select variables
family <- alspac %>%
  select(uniqid, d369, f593, pa369, pd593, d372, pa372, f596, pd596, f117, h766, pd117, YPG2522, YPG2524, YPG2526, YPG2528, ccf104, 	ccf111, 	ccf118, 	ccf125, 	ccf133, 	ccf141, 	ccf149, 	ccf157, 	ccf160, 	ccf165, pe172, pe175, 
         h572, l6130, 	l6131, 	l6132, 	l6133, 	l6134, 	l6135, 	l6136, 	p3130, 	p3131, 	p3132, 	p3133, 	p3134, 	p3135, 	p3136, 	s3130, 	s3131, 	s3132, 	s3133, 	s3134, 	s3135, 	s3136, 
         l6000, p3000, s3000, 
         h629, h630,
         l6200, 	l6201, 	l6202, 	l6203, 	l6204, 	l6205, 	l6206, 	l6207, 	l6208, 	l6209, 	l6210, 	l6211, 	l6212, 	l6213, 	l6214, 	l6215, 	l6216, 	l6217, 	l6218, 	l6219, 	l6220, 	l6221, 	l6222, 	l6223, 	p3200, 	p3201, 	p3202, 	p3203, 	p3204, 	p3205, 	p3206, 	p3207, 	p3208, 	p3209, 	p3210, 	p3211, 	p3212, 	p3213, 	p3214, 	p3215, 	p3216, 	p3217, 	p3218, 	p3219, 	p3220, 	p3221, 	p3222, 	p3223, 	s3200, 	s3201, 	s3202, 	s3203, 	s3204, 	s3205, 	s3206, 	s3207, 	s3208, 	s3209, 	s3210, 	s3211, 	s3212, 	s3213, 	s3214, 	s3215, 	s3216, 	s3217, 	s3218, 	s3219, 	s3220, 	s3221, 	s3222, 	s3223, 
         ke021, kg260, kg240, kq622, kq597)

### Remove missing cases / rename variables
family <- family %>%
  mutate(
    family_maternal_bond_age_0y8m = case_when(f117 > 0 ~ f117),
    family_maternal_bond_age_2y9m = case_when(h766 > 0 ~ h766),
    family_paternal_bond_age_0y8m = case_when(pd117 > 0 ~ pd117),
    family_maternal_care_age_27y = case_when(YPG2522 > 0 ~ YPG2522),
    family_maternal_over_protect_age_27y = case_when(YPG2524 > 0 ~ YPG2524),
    family_paternal_care_age_27y = case_when(YPG2526 > 0 ~ YPG2526),
    family_paternal_over_protect_age_27y = case_when(YPG2528 > 0 ~ YPG2528),
    family_affection_of_partner_age_gest = case_when(d369 >= 0 ~ d369),
    family_affection_of_partner_age_0y8m = case_when(f593 >= 0 ~ f593),
    family_affection_of_mother_age_gest = case_when(pa369 >= 0 ~ pa369),
    family_affection_of_mother_age_0y8m = case_when(pd593 >= 0 ~ pd593),
    family_aggression_of_partner_age_gest = case_when(d372 >= 0 ~ d372),
    family_aggression_of_mother_age_gest = case_when(pa372 >= 0 ~ pa372),
    family_aggression_of_partner_age_0y8m = case_when(f596 >= 0 ~ f596),
    family_aggression_of_mother_age_0y8m = case_when(pd596 >= 0 ~ pd596),
    ccf104b = case_when(ccf104 >= 0 ~ ccf104),
    ccf111b = case_when(ccf111 >= 0 ~ ccf111),
    ccf118b = case_when(ccf118 >= 0 ~ ccf118),
    ccf125b = case_when(ccf125 >= 0 ~ ccf125),
    ccf133b = case_when(ccf133 >= 0 ~ ccf133),
    ccf141b = case_when(ccf141 >= 0 ~ ccf141),
    ccf149b = case_when(ccf149 >= 0 ~ ccf149),
    ccf157b = case_when(ccf157 >= 0 ~ ccf157),
    ccf160b = case_when(ccf160 >= 0 ~ ccf160),
    ccf165b = case_when(ccf165 >= 0 ~ ccf165),
    pe172b = case_when(pe172 >= 0 ~ pe172),
    pe175b = case_when(pe175 >= 0 ~ pe175), 
    family_paternal_bond_age_1y9m = pe172b + pe175b, 
    l6130b = case_when(l6130  >=  0 ~  4 - l6130),
    l6131b = case_when(l6131  >=  0 ~  4 - l6131),
    l6132b = case_when(l6132  >=  0 ~  4 - l6132),
    l6133b = case_when(l6133  >=  0 ~  4 - l6133),
    l6134b = case_when(l6134  >=  0 ~  4 - l6134),
    l6135b = case_when(l6135  >=  0 ~  4 - l6135),
    l6136b = case_when(l6136  >=  0 ~  4 - l6136),
    p3130b = case_when(p3130  >=  0 ~  4 - p3130),
    p3131b = case_when(p3131  >=  0 ~  4 - p3131),
    p3132b = case_when(p3132  >=  0 ~  4 - p3132),
    p3133b = case_when(p3133  >=  0 ~  4 - p3133),
    p3134b = case_when(p3134  >=  0 ~  4 - p3134),
    p3135b = case_when(p3135  >=  0 ~  4 - p3135),
    p3136b = case_when(p3136  >=  0 ~  4 - p3136),
    s3130b = case_when(s3130  >=  0 ~  4 - s3130),
    s3131b = case_when(s3131  >=  0 ~  4 - s3131),
    s3132b = case_when(s3132  >=  0 ~  4 - s3132),
    s3133b = case_when(s3133  >=  0 ~  4 - s3133),
    s3134b = case_when(s3134  >=  0 ~  4 - s3134),
    s3135b = case_when(s3135  >=  0 ~  4 - s3135),
    s3136b = case_when(s3136  >=  0 ~  4 - s3136),
    family_mother_satisfaction_with_partner_age_2y9m = case_when(h572 >= 0 | h572 == -2 ~ h572), 
    family_mother_satisfaction_with_partner_interim_age_6y1m = l6130b + 	l6131b + 	l6132b + 	l6133b + 	l6134b + 	l6135b + 	l6136b,
    family_mother_satisfaction_with_partner_interim_age_9y1m =  p3130b + 	p3131b + 	p3132b + 	p3133b + 	p3134b + 	p3135b + 	p3136b,
    family_mother_satisfaction_with_partner_interim_age_12y1m =  s3130b + 	s3131b + 	s3132b + 	s3133b + 	s3134b + 	s3135b + 	s3136b,
    family_mother_satisfaction_with_partner_age_6y1m = case_when(l6000 == 3 ~ -2, family_mother_satisfaction_with_partner_interim_age_6y1m >= 0 ~ family_mother_satisfaction_with_partner_interim_age_6y1m),
    family_mother_satisfaction_with_partner_age_9y1m = case_when(p3000 == 4 ~ -2, family_mother_satisfaction_with_partner_interim_age_9y1m >= 0 ~ family_mother_satisfaction_with_partner_interim_age_9y1m),
    family_mother_satisfaction_with_partner_age_12y1m = case_when(s3000 == 4 ~ -2, family_mother_satisfaction_with_partner_interim_age_12y1m >= 0 ~ family_mother_satisfaction_with_partner_interim_age_12y1m),
    l6200b = case_when(l6200 >= 0 ~ l6200),
    l6201b = case_when(l6201 >= 0 ~ l6201),
    l6202b = case_when(l6202 >= 0 ~ l6202),
    l6203b = case_when(l6203 >= 0 ~ l6203),
    l6204b = case_when(l6204 >= 0 ~ l6204),
    l6205b = case_when(l6205 >= 0 ~ l6205),
    l6206b = case_when(l6206 >= 0 ~ l6206),
    l6207b = case_when(l6207 >= 0 ~ l6207),
    l6208b = case_when(l6208 >= 0 ~ l6208),
    l6209b = case_when(l6209 >= 0 ~ l6209),
    l6210b = case_when(l6210 >= 0 ~ l6210),
    l6211b = case_when(l6211 >= 0 ~ l6211),
    l6212b = case_when(l6212 >= 0 ~ l6212),
    l6213b = case_when(l6213 >= 0 ~ l6213),
    l6214b = case_when(l6214 >= 0 ~ l6214),
    l6215b = case_when(l6215 >= 0 ~ l6215),
    l6216b = case_when(l6216 >= 0 ~ l6216),
    l6217b = case_when(l6217 >= 0 ~ l6217),
    l6218b = case_when(l6218 >= 0 ~ l6218),
    l6219b = case_when(l6219 >= 0 ~ l6219),
    l6220b = case_when(l6220 >= 0 ~ l6220),
    l6221b = case_when(l6221 >= 0 ~ l6221),
    l6222b = case_when(l6222 >= 0 ~ l6222),
    l6223b = case_when(l6223 >= 0 ~ l6223),
    p3200b = case_when(p3200 >= 0 ~ p3200),
    p3201b = case_when(p3201 >= 0 ~ p3201),
    p3202b = case_when(p3202 >= 0 ~ p3202),
    p3203b = case_when(p3203 >= 0 ~ p3203),
    p3204b = case_when(p3204 >= 0 ~ p3204),
    p3205b = case_when(p3205 >= 0 ~ p3205),
    p3206b = case_when(p3206 >= 0 ~ p3206),
    p3207b = case_when(p3207 >= 0 ~ p3207),
    p3208b = case_when(p3208 >= 0 ~ p3208),
    p3209b = case_when(p3209 >= 0 ~ p3209),
    p3210b = case_when(p3210 >= 0 ~ p3210),
    p3211b = case_when(p3211 >= 0 ~ p3211),
    p3212b = case_when(p3212 >= 0 ~ p3212),
    p3213b = case_when(p3213 >= 0 ~ p3213),
    p3214b = case_when(p3214 >= 0 ~ p3214),
    p3215b = case_when(p3215 >= 0 ~ p3215),
    p3216b = case_when(p3216 >= 0 ~ p3216),
    p3217b = case_when(p3217 >= 0 ~ p3217),
    p3218b = case_when(p3218 >= 0 ~ p3218),
    p3219b = case_when(p3219 >= 0 ~ p3219),
    p3220b = case_when(p3220 >= 0 ~ p3220),
    p3221b = case_when(p3221 >= 0 ~ p3221),
    p3222b = case_when(p3222 >= 0 ~ p3222),
    p3223b = case_when(p3223 >= 0 ~ p3223),
    s3200b = case_when(s3200 >= 0 ~ s3200),
    s3201b = case_when(s3201 >= 0 ~ s3201),
    s3202b = case_when(s3202 >= 0 ~ s3202),
    s3203b = case_when(s3203 >= 0 ~ s3203),
    s3204b = case_when(s3204 >= 0 ~ s3204),
    s3205b = case_when(s3205 >= 0 ~ s3205),
    s3206b = case_when(s3206 >= 0 ~ s3206),
    s3207b = case_when(s3207 >= 0 ~ s3207),
    s3208b = case_when(s3208 >= 0 ~ s3208),
    s3209b = case_when(s3209 >= 0 ~ s3209),
    s3210b = case_when(s3210 >= 0 ~ s3210),
    s3211b = case_when(s3211 >= 0 ~ s3211),
    s3212b = case_when(s3212 >= 0 ~ s3212),
    s3213b = case_when(s3213 >= 0 ~ s3213),
    s3214b = case_when(s3214 >= 0 ~ s3214),
    s3215b = case_when(s3215 >= 0 ~ s3215),
    s3216b = case_when(s3216 >= 0 ~ s3216),
    s3217b = case_when(s3217 >= 0 ~ s3217),
    s3218b = case_when(s3218 >= 0 ~ s3218),
    s3219b = case_when(s3219 >= 0 ~ s3219),
    s3220b = case_when(s3220 >= 0 ~ s3220),
    s3221b = case_when(s3221 >= 0 ~ s3221),
    s3222b = case_when(s3222 >= 0 ~ s3222),
    s3223b = case_when(s3223 >= 0 ~ s3223),
    family_warmth_of_partner_age_2y9m = case_when(h629 >= 0 ~ h629),
    family_warmth_of_partner_age_6y1m = l6200b + 	l6203b + 	l6204b + 	l6207b + 	l6209b + 	l6212b + 	l6213b + 	l6215b + 	l6217b + 	l6220b + 	l6222b + 	l6223b,
    family_warmth_of_partner_age_9y1m = p3200b + 	p3203b + 	p3204b + 	p3207b + 	p3209b + 	p3212b + 	p3213b + 	p3215b + 	p3217b + 	p3220b + 	p3222b + 	p3223b, 
    family_warmth_of_partner_age_12y1m = s3200b + 	s3203b + 	s3204b + 	s3207b + 	s3209b + 	s3212b + 	s3213b + 	s3215b + 	s3217b + 	s3220b + 	s3222b + 	s3223b,
    family_authority_of_partner_age_2y9m = case_when(h630 >= 0 ~ h630),
    family_authority_of_partner_age_6y1m = l6201b + 	l6202b + 	l6205b + 	l6206b + 	l6208b + 	l6210b + 	l6211b + 	l6214b + 	l6216b + 	l6218b + 	l6219b + 	l6221b,
    family_authority_of_partner_age_9y1m = p3201b + 	p3202b + 	p3205b + 	p3206b + 	p3208b + 	p3210b + 	p3211b + 	p3214b + 	p3216b + 	p3218b + 	p3219b + 	p3221b,
    family_authority_of_partner_age_12y1m = s3201b + 	s3202b + 	s3205b + 	s3206b + 	s3208b + 	s3210b + 	s3211b + 	s3214b + 	s3216b + 	s3218b + 	s3219b + 	s3221b
    )

family %>% select(family_authority_of_partner_age_12y1m, s3201b , 	s3202b , 	s3205b , 	s3206b , 	s3208b , 	s3210b , 	s3211b , 	s3214b , 	s3216b , 	s3218b , 	s3219b , 	s3221b)

#Add variable for parental relationship quality  
#Reverse two items: ccf111, ccf160. 
family <- family %>% mutate_at(vars(c("ccf104b",	"ccf111b",	"ccf118b",	"ccf125b",	"ccf133b",	"ccf141b",	"ccf149b",	"ccf157b",	"ccf160b",	"ccf165b")), as.numeric)
family <- family %>%  mutate(
  ccf111b = case_when(
    ccf111b == 1 ~ 5,
    ccf111b == 2 ~ 4,
    ccf111b == 3 ~ 3,
    ccf111b == 4 ~ 2,
    ccf111b == 5 ~ 1), 
  ccf160b = case_when(
    ccf160b == 1 ~ 5,
    ccf160b == 2 ~ 4,
    ccf160b == 3 ~ 3,
    ccf160b == 4 ~ 2,
    ccf160b == 5 ~ 1))
#Derive family parent child relationship quality score
family <- family %>% mutate(family_parent_child_rel_quality_9y7m = rowSums(across(c(ccf104b, 	ccf111b, 	ccf118b, 	ccf125b, 	ccf133b, 	ccf141b, 	ccf149b, 	ccf157b, 	ccf160b, 	ccf165b))))

family %>% count(family_mother_satisfaction_with_partner_age_2y9m, h572) %>% print(n=40)
family %>% count(l6000, family_mother_satisfaction_with_partner_age_6y1m) %>% print(n=103)
family %>% count(p3000, family_mother_satisfaction_with_partner_age_9y1m) %>% print(n=103)
family %>% count(s3000, family_mother_satisfaction_with_partner_age_12y1m) %>% print(n=103)
family %>% count(family_maternal_bond_age_0y8m, f117)
family %>% count(family_maternal_bond_age_2y9m, h766)
family %>% count(family_paternal_bond_age_0y8m, pd117)
family %>% count(family_paternal_bond_age_1y9m) %>% print(n=40)
family %>% count(family_maternal_care_age_27y, YPG2522)
family %>% count(family_maternal_over_protect_age_27y, YPG2524)
family %>% count(family_paternal_care_age_27y, YPG2526)
family %>% count(family_paternal_over_protect_age_27y, YPG2528)
family %>% count(family_affection_of_partner_age_gest, d369) %>% print(n = 45)
family %>% count(family_affection_of_partner_age_0y8m, f593) %>% print(n = 45)
family %>% count(family_affection_of_mother_age_gest, pa369) %>% print(n = 45)
family %>% count(family_affection_of_mother_age_0y8m, pd593) %>% print(n = 45)
family %>% count(family_aggression_of_partner_age_gest, d372) %>% print(n = 45)
family %>% count(family_aggression_of_mother_age_gest, pa372) %>% print(n = 45)
family %>% count(family_aggression_of_partner_age_0y8m, f596) %>% print(n = 45)
family %>% count(family_aggression_of_mother_age_0y8m, pd596) %>% print(n = 45)

family %>% count(family_parent_child_rel_quality_9y7m) %>% print(n = 50)
family %>% filter(
  family_parent_child_rel_quality_9y7m != ccf104b + ccf111b + ccf118b + ccf125b + ccf133b + ccf141b + ccf149b + ccf157b + ccf160b + ccf165b) %>%
  count()

###  Parenting score items

#Pre-derived items
family <- family %>%
  mutate(
    family_overall_parenting_score_age_2y = case_when(ke021 >= 0 ~ ke021),
    family_partner_parenting_score_age_3y2m = case_when(kg240 >= 0 ~ kg240),
    family_mother_parenting_score_age_3y2m = case_when(kg260 >= 0 ~ kg260),
    family_female_parenting_score_age_6y9m = case_when(kq597 >= 0 ~ kq597),
    family_male_parenting_score_age_6y9m = case_when(kq622 >= 0 ~ kq622))


## Individual items identified separately to primary domains: 

# Review items
selected_items <- names(alspac %>% select(CCXD808, 	CCXD811, 	e525a, 	e526a, 	e527a, 	f711, 	f712, 	f713, 	fa7119, 	fh8330, 	fh8331, 	fh8332, 	fh8333, 	FJPC2000, 	FJPC2050, 	g198, 	g630, 	g642, 	g643, 	j560, 	j570, 	kb534, 	kd319, 	kd436, 	kd447, 	kd465, 	kf390, 	kj284, 	kj330, 	kj410, 	kj430, 	kl356, 	kl452, 	km4362, 	kn3125, 	kn3150, 	kp2173, 	kp6074, 	kp6075, 	kp6200, 	kq330, 	kq570, 	kq652, 	kq653, 	kt3017, 	kt3157, 	ku301, 	ku302, 	ku305, 	ku308, 	ku535, 	ku647, 	ku690, 	kw6510, 	m3350, 	m3360, 	n8201, 	n8370, 	n8377, 	n8387, 	p3050, 	pg4160, 	pj6054, 	pk3350, 	pk3357, 	s3054, 	tb9521, 	tc1191, 	TE1510, kp2174, kp2175, kw9147)) 
for (var in names(alspac %>% select(selected_items))) {
  alspac %>% count(.data[[var]]) %>% print(n=40)
}

# Create separate dataset
individual_items <- alspac %>% select(uniqid, CCXD808, 	CCXD811, 	e525a, 	e526a, 	e527a, 	f711, 	f712, 	f713, 	fa7119, 	fh8330, 	fh8331, 	fh8332, 	fh8333, 	FJPC2000, 	FJPC2050, 	g198, 	g630, 	g642, 	g643, 	j560, 	j570, 	kb534, 	kd319, 	kd436, 	kd447, 	kd465, 	kf390, 	kj284, 	kj330, 	kj410, 	kj430, 	kl356, 	kl452, 	km4362, 	kn3125, 	kn3150, 	kp2173, 	kp6074, 	kp6075, 	kp6200, 	kq330, 	kq570, 	kq652, 	kq653, 	kt3017, 	kt3157, 	ku301, 	ku302, 	ku305, 	ku308, 	ku535, 	ku647, 	ku690, 	kw6510, 	m3350, 	m3360, 	n8201, 	n8370, 	n8377, 	n8387, 	p3050, 	pg4160, 	pj6054, 	pk3350, 	pk3357, 	s3054, 	tb9521, 	tc1191, 	TE1510, kp2174, kp2175, kw9147)

# Rename items and remove NAs
individual_items <- individual_items %>% mutate(
  social_support_frequency_feeling_close_to_people_age_17y6m = case_when(CCXD808 >= 0 ~ CCXD808),
  social_support_frequency_feeling_loved_age_17y6m = case_when(CCXD811 >= 0 ~ CCXD811),
  childcare_grandparent_looks_after_ch_age_0y2m = case_when(e525a >= 0 ~ e525a),
  childcare_other_rel_looks_after_ch_age_0y2m = case_when(e526a >= 0 ~ e526a),
  childcare_friend_or_neighbour_looks_after_ch_age_0y2m = case_when(e527a >= 0 ~ e527a),
  childcare_gdprt_looks_after_ch_age_0y8m = case_when(f711 >= 0 ~ f711),
  childcare_other_rel_looks_after_ch_age_0y8m = case_when(f712 >= 0 ~ f712),
  childcare_frd_or_neighbour_looks_after_ch_age_0y8m = case_when(f713 >= 0 ~ f713),
  friends_yps_friends_fall_out_with_them_age_15y6m = case_when(fh8330 >= 0 ~ fh8330),
  friends_friends_support_yp_when_they_need_them_age_15y6m = case_when(fh8331 >= 0 ~ fh8331),
  friends_friends_put_yp_down_in_front_of_others_age_15y6m = case_when(fh8332 >= 0 ~ fh8332),
  friends_friends_make_yp_feel_confident_age_15y6m = case_when(fh8333 >= 0 ~ fh8333),
  relationship_to_parents_how_close_to_parents_age_17y6m = case_when(FJPC2000 >= 0 & FJPC2000 < 9 ~ FJPC2000),
  family_how_close_yp_feels_to_their_siblings_age_17y6m = case_when(FJPC2050 >= 0 & FJPC2050 < 9 ~ FJPC2050),
  relationship_to_parents_mum_really_loves_toddler_age_1y9m = case_when(g198 >= 0 ~ g198),
  relationship_to_parents_partner_really_loves_child_age_1y9m = case_when(g630 >= 0 ~ g630),
  relationship_to_parents_partner_positive_relationship_score_age_1y9m = case_when(g642 >= 0 ~ g642),
  relationship_to_parents_partner_negative_relationship_score_age_1y9m = case_when(g643 >= 0 ~ g643),
  relationship_to_parents_mum_really_loves_ch_age_3y9m = case_when(j560 >= 0 ~ j560),
  relationship_to_parents_partner_really_loves_ch_age_3y9m = case_when(j570 >= 0 ~ j570),
  social_interaction_times_child_taken_to_friends_family_age_0y6m = case_when(kb534 >= 0 ~ kb534),
  social_interaction_frequency_child_visits_family_friends_age_1y6m = case_when(kd319 >= 0 ~ kd319),
  social_conflict_child_quarrels_with_older_children_age_1y6m = case_when(kd436 >= 0 ~ kd436),
  social_conflict_child_quarrels_with_twin_age_1y6m = case_when(kd447 >= 0 ~ kd447),
  social_interaction_child_affectionate_to_younger_sibs_age_1y6m = case_when(kd465 >= 0 ~ kd465),
  social_interaction_frequency_child_plays_with_children_not_sibs_age_2y6m = case_when(kf390 >= 0 ~ kf390),
  social_interaction_frequency_child_visits_friends_family_age_2y6m = case_when(kj284 >= 0 ~ kj284),
  social_interaction_frequency_child_plays_w_other_children_age_2y6m = case_when(kj330 >= 0 ~ kj330),
  relationship_to_parents_partner_interaction_score_age_2y6m = case_when(kj410 >= 0 ~ kj410),
  relationship_to_parents_mother_interaction_score_age_2y6m = case_when(kj430 >= 0 ~ kj430),
  social_interaction_child_plays_with_children_not_sibs_age_4y9m = case_when(kl356 >= 0 ~ kl356),
  childcare_frequency_child_sees_grandparents_age_4y9m = case_when(kl452 >= 0 ~ kl452),
  childcare_frequency_child_sees_grandparents_age_5y5m = case_when(km4362 >= 0 ~ km4362),
  childcare_frequency_child_visits_relatives_age_5y9m = case_when(kn3125 >= 0 ~ kn3125),
  social_interaction_frequency_child_plays_with_children_not_sibs_age_5y9m = case_when(kn3150 >= 0 ~ kn3150),
  social_conflict_mealtime_arguments_between_kids_age_6y5m = case_when(kp2173 >= 0 ~ kp2173),
  social_conflict_mealtime_arguments_between_kids_and_adults_age_6y5m = case_when(kp2174 >= 0 ~ kp2174),
  social_conflict_mealtime_arguments_between__adults_age_6y5m = case_when(kp2175 >= 0 ~ kp2175),
  social_interaction_frequency_child_visits_friends_age_6y5m = case_when(kp6074 >= 0 ~ kp6074),
  family_frequency_child_visits_relatives_age_6y5m = case_when(kp6075 >= 0 ~ kp6075),
  social_interaction_frq_child_plays_w_children_outside_school_age_6y5m = case_when(kp6200 >= 0 ~ kp6200),
  friends_ch_one_good_friend_past_6_mths_age_6y9m = case_when(kq330 >= 0 ~ kq330),
  childcare_frequency_child_sees_grandparents_age_6y9m = case_when(kq570 >= 0 ~ kq570),
  sibling_interaction_child_argues_with_siblings_age_6y9m = case_when(kq652 >= 0 ~ kq652),
  sibling_interaction_sibling_interaction_score_age_6y9m = case_when(kq653 >= 0 ~ kq653),
  family_child_sees_grandparents_age_8y7m = case_when(kt3017 >= 0 ~ kt3017),
  sibling_interaction_child_argues_with_siblings_age_8y7m = case_when(kt3157 >= 0 ~ kt3157),
  social_conflict_child_afraid_of_father_age_9y7m = case_when(ku301 >= 0 ~ ku301),
  social_conflict_child_afraid_of_mother_age_9y7m = case_when(ku302 >= 0 ~ ku302),
  social_conflict_child_afraid_of_other_relative_age_9y7m = case_when(ku305 >= 0 ~ ku305),
  social_conflict_child_afraid_of_other_children_age_9y7m = case_when(ku308 >= 0 ~ ku308),
  family_child_sees_his_or_her_grandparents_age_9y7m = case_when(ku535 >= 0 ~ ku535),
  sibling_interaction_child_argues_with_siblings_age_9y7m = case_when(ku647 >= 0 ~ ku647),
  social_network_child_has_one_good_friend_past_6_months_age_9y7m = case_when(ku690 >= 0 ~ ku690),
  friends_child_has_one_good_friend_last_six_months_age_11y8m = case_when(kw6510 >= 0 ~ kw6510),
  sibling_interaction_child_argues_with_siblings_age_11y8m = case_when(kw9147 >= 0 ~ kw9147),
  relationship_to_parents_mother_loves_study_child_age_7y1m = case_when(m3350 >= 0 ~ m3350),
  relationship_to_parents_partner_loves_study_child_age_7y1m = case_when(m3360 >= 0 ~ m3360),
  sibling_interaction_child_argues_with_older_child_age_8y1m = case_when(n8201 >= 0 ~ n8201),
  relationship_to_parents_mother_really_loves_child_age_8y1m = case_when(n8370 >= 0 ~ n8370),
  relationship_to_parents_mother_close_to_child_age_8y1m = case_when(n8377 >= 0 ~ n8377),
  relationship_to_parents_partner_close_to_child_age_8y1m = case_when(n8387 >= 0 ~ n8387),
  relationship_to_parents_partner_really_loves_child_age_9y1m = case_when(p3050 >= 0 ~ p3050),
  relationship_to_parents_partner_really_loves_child_age_3y9m = case_when(pg4160 >= 0 ~ pg4160),
  relationship_to_parents_partner_very_close_to_study_child_age_6y1m = case_when(pj6054 >= 0 ~ pj6054),
  relationship_to_parents_partner_really_loves_child_age_7y1m = case_when(pk3350 >= 0 ~ pk3350),
  relationship_to_parents_partner_very_close_child_age_7y1m = case_when(pk3357 >= 0 ~ pk3357),
  relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m = case_when(s3054 >= 0 ~ s3054),
  family_child_gets_on_well_with_rest_of_family_age_14y2m = case_when(tb9521 >= 0 ~ tb9521),
  friends_yp_has_a_best_friend_age_16y6m = case_when(tc1191 >= 0 ~ tc1191))

### Consolidate datasets ###

alspac_reduced <- left_join(friends, bullying, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, depression, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, anxiety, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, depression_smfq, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, neighbour, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, family, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, life_events, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, peer_problems, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, school_connection, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, parent_networks, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, parents_religion, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, romantic, by = "uniqid")
alspac_reduced <- left_join(alspac_reduced, individual_items, by = "uniqid")

# Check for duplicate variables
names(alspac_reduced %>% select(contains(".x")))
names(alspac_reduced %>% select(contains(".y")))

# Save out data
saveRDS(alspac_reduced, file = "alspac_reduced")

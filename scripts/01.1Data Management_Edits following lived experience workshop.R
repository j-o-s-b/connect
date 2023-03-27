
library(tidyverse)
alspac <- readRDS("alspac_reduced.rds")

###############################################################################
# Life events score: 
###############################################################################

# Agreed to compute alternative version of life score - looking at:

#  a) total number of life events experienced, 
#  b) total number of adverse life events exerpienced. 

names(alspac %>% select(contains("_age")))

# Dropped from most recent dataset, as we moved to looking at individual items

### Reinstate old life events scores. 

# Need to reimport some score variables, as these were dropped when we stopped looking at score. 
alspac_all <- readRDS("alspac.rds")
vars_to_add <- alspac_all %>% mutate(uniqid = paste0(cidB4083, qlet), collapse="") %>% select(uniqid, kd515, kf465, kj476, ccs2200, fh9140, pp2017, pp2018, r2017, r2018, f500, g590, n3000, a525, f460, g517, h386, j370, m3040, n8040, pl8040, q3040, t1050, kq348a, kq348b, kq348c, kq348d, kq348f, c645, c645a)
head(vars_to_add)
alspac <- left_join(alspac, vars_to_add, by = "uniqid")

# kj475 is a weighted life events score. This accounts for how upsetting the child found an event. 
# Other waves simply count number of events, so I have matched this. 

# Check existing life score at wave KD - fairly low level of missingness. 
attributes(alspac$kd515) # Age 1 year 6 months
alspac %>% count(kd515)
alspac %>%
  summarise(
    mean = mean(kd515 < 0 | is.na(kd515))*100,
    missing = sum(kd515 < 0 | is.na(kd515)))

# Check existing life score  - wave KF - still fairly low, but increasing. 
attributes(alspac$kf465) # Age 2 years 6 months
alspac %>% count(kf465)
alspac %>%
  summarise(
    mean = mean(kf465 < 0 | is.na(kf465))*100,
    missing = sum(kf465 < 0 | is.na(kf465)))

# kj475 is a weighted life events score. 
# This accounts for how upsetting the child found an event. 
# Other waves simply count number of events, so I have rederived this variable to match that approach. 
# Therefore, kj476 is not used. 
# Missingness has remained steady since wave KF. 

attributes(alspac$kj476) # Age 3 years 6 months
alspac %>% count(kj476)
alspac %>%
  summarise(
    mean = mean(kj476 < 0 | is.na(kj476))*100,
    missing = sum(kj476 < 0 | is.na(kj476)))

# No derived score at KL, KN, KT, CCS,YPB or YPC timepoints, so imported individual questions. 

# WAVE KL
alspac %>% select(kl470, kl471, kl472, kl473, kl474, kl475, kl476, kl477, kl478, kl479, kl480, kl481, kl482, kl483, kl484, kl485) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(kl470, kl471, kl472, kl473, kl474, kl475, kl476, kl477, kl478, kl479, kl480, kl481, kl482, kl483, kl484, kl485))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# WAVE KN
alspac %>% select(kn4000, 	kn4001, 	kn4002, 	kn4003, 	kn4004, 	kn4005, 	kn4006, 	kn4007, 	kn4008, 	kn4009, 	kn4010, 	kn4011, 	kn4012, 	kn4013, 	kn4014, 	kn4015) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(kn4000, 	kn4001, 	kn4002, 	kn4003, 	kn4004, 	kn4005, 	kn4006, 	kn4007, 	kn4008, 	kn4009, 	kn4010, 	kn4011, 	kn4012, 	kn4013, 	kn4014, 	kn4015))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# WAVE KT
alspac %>% select(kt5000, 	kt5001, 	kt5002, 	kt5003, 	kt5004, 	kt5005, 	kt5006, 	kt5007, 	kt5008, 	kt5009, 	kt5010, 	kt5011, 	kt5012, 	kt5013, 	kt5014, 	kt5015, 	kt5016) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(kt5000, 	kt5001, 	kt5002, 	kt5003, 	kt5004, 	kt5005, 	kt5006, 	kt5007, 	kt5008, 	kt5009, 	kt5010, 	kt5011, 	kt5012, 	kt5013, 	kt5014, 	kt5015, 	kt5016))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# WAVE CCS
alspac %>% select(ccs2000, 	ccs2010, 	ccs2020, 	ccs2030, 	ccs2040, 	ccs2050, 	ccs2060, 	ccs2070, 	ccs2080, 	ccs2090, 	ccs2100, 	ccs2110, 	ccs2120, 	ccs2130, 	ccs2140, 	ccs2150, 	ccs2160, 	ccs2170, 	ccs2180, 	ccs2190, 	ccs2200, 	ccs2210, 	ccs2220) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(ccs2000, 	ccs2010, 	ccs2020, 	ccs2030, 	ccs2040, 	ccs2050, 	ccs2060, 	ccs2070, 	ccs2080, 	ccs2090, 	ccs2100, 	ccs2110, 	ccs2120, 	ccs2130, 	ccs2140, 	ccs2150, 	ccs2160, 	ccs2170, 	ccs2180, 	ccs2190, 	ccs2200, 	ccs2210, 	ccs2220))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# WAVE YPB
alspac %>% select(YPB6000, 	YPB6010, 	YPB6020, 	YPB6030, 	YPB6040, 	YPB6050, 	YPB6060, 	YPB6070, 	YPB6080, 	YPB6090, 	YPB6100, 	YPB6110, 	YPB6120, 	YPB6130, 	YPB6140, 	YPB6150, 	YPB6160, 	YPB6170, 	YPB6180, 	YPB6190, 	YPB6210, 	YPB6220, 	YPB6230) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(YPB6000, 	YPB6010, 	YPB6020, 	YPB6030, 	YPB6040, 	YPB6050, 	YPB6060, 	YPB6070, 	YPB6080, 	YPB6090, 	YPB6100, 	YPB6110, 	YPB6120, 	YPB6130, 	YPB6140, 	YPB6150, 	YPB6160, 	YPB6170, 	YPB6180, 	YPB6190, 	YPB6210, 	YPB6220, 	YPB6230))) {
  alspac %>% count(.data[[var]]) %>% print()
}

# WAVE YPC
# In this wave zero is a valid value, where in other waves it is missing. 
# Recode 0 to 5 before removing missing values, so zero value cases are not recoded to missing.  
alspac %>% select(YPC2150, 	YPC2160, 	YPC2170, 	YPC2180, 	YPC2190, 	YPC2200, 	YPC2210, 	YPC2220, 	YPC2230, 	YPC2240, 	YPC2250, 	YPC2260, 	YPC2270, 	YPC2280, 	YPC2290, 	YPC2300, 	YPC2310, 	YPC2320, 	YPC2330, 	YPC2340, 	YPC2350, 	YPC2360, 	YPC2380, 	YPC2390, 	YPC2400, 	YPC2410) %>%
  lapply(., attributes)
for (var in names(alspac %>% select(YPC2150, 	YPC2160, 	YPC2170, 	YPC2180, 	YPC2190, 	YPC2200, 	YPC2210, 	YPC2220, 	YPC2230, 	YPC2240, 	YPC2250, 	YPC2260, 	YPC2270, 	YPC2280, 	YPC2290, 	YPC2300, 	YPC2310, 	YPC2320, 	YPC2330, 	YPC2340, 	YPC2350, 	YPC2360, 	YPC2380, 	YPC2390, 	YPC2400, 	YPC2410))) {
  alspac %>% count(.data[[var]]) %>% print()
}

### Select variables for life score sub dataset
life_score <- alspac %>% select(uniqid, kd515, kf465, kj476,
                                kl470, kl471, kl472, kl473, kl474, kl475, kl476, kl477, kl478, kl479, kl480, kl481, kl482, kl483, kl484, kl485,
                                kn4000, 	kn4001, 	kn4002, 	kn4003, 	kn4004, 	kn4005, 	kn4006, 	kn4007, 	kn4008, 	kn4009, 	kn4010, 	kn4011, 	kn4012, 	kn4013, 	kn4014, 	kn4015,
                                kt5000, 	kt5001, 	kt5002, 	kt5003, 	kt5004, 	kt5005, 	kt5006, 	kt5007, 	kt5008, 	kt5009, 	kt5010, 	kt5011, 	kt5012, 	kt5013, 	kt5014, 	kt5015, 	kt5016,
                                ccs2000, 	ccs2010, 	ccs2020, 	ccs2030, 	ccs2040, 	ccs2050, 	ccs2060, 	ccs2070, 	ccs2080, 	ccs2090, 	ccs2100, 	ccs2110, 	ccs2120, 	ccs2130, 	ccs2140, 	ccs2150, 	ccs2160, 	ccs2170, 	ccs2180, 	ccs2190, 	ccs2200, 	ccs2210, 	ccs2220, 
                                YPB6000, 	YPB6010, 	YPB6020, 	YPB6030, 	YPB6040, 	YPB6050, 	YPB6060, 	YPB6070, 	YPB6080, 	YPB6090, 	YPB6100, 	YPB6110, 	YPB6120, 	YPB6130, 	YPB6140, 	YPB6150, 	YPB6160, 	YPB6170, 	YPB6180, 	YPB6190, 	YPB6210, 	YPB6220, 	YPB6230, 
                                YPC2150, 	YPC2160, 	YPC2170, 	YPC2180, 	YPC2190, 	YPC2200, 	YPC2210, 	YPC2220, 	YPC2230, 	YPC2240, 	YPC2250, 	YPC2260, 	YPC2270, 	YPC2280, 	YPC2290, 	YPC2300, 	YPC2310, 	YPC2320, 	YPC2330, 	YPC2340, 	YPC2350, 	YPC2360, 	YPC2380, 	YPC2390, 	YPC2400, 	YPC2410)

# Identify number of items in life score at each wave: 
var_names <- data.frame(var_names = names(life_score))
var_names %>% filter(grepl('kl', var_names)) %>% as_vector %>% length()
var_names %>% filter(grepl('kn', var_names)) %>% as_vector %>% length()
var_names %>% filter(grepl('kt', var_names)) %>% as_vector %>% length()
var_names %>% filter(grepl('ccs', var_names)) %>% as_vector %>% length()
var_names %>% filter(grepl('YPB', var_names)) %>% as_vector %>% length()
var_names %>% filter(grepl('YPC', var_names)) %>% as_vector %>% length()


#YPC data, recode cases equal to zero to 5, other values to 1. 
life_score <- life_score %>%
  mutate(
    YPC2150b = case_when(YPC2150 == 1 | YPC2150 == 2 | YPC2150 ==3 |YPC2150  == 4 ~ 1, YPC2150 == 0 ~ 5),
    YPC2160b = case_when(YPC2160 == 1 | YPC2160 == 2 | YPC2160 ==3 |YPC2160  == 4 ~ 1, YPC2160 == 0 ~ 5),
    YPC2170b = case_when(YPC2170 == 1 | YPC2170 == 2 | YPC2170 ==3 |YPC2170  == 4 ~ 1, YPC2170 == 0 ~ 5),
    YPC2180b = case_when(YPC2180 == 1 | YPC2180 == 2 | YPC2180 ==3 |YPC2180  == 4 ~ 1, YPC2180 == 0 ~ 5),
    YPC2190b = case_when(YPC2190 == 1 | YPC2190 == 2 | YPC2190 ==3 |YPC2190  == 4 ~ 1, YPC2190 == 0 ~ 5),
    YPC2200b = case_when(YPC2200 == 1 | YPC2200 == 2 | YPC2200 ==3 |YPC2200  == 4 ~ 1, YPC2200 == 0 ~ 5),
    YPC2210b = case_when(YPC2210 == 1 | YPC2210 == 2 | YPC2210 ==3 |YPC2210  == 4 ~ 1, YPC2210 == 0 ~ 5),
    YPC2220b = case_when(YPC2220 == 1 | YPC2220 == 2 | YPC2220 ==3 |YPC2220  == 4 ~ 1, YPC2220 == 0 ~ 5),
    YPC2230b = case_when(YPC2230 == 1 | YPC2230 == 2 | YPC2230 ==3 |YPC2230  == 4 ~ 1, YPC2230 == 0 ~ 5),
    YPC2240b = case_when(YPC2240 == 1 | YPC2240 == 2 | YPC2240 ==3 |YPC2240  == 4 ~ 1, YPC2240 == 0 ~ 5),
    YPC2250b = case_when(YPC2250 == 1 | YPC2250 == 2 | YPC2250 ==3 |YPC2250  == 4 ~ 1, YPC2250 == 0 ~ 5),
    YPC2260b = case_when(YPC2260 == 1 | YPC2260 == 2 | YPC2260 ==3 |YPC2260  == 4 ~ 1, YPC2260 == 0 ~ 5),
    YPC2270b = case_when(YPC2270 == 1 | YPC2270 == 2 | YPC2270 ==3 |YPC2270  == 4 ~ 1, YPC2270 == 0 ~ 5),
    YPC2280b = case_when(YPC2280 == 1 | YPC2280 == 2 | YPC2280 ==3 |YPC2280  == 4 ~ 1, YPC2280 == 0 ~ 5),
    YPC2290b = case_when(YPC2290 == 1 | YPC2290 == 2 | YPC2290 ==3 |YPC2290  == 4 ~ 1, YPC2290 == 0 ~ 5),
    YPC2300b = case_when(YPC2300 == 1 | YPC2300 == 2 | YPC2300 ==3 |YPC2300  == 4 ~ 1, YPC2300 == 0 ~ 5),
    YPC2310b = case_when(YPC2310 == 1 | YPC2310 == 2 | YPC2310 ==3 |YPC2310  == 4 ~ 1, YPC2310 == 0 ~ 5),
    YPC2320b = case_when(YPC2320 == 1 | YPC2320 == 2 | YPC2320 ==3 |YPC2320  == 4 ~ 1, YPC2320 == 0 ~ 5),
    YPC2330b = case_when(YPC2330 == 1 | YPC2330 == 2 | YPC2330 ==3 |YPC2330  == 4 ~ 1, YPC2330 == 0 ~ 5),
    YPC2340b = case_when(YPC2340 == 1 | YPC2340 == 2 | YPC2340 ==3 |YPC2340  == 4 ~ 1, YPC2340 == 0 ~ 5),
    YPC2350b = case_when(YPC2350 == 1 | YPC2350 == 2 | YPC2350 ==3 |YPC2350  == 4 ~ 1, YPC2350 == 0 ~ 5),
    YPC2360b = case_when(YPC2360 == 1 | YPC2360 == 2 | YPC2360 ==3 |YPC2360  == 4 ~ 1, YPC2360 == 0 ~ 5),
    YPC2380b = case_when(YPC2380 == 1 | YPC2380 == 2 | YPC2380 ==3 |YPC2380  == 4 ~ 1, YPC2380 == 0 ~ 5),
    YPC2390b = case_when(YPC2390 == 1 | YPC2390 == 2 | YPC2390 ==3 |YPC2390  == 4 ~ 1, YPC2390 == 0 ~ 5),
    YPC2400b = case_when(YPC2400 == 1 | YPC2400 == 2 | YPC2400 ==3 |YPC2400  == 4 ~ 1, YPC2400 == 0 ~ 5),
    YPC2410b = case_when(YPC2410 == 1 | YPC2410 == 2 | YPC2410 ==3 |YPC2410  == 4 ~ 1, YPC2410 == 0 ~ 5))

life_score <- life_score %>%
  mutate(
    kd515 = case_when(kd515 != 0 ~ kd515, kd515 == 0 ~ -500), 
    kf465 = case_when(kf465 != 0 ~ kf465, kf465 == 0 ~ -500), 
    kj476 = case_when(kj476 != 0 ~ kj476, kj476 == 0 ~ -500),
    kn4015 = case_when(kn4015 != -1 ~kn4015, kn4014==-1 ~ -10, kn4015== - 1 ~ 5))
life_score %>% count(kd515)
life_score %>% count(kf465)
life_score %>% count(kj476)
#Edit made to kn4015, because the questions were from the same bank, bt -1 (no response) was much higher. 
#Given this I kept those where kn4014 was 'no repsonse' as 'no response', and moved the rest to 'did not happen'. 
life_score %>% count(kn4015)

life_score <- na_if(life_score, -9999)
life_score <- na_if(life_score, -10)
life_score <- na_if(life_score, -1)
life_score <- na_if(life_score, -9)
life_score <- na_if(life_score, -8)
life_score <- na_if(life_score, 0)

life_score <- life_score %>%
  mutate(
    kd515 = case_when(kd515 != -500 ~ kd515, kd515 == -500 ~ 0), 
    kf465 = case_when(kf465 != -500 ~ kf465, kf465 == -500 ~ 0), 
    kj476 = case_when(kj476 != -500 ~ kj476, kj476 == -500 ~ 0))
life_score %>% count(kd515)
life_score %>% count(kf465)
life_score %>% count(kj476)

###############################################################################
#Remove NA from existing life score variables
life_score <- life_score %>%
  mutate(
    life_score_since_6m_age_1y6m = case_when(kd515 >= 0 ~ kd515),
    life_score_since_18m_age_2y6m = case_when(kf465 >= 0 ~ kf465),
    life_score_since_30m_age_3y6m = case_when(kj476 >= 0 ~ kj476))

life_score %>% count(life_score_since_6m_age_1y6m)

###############################################################################
#Calculate life experience score age 57 months
for (var in names(life_score %>% select(kl470, kl471, kl472, kl473, kl474, kl475, kl476, kl477, kl478, kl479, kl480, kl481, kl482, kl483, kl484, kl485))) {
  life_score %>% count(.data[[var]]) %>% print()
}

#Exclude the 'other' event variable [kl485], much higher level of missingness than in the other question items. 

life_score <- life_score %>%
  mutate(
    kl470b = case_when(kl470 < 5 ~ 1, kl470 == 5~0),
    kl471b = case_when(kl471 < 5 ~ 1, kl471 == 5~0),
    kl472b = case_when(kl472 < 5 ~ 1, kl472 == 5~0),
    kl473b = case_when(kl473 < 5 ~ 1, kl473 == 5~0),
    kl474b = case_when(kl474 < 5 ~ 1, kl474 == 5~0),
    kl475b = case_when(kl475 < 5 ~ 1, kl475 == 5~0),
    kl476b = case_when(kl476 < 5 ~ 1, kl476 == 5~0),
    kl477b = case_when(kl477 < 5 ~ 1, kl477 == 5~0),
    kl478b = case_when(kl478 < 5 ~ 1, kl478 == 5~0),
    kl479b = case_when(kl479 < 5 ~ 1, kl479 == 5~0),
    kl480b = case_when(kl480 < 5 ~ 1, kl480 == 5~0),
    kl481b = case_when(kl481 < 5 ~ 1, kl481 == 5~0),
    kl482b = case_when(kl482 < 5 ~ 1, kl482 == 5~0),
    kl483b = case_when(kl483 < 5 ~ 1, kl483 == 5~0),
    kl484b = case_when(kl484 < 5 ~ 1, kl484 == 5~0),
    kl485b = case_when(kl485 < 5 ~ 1, kl485 == 5~0))
life_score <- life_score %>% mutate(life_score_since_3y_age_4y9m = rowSums(across(c(kl470b, 	kl471b, 	kl472b, 	kl473b, 	kl474b, 	kl475b, 	kl476b, 	kl477b, 	kl478b, 	kl479b, 	kl480b, 	kl481b, 	kl482b, 	kl483b, 	kl484b))))


###############################################################################
#Calculate life experience score age 6 years 9 months
for (var in names(life_score %>% select(kn4000, 	kn4001, 	kn4002, 	kn4003, 	kn4004, 	kn4005, 	kn4006, 	kn4007, 	kn4008, 	kn4009, 	kn4010, 	kn4011, 	kn4012, 	kn4013, 	kn4014, 	kn4015))) {
  life_score %>% count(.data[[var]]) %>% print()
}
#Again, excluding 'other' (kn4015) variable, higher level of missingness. 

life_score <- life_score %>%
  mutate(
    kn4000b = case_when(kn4000 < 5 ~ 1, kn4000 == 5~0),
    kn4001b = case_when(kn4001 < 5 ~ 1, kn4001 == 5~0),
    kn4002b = case_when(kn4002 < 5 ~ 1, kn4002 == 5~0),
    kn4003b = case_when(kn4003 < 5 ~ 1, kn4003 == 5~0),
    kn4004b = case_when(kn4004 < 5 ~ 1, kn4004 == 5~0),
    kn4005b = case_when(kn4005 < 5 ~ 1, kn4005 == 5~0),
    kn4006b = case_when(kn4006 < 5 ~ 1, kn4006 == 5~0),
    kn4007b = case_when(kn4007 < 5 ~ 1, kn4007 == 5~0),
    kn4008b = case_when(kn4008 < 5 ~ 1, kn4008 == 5~0),
    kn4009b = case_when(kn4009 < 5 ~ 1, kn4009 == 5~0),
    kn4010b = case_when(kn4010 < 5 ~ 1, kn4010 == 5~0),
    kn4011b = case_when(kn4011 < 5 ~ 1, kn4011 == 5~0),
    kn4012b = case_when(kn4012 < 5 ~ 1, kn4012 == 5~0),
    kn4013b = case_when(kn4013 < 5 ~ 1, kn4013 == 5~0),
    kn4014b = case_when(kn4014 < 5 ~ 1, kn4014 == 5~0),
    kn4015b = case_when(kn4015 < 5 ~ 1, kn4015 == 5~0))
life_score <- life_score %>% mutate(life_score_since_5y_age_5y9m = rowSums(across(c(kn4000b, 	kn4001b, 	kn4002b, 	kn4003b, 	kn4004b, 	kn4005b, 	kn4006b, 	kn4007b, 	kn4008b, 	kn4009b, 	kn4010b, 	kn4011b, 	kn4012b, 	kn4013b, 	kn4014b, 	kn4015b))))

###############################################################################
#Calculate life experience score age 8 years 7 months
for (var in names(life_score %>% select(kt5000, 	kt5001, 	kt5002, 	kt5003, 	kt5004, 	kt5005, 	kt5006, 	kt5007, 	kt5008, 	kt5009, 	kt5010, 	kt5011, 	kt5012, 	kt5013, 	kt5014, 	kt5015, 	kt5016))) {
  life_score %>% count(.data[[var]]) %>% print()
}
#Again, excluding 'other' (kt5016) variable, higher level of missingness. 

#One additional item in KT dataset than previous ones - whether YP lost their best friend or not. 
life_score <- life_score %>%
  mutate(
    kt5000b = case_when(kt5000 < 5 ~ 1, kt5000 == 5~0),
    kt5001b = case_when(kt5001 < 5 ~ 1, kt5001 == 5~0),
    kt5002b = case_when(kt5002 < 5 ~ 1, kt5002 == 5~0),
    kt5003b = case_when(kt5003 < 5 ~ 1, kt5003 == 5~0),
    kt5004b = case_when(kt5004 < 5 ~ 1, kt5004 == 5~0),
    kt5005b = case_when(kt5005 < 5 ~ 1, kt5005 == 5~0),
    kt5006b = case_when(kt5006 < 5 ~ 1, kt5006 == 5~0),
    kt5007b = case_when(kt5007 < 5 ~ 1, kt5007 == 5~0),
    kt5008b = case_when(kt5008 < 5 ~ 1, kt5008 == 5~0),
    kt5009b = case_when(kt5009 < 5 ~ 1, kt5009 == 5~0),
    kt5010b = case_when(kt5010 < 5 ~ 1, kt5010 == 5~0),
    kt5011b = case_when(kt5011 < 5 ~ 1, kt5011 == 5~0),
    kt5012b = case_when(kt5012 < 5 ~ 1, kt5012 == 5~0),
    kt5013b = case_when(kt5013 < 5 ~ 1, kt5013 == 5~0),
    kt5014b = case_when(kt5014 < 5 ~ 1, kt5014 == 5~0),
    kt5015b = case_when(kt5015 < 5 ~ 1, kt5015 == 5~0))
#Note kt5015 is a new question, not asked at previous wave, so excluded from life score for consistency. 
life_score <- life_score %>% mutate(life_score_since_7y_age_8y7m = rowSums(across(c(kt5000b, 	kt5001b, 	kt5002b, 	kt5003b, 	kt5004b, 	kt5005b, 	kt5006b, 	kt5007b, 	kt5008b, 	kt5009b, 	kt5010b, 	kt5011b, 	kt5012b, 	kt5013b, 	kt5014b))))

###############################################################################
#Young people life score - earlier versions based on parent's report. 
###############################################################################


###############################################################################
#Calculate life experience score age 16 years 6 months
for (var in names(life_score %>% select(ccs2000, 	ccs2010, 	ccs2020, 	ccs2030, 	ccs2040, 	ccs2050, 	ccs2060, 	ccs2070, 	ccs2080, 	ccs2090, 	ccs2100, 	ccs2110, 	ccs2120, 	ccs2130, 	ccs2140, 	ccs2150, 	ccs2160, 	ccs2170, 	ccs2180, 	ccs2190, 	ccs2200, 	ccs2210, 	ccs2220))) {
  life_score %>% count(.data[[var]]) %>% print()
}

#Item number
length(names(alspac %>% select(ccs2000, 	ccs2010, 	ccs2020, 	ccs2030, 	ccs2040, 	ccs2050, 	ccs2060, 	ccs2070, 	ccs2080, 	ccs2090, 	ccs2100, 	ccs2110, 	ccs2120, 	ccs2130, 	ccs2140, 	ccs2150, 	ccs2160, 	ccs2170, 	ccs2180, 	ccs2190, 	ccs2200, 	ccs2210, 	ccs2220)))
#Note: there are 23 items here, compared to 16 in earlier versions, so life score will be higher. 
#Marked variable as inconsistent. 

life_score <- life_score %>%
  mutate(
    ccs2000b = case_when(ccs2000 == 1 ~ 1, ccs2000 == 2 ~ 0),
    ccs2010b = case_when(ccs2010 == 1 ~ 1, ccs2010 == 2 ~ 0),
    ccs2020b = case_when(ccs2020 == 1 ~ 1, ccs2020 == 2 ~ 0),
    ccs2030b = case_when(ccs2030 == 1 ~ 1, ccs2030 == 2 ~ 0),
    ccs2040b = case_when(ccs2040 == 1 ~ 1, ccs2040 == 2 ~ 0),
    ccs2050b = case_when(ccs2050 == 1 ~ 1, ccs2050 == 2 ~ 0),
    ccs2060b = case_when(ccs2060 == 1 ~ 1, ccs2060 == 2 ~ 0),
    ccs2070b = case_when(ccs2070 == 1 ~ 1, ccs2070 == 2 ~ 0),
    ccs2080b = case_when(ccs2080 == 1 ~ 1, ccs2080 == 2 ~ 0),
    ccs2090b = case_when(ccs2090 == 1 ~ 1, ccs2090 == 2 ~ 0),
    ccs2100b = case_when(ccs2100 == 1 ~ 1, ccs2100 == 2 ~ 0),
    ccs2110b = case_when(ccs2110 == 1 ~ 1, ccs2110 == 2 ~ 0),
    ccs2120b = case_when(ccs2120 == 1 ~ 1, ccs2120 == 2 ~ 0),
    ccs2130b = case_when(ccs2130 == 1 ~ 1, ccs2130 == 2 ~ 0),
    ccs2140b = case_when(ccs2140 == 1 ~ 1, ccs2140 == 2 ~ 0),
    ccs2150b = case_when(ccs2150 == 1 ~ 1, ccs2150 == 2 ~ 0),
    ccs2160b = case_when(ccs2160 == 1 ~ 1, ccs2160 == 2 ~ 0),
    ccs2170b = case_when(ccs2170 == 1 ~ 1, ccs2170 == 2 ~ 0),
    ccs2180b = case_when(ccs2180 == 1 ~ 1, ccs2180 == 2 ~ 0),
    ccs2190b = case_when(ccs2190 == 1 ~ 1, ccs2190 == 2 ~ 0),
    ccs2200b = case_when(ccs2200 == 1 ~ 1, ccs2200 == 2 ~ 0),
    ccs2210b = case_when(ccs2210 == 1 ~ 1, ccs2210 == 2 ~ 0),
    ccs2220b = case_when(ccs2220 == 1 ~ 1, ccs2220 == 2 ~ 0))

for (var in names(life_score %>% select(ccs2000b, 	ccs2010b, 	ccs2020b, 	ccs2030b, 	ccs2040b, 	ccs2050b, 	ccs2060b, 	ccs2070b, 	ccs2080b, 	ccs2090b, 	ccs2100b, 	ccs2110b, 	ccs2120b, 	ccs2130b, 	ccs2140b, 	ccs2150b, 	ccs2160b, 	ccs2170b, 	ccs2180b, 	ccs2190b, 	ccs2200b, 	ccs2210b, 	ccs2220b))) {
  life_score %>% count(.data[[var]]) %>% print()
}

life_score <- life_score %>% mutate(life_score_inconsistent_since_12y_age_16y6m = rowSums(across(c(ccs2000b, 	ccs2010b, 	ccs2020b, 	ccs2030b, 	ccs2040b, 	ccs2050b, 	ccs2060b, 	ccs2070b, 	ccs2080b, 	ccs2090b, 	ccs2100b, 	ccs2110b, 	ccs2120b, 	ccs2130b, 	ccs2140b, 	ccs2150b, 	ccs2160b, 	ccs2170b, 	ccs2180b, 	ccs2190b, 	ccs2200b, 	ccs2210b, 	ccs2220b))))
life_score %>% count(life_score_inconsistent_since_12y_age_16y6m)

###############################################################################
#Calculate life experience score age 22 years 

# Note: item YPB6200 missing from data. Indicates presence of suicide attempt. 
# I think we agreed not to include due to sensitivity. 
for (var in names(life_score %>% select(YPB6000, 	YPB6010, 	YPB6020, 	YPB6030, 	YPB6040, 	YPB6050, 	YPB6060, 	YPB6070, 	YPB6080, 	YPB6090, 	YPB6100, 	YPB6110, 	YPB6120, 	YPB6130, 	YPB6140, 	YPB6150, 	YPB6160, 	YPB6170, 	YPB6180, 	YPB6190, 	YPB6210, 	YPB6220, 	YPB6230))) {
  life_score %>% count(.data[[var]]) %>% print()
}

life_score <- life_score %>%
  mutate(
    YPB6000b = case_when(YPB6000 < 5 ~ 1, YPB6000 == 5 ~ 0),
    YPB6010b = case_when(YPB6010 < 5 ~ 1, YPB6010 == 5 ~ 0),
    YPB6020b = case_when(YPB6020 < 5 ~ 1, YPB6020 == 5 ~ 0),
    YPB6030b = case_when(YPB6030 < 5 ~ 1, YPB6030 == 5 ~ 0),
    YPB6040b = case_when(YPB6040 < 5 ~ 1, YPB6040 == 5 ~ 0),
    YPB6050b = case_when(YPB6050 < 5 ~ 1, YPB6050 == 5 ~ 0),
    YPB6060b = case_when(YPB6060 < 5 ~ 1, YPB6060 == 5 ~ 0),
    YPB6070b = case_when(YPB6070 < 5 ~ 1, YPB6070 == 5 ~ 0),
    YPB6080b = case_when(YPB6080 < 5 ~ 1, YPB6080 == 5 ~ 0),
    YPB6090b = case_when(YPB6090 < 5 ~ 1, YPB6090 == 5 ~ 0),
    YPB6100b = case_when(YPB6100 < 5 ~ 1, YPB6100 == 5 ~ 0),
    YPB6110b = case_when(YPB6110 < 5 ~ 1, YPB6110 == 5 ~ 0),
    YPB6120b = case_when(YPB6120 < 5 ~ 1, YPB6120 == 5 ~ 0),
    YPB6130b = case_when(YPB6130 < 5 ~ 1, YPB6130 == 5 ~ 0),
    YPB6140b = case_when(YPB6140 < 5 ~ 1, YPB6140 == 5 ~ 0),
    YPB6150b = case_when(YPB6150 < 5 ~ 1, YPB6150 == 5 ~ 0),
    YPB6160b = case_when(YPB6160 < 5 ~ 1, YPB6160 == 5 ~ 0),
    YPB6170b = case_when(YPB6170 < 5 ~ 1, YPB6170 == 5 ~ 0),
    YPB6180b = case_when(YPB6180 < 5 ~ 1, YPB6180 == 5 ~ 0),
    YPB6190b = case_when(YPB6190 < 5 ~ 1, YPB6190 == 5 ~ 0),
    YPB6210b = case_when(YPB6210 < 5 ~ 1, YPB6210 == 5 ~ 0),
    YPB6220b = case_when(YPB6220 < 5 ~ 1, YPB6220 == 5 ~ 0),
    YPB6230b = case_when(YPB6230 < 5 ~ 1, YPB6230 == 5 ~ 0))

life_score <- life_score %>% mutate(life_score_inconsistent_since_21y_age_22y = rowSums(across(c(YPB6000b, 	YPB6010b, 	YPB6020b, 	YPB6030b, 	YPB6040b, 	YPB6050b, 	YPB6060b, 	YPB6070b, 	YPB6080b, 	YPB6090b, 	YPB6100b, 	YPB6110b, 	YPB6120b, 	YPB6130b, 	YPB6140b, 	YPB6150b, 	YPB6160b, 	YPB6170b, 	YPB6180b, 	YPB6190b, 	YPB6210b, 	YPB6220b, 	YPB6230b))))

###############################################################################
#Calculate life experience score age 23 years 

# Note: item YPC2370 missing from data. Indicates presence of suicide attempt. 
# I think we may have agreed not to include due to sensitivity. Need to confirm. 
for (var in names(life_score %>% select(YPC2150b, 	YPC2160b, 	YPC2170b, 	YPC2180b, 	YPC2190b, 	YPC2200b, 	YPC2210b, 	YPC2220b, 	YPC2230b, 	YPC2240b, 	YPC2250b, 	YPC2260b, 	YPC2270b, 	YPC2280b, 	YPC2290b, 	YPC2300b, 	YPC2310b, 	YPC2320b, 	YPC2330b, 	YPC2340b, 	YPC2350b, 	YPC2360b, 	YPC2380b, 	YPC2390b, 	YPC2400b, 	YPC2410b, ))) {
  life_score %>% count(.data[[var]]) %>% print()
}

life_score <- life_score %>%
  mutate(
    YPC2150b = case_when(YPC2150b < 5 ~ 1, YPC2150b == 5 ~ 0),
    YPC2160b = case_when(YPC2160b < 5 ~ 1, YPC2160b == 5 ~ 0),
    YPC2170b = case_when(YPC2170b < 5 ~ 1, YPC2170b == 5 ~ 0),
    YPC2180b = case_when(YPC2180b < 5 ~ 1, YPC2180b == 5 ~ 0),
    YPC2190b = case_when(YPC2190b < 5 ~ 1, YPC2190b == 5 ~ 0),
    YPC2200b = case_when(YPC2200b < 5 ~ 1, YPC2200b == 5 ~ 0),
    YPC2210b = case_when(YPC2210b < 5 ~ 1, YPC2210b == 5 ~ 0),
    YPC2220b = case_when(YPC2220b < 5 ~ 1, YPC2220b == 5 ~ 0),
    YPC2230b = case_when(YPC2230b < 5 ~ 1, YPC2230b == 5 ~ 0),
    YPC2240b = case_when(YPC2240b < 5 ~ 1, YPC2240b == 5 ~ 0),
    YPC2250b = case_when(YPC2250b < 5 ~ 1, YPC2250b == 5 ~ 0),
    YPC2260b = case_when(YPC2260b < 5 ~ 1, YPC2260b == 5 ~ 0),
    YPC2270b = case_when(YPC2270b < 5 ~ 1, YPC2270b == 5 ~ 0),
    YPC2280b = case_when(YPC2280b < 5 ~ 1, YPC2280b == 5 ~ 0),
    YPC2290b = case_when(YPC2290b < 5 ~ 1, YPC2290b == 5 ~ 0),
    YPC2300b = case_when(YPC2300b < 5 ~ 1, YPC2300b == 5 ~ 0),
    YPC2310b = case_when(YPC2310b < 5 ~ 1, YPC2310b == 5 ~ 0),
    YPC2320b = case_when(YPC2320b < 5 ~ 1, YPC2320b == 5 ~ 0),
    YPC2330b = case_when(YPC2330b < 5 ~ 1, YPC2330b == 5 ~ 0),
    YPC2340b = case_when(YPC2340b < 5 ~ 1, YPC2340b == 5 ~ 0),
    YPC2350b = case_when(YPC2350b < 5 ~ 1, YPC2350b == 5 ~ 0),
    YPC2360b = case_when(YPC2360b < 5 ~ 1, YPC2360b == 5 ~ 0),
    YPC2380b = case_when(YPC2380b < 5 ~ 1, YPC2380b == 5 ~ 0),
    YPC2390b = case_when(YPC2390b < 5 ~ 1, YPC2390b == 5 ~ 0),
    YPC2400b = case_when(YPC2400b < 5 ~ 1, YPC2400b == 5 ~ 0),
    YPC2410b = case_when(YPC2410b < 5 ~ 1, YPC2410b == 5 ~ 0))

life_score <- life_score %>% mutate(life_score_inconsistent_since_22y_age_23y = rowSums(across(c(YPC2150b, 	YPC2160b, 	YPC2170b, 	YPC2180b, 	YPC2190b, 	YPC2200b, 	YPC2210b, 	YPC2220b, 	YPC2230b, 	YPC2240b, 	YPC2250b, 	YPC2260b, 	YPC2270b, 	YPC2280b, 	YPC2290b, 	YPC2300b, 	YPC2310b, 	YPC2320b, 	YPC2330b, 	YPC2340b, 	YPC2350b, 	YPC2360b, 	YPC2380b, 	YPC2390b, 	YPC2400b, 	YPC2410b))))

# Summary of life score variables created
data.frame(var.names=names(life_score)) %>% filter(grepl('_age', var.names))

# Retain list of old life score variables, to drop at the end 
old_life_score_vars <- as_vector(data.frame(var.names=names(life_score)) %>% filter(grepl('_age', var.names)))

for (var in names(life_score %>% select((contains('_age'))))) {
  life_score %>% count(.data[[var]]) %>% print(n=50)
}

# Reduce life scores to binary variables - for any events experienced. 
life_score <- life_score %>%
  mutate(
    life_score_since_6m_age_1y6m_bin = case_when(life_score_since_6m_age_1y6m > 0 ~ 1, life_score_since_6m_age_1y6m == 0 ~ 0), 
    life_score_since_18m_age_2y6m_bin = case_when(life_score_since_18m_age_2y6m > 0 ~ 1, life_score_since_18m_age_2y6m == 0 ~ 0), 
    life_score_since_30m_age_3y6m_bin = case_when(life_score_since_30m_age_3y6m > 0 ~ 1, life_score_since_30m_age_3y6m == 0 ~ 0),
    life_score_since_3y_age_4y9m_bin = case_when(life_score_since_3y_age_4y9m > 0 ~ 1, life_score_since_3y_age_4y9m == 0 ~ 0),
    life_score_since_5y_age_5y9m_bin = case_when(life_score_since_5y_age_5y9m > 0 ~ 1, life_score_since_5y_age_5y9m == 0 ~ 0),
    life_score_since_7y_age_8y7m_bin = case_when(life_score_since_7y_age_8y7m > 0 ~ 1, life_score_since_7y_age_8y7m == 0 ~ 0),
    life_score_inconsistent_since_12y_age_16y6m_bin = case_when(life_score_inconsistent_since_12y_age_16y6m > 0 ~ 1, life_score_inconsistent_since_12y_age_16y6m == 0 ~ 0),
    life_score_inconsistent_since_21y_age_22y_bin = case_when(life_score_inconsistent_since_21y_age_22y > 0 ~ 1, life_score_inconsistent_since_21y_age_22y == 0 ~ 0),
    life_score_inconsistent_since_22y_age_23y_bin = case_when(life_score_inconsistent_since_22y_age_23y > 0 ~ 1, life_score_inconsistent_since_22y_age_23y == 0 ~ 0))
    
# Check DVs
life_score %>% count(life_score_since_6m_age_1y6m_bin)
life_score %>% count(life_score_inconsistent_since_12y_age_16y6m_bin)
life_score %>% count(life_score_since_6m_age_1y6m, life_score_since_6m_age_1y6m_bin)
life_score %>% count(life_score_inconsistent_since_22y_age_23y_bin, life_score_inconsistent_since_22y_age_23y) %>% print(n=40)
life_score %>% count(life_score_inconsistent_since_22y_age_23y_bin)

life_score %>%
  summarise(
    mean1.5=mean(life_score_since_6m_age_1y6m_bin, na.rm=TRUE)*100,
    mean2.5= mean(life_score_since_18m_age_2y6m_bin, na.rm=TRUE)*100,
    mean3.5= mean(life_score_since_30m_age_3y6m_bin, na.rm=TRUE)*100,
    mean4.75= mean(life_score_since_3y_age_4y9m_bin, na.rm=TRUE)*100,
    mean5.75=mean(life_score_since_5y_age_5y9m_bin, na.rm=TRUE)*100,
    mean8.5= mean(life_score_since_7y_age_8y7m_bin, na.rm=TRUE)*100,
    mean16.5= mean(life_score_inconsistent_since_12y_age_16y6m_bin, na.rm=TRUE)*100,
    mean22= mean(life_score_inconsistent_since_21y_age_22y_bin, na.rm=TRUE)*100,
    mean23=mean(life_score_inconsistent_since_22y_age_23y_bin, na.rm=TRUE)*100) %>% 
      pivot_longer(cols=1:9, names_to="year", values_to="mean")

life_score %>%
  summarise(
    missing1.5=sum(is.na(life_score_since_6m_age_1y6m_bin)),
    missing2.5= sum(is.na(life_score_since_18m_age_2y6m_bin)),
    missing3.5= sum(is.na(life_score_since_30m_age_3y6m_bin)),
    missing4.75= sum(is.na(life_score_since_3y_age_4y9m_bin)),
    missing5.75=sum(is.na(life_score_since_5y_age_5y9m)),
    missing8.5= sum(is.na(life_score_since_7y_age_8y7m_bin)),
    missing16.5= sum(is.na(life_score_inconsistent_since_12y_age_16y6m_bin)),
    missing22= sum(is.na(life_score_inconsistent_since_21y_age_22y_bin)),
    missing23=sum(is.na(life_score_inconsistent_since_22y_age_23y_bin))) %>% 
  pivot_longer(cols=1:9, names_to="year", values_to="num_missing")     

names(life_score %>% select(!(all_of(old_life_score_vars))) %>% select(contains("_age")))
life_score_to_merge <- life_score %>% select(!(all_of(old_life_score_vars))) %>% select(contains("_age") | uniqid)
alspac <- left_join(alspac, life_score_to_merge, by = "uniqid")

################################################################################
# Adverse life event score
################################################################################

### Adverse life event score - 1y6m

alspac %>% count(life_event_child_taken_into_care_age_1y_6m)
alspac %>% count(life_event_control_physically_hurt_by_someone_age_1y_6m)
alspac %>% count(life_event_control_sexually_abused_age_1y_6m)
alspac %>% count(life_event_acquired_a_new_parent_age_1y_6m)
alspac %>% count(life_event_changed_carer_age_1y_6m)

alspac <- alspac %>%
  mutate(
    adverse_life_event_age_1y_6m = case_when(life_event_child_taken_into_care_age_1y_6m == 1 | life_event_control_physically_hurt_by_someone_age_1y_6m == 1 | life_event_control_sexually_abused_age_1y_6m == 1 | life_event_acquired_a_new_parent_age_1y_6m == 1 | life_event_changed_carer_age_1y_6m == 1 ~ 1, 
                                   life_event_child_taken_into_care_age_1y_6m == 2 & life_event_control_physically_hurt_by_someone_age_1y_6m == 2 & life_event_control_sexually_abused_age_1y_6m == 2 & life_event_acquired_a_new_parent_age_1y_6m == 2 & life_event_changed_carer_age_1y_6m == 2 ~ 0))

alspac %>% count(adverse_life_event_age_1y_6m)                                   
alspac %>% count(life_event_child_taken_into_care_age_1y_6m, adverse_life_event_age_1y_6m)
alspac %>% count(life_event_control_physically_hurt_by_someone_age_1y_6m, adverse_life_event_age_1y_6m)
alspac %>% count(life_event_control_sexually_abused_age_1y_6m, adverse_life_event_age_1y_6m)
alspac %>% count(life_event_acquired_a_new_parent_age_1y_6m, adverse_life_event_age_1y_6m)
alspac %>% count(life_event_changed_carer_age_1y_6m, adverse_life_event_age_1y_6m)                           
    

### Adverse life event score - 2y6m

alspac %>% count(life_event_child_taken_into_care_age_2y_6m)
alspac %>% count(life_event_physically_hurt_by_someone_age_2y_6m)
alspac %>% count(life_event_sexually_abused_age_2y_6m)
alspac %>% count(life_event_acquired_a_new_parent_age_2y_6m)
alspac %>% count(life_event_changed_carer_age_2y_6m)

alspac <- alspac %>%
  mutate(
    adverse_life_event_age_2y_6m = case_when(life_event_child_taken_into_care_age_2y_6m == 1 | life_event_physically_hurt_by_someone_age_2y_6m == 1 | life_event_sexually_abused_age_2y_6m == 1 | life_event_acquired_a_new_parent_age_2y_6m == 1 | life_event_changed_carer_age_2y_6m == 1 ~ 1, 
                                             life_event_child_taken_into_care_age_2y_6m == 2 & life_event_physically_hurt_by_someone_age_2y_6m == 2 & life_event_sexually_abused_age_2y_6m == 2 & life_event_acquired_a_new_parent_age_2y_6m == 2 & life_event_changed_carer_age_2y_6m == 2 ~ 0))

alspac %>% count(adverse_life_event_age_2y_6m)                                   
alspac %>% count(life_event_child_taken_into_care_age_2y_6m, adverse_life_event_age_2y_6m)
alspac %>% count(life_event_physically_hurt_by_someone_age_2y_6m, adverse_life_event_age_2y_6m)
alspac %>% count(life_event_sexually_abused_age_2y_6m, adverse_life_event_age_2y_6m)
alspac %>% count(life_event_acquired_a_new_parent_age_2y_6m, adverse_life_event_age_2y_6m)
alspac %>% count(life_event_changed_carer_age_2y_6m, adverse_life_event_age_2y_6m) 

### Adverse life event score - 3y6m

alspac %>% count(life_event_control_physically_hurt_by_someone_age_3y_6m)
alspac %>% count(life_event_control_sexually_abused_age_3y_6m)
alspac %>% count(life_event_acquired_a_new_parent_age_3y_6m)
alspac %>% count(life_event_changed_carer_age_3y_6m)

alspac <- alspac %>%
  mutate(
    adverse_life_event_age_3y_6m = case_when(life_event_control_physically_hurt_by_someone_age_3y_6m == 1 | life_event_control_sexually_abused_age_3y_6m == 1 | life_event_acquired_a_new_parent_age_3y_6m == 1 |  life_event_changed_carer_age_3y_6m == 1 ~ 1, 
                                             life_event_control_physically_hurt_by_someone_age_3y_6m == 2 & life_event_control_sexually_abused_age_3y_6m == 2 & life_event_acquired_a_new_parent_age_3y_6m == 2 &  life_event_changed_carer_age_3y_6m == 2 ~ 0))

alspac %>% count(adverse_life_event_age_3y_6m)                                   
alspac %>% count(life_event_control_physically_hurt_by_someone_age_3y_6m, adverse_life_event_age_3y_6m)
alspac %>% count(life_event_control_sexually_abused_age_3y_6m, adverse_life_event_age_3y_6m)
alspac %>% count(life_event_acquired_a_new_parent_age_3y_6m, adverse_life_event_age_3y_6m)
alspac %>% count(life_event_changed_carer_age_3y_6m, adverse_life_event_age_3y_6m)

### Adverse life event score - 4y9m

alspac %>% count(life_event_child_taken_into_care_age_4y_9m)
alspac %>% count(life_event_control_physically_hurt_by_someone_age_4y_9m)
alspac %>% count(life_event_control_sexually_abused_age_4y_9m)
alspac %>% count(life_event_acquired_a_new_parent_age_4y_9m)
alspac %>% count(life_event_changed_carer_age_4y_9m)


alspac <- alspac %>%
  mutate(
    adverse_life_event_age_4y_9m = case_when(life_event_child_taken_into_care_age_4y_9m == "Yes" | life_event_control_physically_hurt_by_someone_age_4y_9m == "Yes" | life_event_control_sexually_abused_age_4y_9m == "Yes" | life_event_acquired_a_new_parent_age_4y_9m == "Yes" | life_event_changed_carer_age_4y_9m == "Yes" ~ 1, 
                                             life_event_child_taken_into_care_age_4y_9m == "No" & life_event_control_physically_hurt_by_someone_age_4y_9m == "No" & life_event_control_sexually_abused_age_4y_9m == "No" & life_event_acquired_a_new_parent_age_4y_9m == "No" & life_event_changed_carer_age_4y_9m == "No" ~ 0))

alspac %>% count(adverse_life_event_age_4y_9m)                                   
alspac %>% count(life_event_child_taken_into_care_age_4y_9m, adverse_life_event_age_4y_9m)
alspac %>% count(life_event_control_physically_hurt_by_someone_age_4y_9m, adverse_life_event_age_4y_9m)
alspac %>% count(life_event_control_sexually_abused_age_4y_9m, adverse_life_event_age_4y_9m)
alspac %>% count(life_event_acquired_a_new_parent_age_4y_9m, adverse_life_event_age_4y_9m)
alspac %>% count(life_event_changed_carer_age_4y_9m, adverse_life_event_age_4y_9m) 

### Adverse life event score - 5y9m

alspac %>% count(life_event_child_taken_into_care_age_5y_9m)
alspac %>% count(life_event_control_physically_hurt_by_someone_age_5y_9m)
alspac %>% count(life_event_control_sexually_abused_age_5y_9m)
alspac %>% count(life_event_changed_carer_age_5y_9m)

alspac <- alspac %>%
  mutate(
    adverse_life_event_age_5y_9m = case_when(life_event_child_taken_into_care_age_5y_9m == "Yes" | life_event_control_physically_hurt_by_someone_age_5y_9m == "Yes" | life_event_control_sexually_abused_age_5y_9m == "Yes" | life_event_changed_carer_age_5y_9m == "Yes"  ~ 1, 
                                             life_event_child_taken_into_care_age_5y_9m == "No" & life_event_control_physically_hurt_by_someone_age_5y_9m == "No" & life_event_control_sexually_abused_age_5y_9m == "No" & life_event_changed_carer_age_5y_9m == "No" ~ 0))

alspac %>% count(adverse_life_event_age_5y_9m)                                   
alspac %>% count(life_event_child_taken_into_care_age_5y_9m, adverse_life_event_age_5y_9m)
alspac %>% count(life_event_control_physically_hurt_by_someone_age_5y_9m, adverse_life_event_age_5y_9m)
alspac %>% count(life_event_control_sexually_abused_age_5y_9m, adverse_life_event_age_5y_9m)
alspac %>% count(life_event_changed_carer_age_5y_9m, adverse_life_event_age_5y_9m)

### Adverse life event score - 8y7m

alspac %>% count(life_event_control_child_taken_into_care_age_8y_7m)
alspac %>% count(life_event_control_physically_hurt_by_someone_age_8y_7m)
alspac %>% count(life_event_control_sexually_abused_age_8y_7m)
alspac %>% count(life_event_control_death_in_family_age_8y_7m)
alspac %>% count(life_event_control_acquired_a_new_parent_age_8y_7m)
alspac %>% count(life_event_control_changed_carer_age_8y_7m)
alspac %>% count(life_event_lost_best_friend_age_8y_7m)

# Excluding lost best friend and death in family - not present in earlier waves. 

alspac <- alspac %>%
  mutate(
    adverse_life_event_age_8y_7m = case_when(life_event_control_child_taken_into_care_age_8y_7m == "Yes" | life_event_control_physically_hurt_by_someone_age_8y_7m == "Yes" | life_event_control_sexually_abused_age_8y_7m == "Yes" |  life_event_control_acquired_a_new_parent_age_8y_7m == "Yes" | life_event_control_changed_carer_age_8y_7m == "Yes"  ~ 1, 
                                            life_event_control_child_taken_into_care_age_8y_7m ==  "No" & life_event_control_physically_hurt_by_someone_age_8y_7m ==  "No" & life_event_control_sexually_abused_age_8y_7m ==  "No"   & life_event_control_acquired_a_new_parent_age_8y_7m ==  "No" & life_event_control_changed_carer_age_8y_7m ==  "No"  ~ 0))

alspac %>% count(adverse_life_event_age_8y_7m)
alspac %>% count(life_event_control_child_taken_into_care_age_8y_7m, adverse_life_event_age_8y_7m)
alspac %>% count(life_event_control_physically_hurt_by_someone_age_8y_7m, adverse_life_event_age_8y_7m)
alspac %>% count(life_event_control_sexually_abused_age_8y_7m, adverse_life_event_age_8y_7m)
alspac %>% count(life_event_control_death_in_family_age_8y_7m, adverse_life_event_age_8y_7m)
alspac %>% count(life_event_control_acquired_a_new_parent_age_8y_7m, adverse_life_event_age_8y_7m) 
alspac %>% count(life_event_control_changed_carer_age_8y_7m, adverse_life_event_age_8y_7m) 
alspac %>% count(life_event_lost_best_friend_age_8y_7m, adverse_life_event_age_8y_7m) 

### Adverse life event score - 16y6m

alspac %>% count(life_event_control_parental_divorce_age_16y_6m)
alspac %>% count(life_event_control_death_in_immediate_family_age_16y_6m)
alspac %>% count(life_event_grandparet_died_age_16y_6m)
alspac %>% count(life_event_close_friend_died_age_16y_6m)
alspac %>% count(life_event_control_parent_in_trouble_with_police_age_16y_6m)
alspac %>% count(life_event_parent_lost_job_age_16y_6m)


alspac <- alspac %>%
  mutate(
    adverse_life_event_age_16y6m = case_when(life_event_control_parental_divorce_age_16y_6m == 1 | life_event_control_death_in_immediate_family_age_16y_6m == 1 | life_event_grandparet_died_age_16y_6m == 1 | life_event_close_friend_died_age_16y_6m == 1 | life_event_control_parent_in_trouble_with_police_age_16y_6m == 1 | life_event_parent_lost_job_age_16y_6m == 1 ~ 1, 
                                             life_event_control_parental_divorce_age_16y_6m ==  2 & life_event_control_death_in_immediate_family_age_16y_6m ==  2 & life_event_grandparet_died_age_16y_6m ==  2 & life_event_close_friend_died_age_16y_6m ==  2 & life_event_control_parent_in_trouble_with_police_age_16y_6m ==  2 & life_event_parent_lost_job_age_16y_6m ==  2 ~ 0))

alspac %>% count(adverse_life_event_age_16y6m)                                   
alspac %>% count(life_event_control_parental_divorce_age_16y_6m, adverse_life_event_age_16y6m)
alspac %>% count(life_event_control_death_in_immediate_family_age_16y_6m, adverse_life_event_age_16y6m)
alspac %>% count(life_event_grandparet_died_age_16y_6m, adverse_life_event_age_16y6m)
alspac %>% count(life_event_close_friend_died_age_16y_6m, adverse_life_event_age_16y6m)
alspac %>% count(life_event_control_parent_in_trouble_with_police_age_16y_6m, adverse_life_event_age_16y6m) 
alspac %>% count(life_event_parent_lost_job_age_16y_6m, adverse_life_event_age_16y6m) 

### Adverse life event score - 22y

alspac %>% count(life_event_control_lost_job_age_22y)
alspac %>% count(life_event_were_divorced_age_22y)
alspac %>% count(life_event_control_trouble_with_law_age_22y)
alspac %>% count(life_event_parent_died_age_22y)
alspac %>% count(life_event_friend_died_age_22y)
alspac %>% count(life_event_relative_died_not_parent_age_22y)
alspac %>% count(life_event_control_became_homeless_age_22y)
alspac %>% count(life_event_control_major_financial_problems_age_22y)
alspac %>% count(life_event_parents_divorced_age_22y)

alspac <- alspac %>%
  mutate(
    adverse_life_event_age_22y = case_when(life_event_control_lost_job_age_22y == "Yes" | life_event_were_divorced_age_22y == "Yes" | life_event_control_trouble_with_law_age_22y == "Yes" | life_event_parent_died_age_22y == "Yes" | life_event_friend_died_age_22y == "Yes" | life_event_relative_died_not_parent_age_22y == "Yes" | life_event_control_became_homeless_age_22y == "Yes" | life_event_control_major_financial_problems_age_22y == "Yes" | life_event_parents_divorced_age_22y == "Yes" ~ 1, 
                                           life_event_control_lost_job_age_22y ==  "No" & life_event_were_divorced_age_22y ==  "No" & life_event_control_trouble_with_law_age_22y ==  "No" & life_event_parent_died_age_22y ==  "No" | life_event_friend_died_age_22y ==  "No" & life_event_relative_died_not_parent_age_22y ==  "No" & life_event_control_became_homeless_age_22y ==  "No" & life_event_control_major_financial_problems_age_22y ==  "No" & life_event_parents_divorced_age_22y ==  "No" ~ 0))

alspac %>% count(adverse_life_event_age_22y)                                   
alspac %>% count(life_event_control_lost_job_age_22y, adverse_life_event_age_22y)
alspac %>% count(life_event_were_divorced_age_22y, adverse_life_event_age_22y)
alspac %>% count(life_event_control_trouble_with_law_age_22y, adverse_life_event_age_22y)
alspac %>% count(life_event_parent_died_age_22y, adverse_life_event_age_22y)
alspac %>% count(life_event_friend_died_age_22y, adverse_life_event_age_22y) 
alspac %>% count(life_event_relative_died_not_parent_age_22y, adverse_life_event_age_22y) 
alspac %>% count(life_event_control_became_homeless_age_22y, adverse_life_event_age_22y) 
alspac %>% count(life_event_control_major_financial_problems_age_22y, adverse_life_event_age_22y) 
alspac %>% count(life_event_parents_divorced_age_22y, adverse_life_event_age_22y) 

### Adverse life event score - 23y

alspac %>% count(life_event_control_lost_job_age_23y)
alspac %>% count(life_event_were_divorced_age_23y)
alspac %>% count(life_event_control_trouble_with_law_age_23y)
alspac %>% count(life_event_parent_died_age_23y)
alspac %>% count(life_event_friend_died_age_23y)
alspac %>% count(life_event_child_died_age_23y)
alspac %>% count(life_event_relative_died_not_parent_age_23y)
alspac %>% count(life_event_control_became_homeless_age_23y)
alspac %>% count(life_event_control_major_financial_problems_age_23y)
alspac %>% count(life_event_parents_divorced_age_23y)

alspac <- alspac %>%
  mutate(
    adverse_life_event_age_23y = case_when(life_event_friend_died_age_23y == "Yes" | life_event_child_died_age_23y == "Yes" | life_event_relative_died_not_parent_age_23y == "Yes" | life_event_control_became_homeless_age_23y == "Yes" | life_event_control_major_financial_problems_age_23y == "Yes" | life_event_parents_divorced_age_23y == "Yes" | life_event_control_lost_job_age_23y == "Yes" | life_event_were_divorced_age_23y == "Yes" | life_event_control_trouble_with_law_age_23y == "Yes" | life_event_parent_died_age_23y == "Yes"  ~ 1, 
                                           life_event_friend_died_age_23y ==  "No" & life_event_child_died_age_23y ==  "No" & life_event_relative_died_not_parent_age_23y ==  "No" & life_event_control_became_homeless_age_23y ==  "No" & life_event_control_major_financial_problems_age_23y ==  "No" & life_event_parents_divorced_age_23y ==  "No" & life_event_control_lost_job_age_23y ==  "No" & life_event_were_divorced_age_23y ==  "No" & life_event_control_trouble_with_law_age_23y ==  "No" & life_event_parent_died_age_23y ==  "No"  ~ 0))

alspac %>% count(adverse_life_event_age_23y)                                   
alspac %>% count(life_event_control_lost_job_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_were_divorced_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_control_trouble_with_law_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_parent_died_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_friend_died_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_child_died_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_relative_died_not_parent_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_control_became_homeless_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_control_major_financial_problems_age_23y, adverse_life_event_age_23y)
alspac %>% count(life_event_parents_divorced_age_23y, adverse_life_event_age_23y)

data.frame(var.names=names(alspac)) %>% filter(grepl('adverse', var.names))

## Summarise data

alspac %>%
  summarise(
    mean1.5=mean(adverse_life_event_age_1y_6m, na.rm=TRUE)*100,
    mean2.5= mean(adverse_life_event_age_2y_6m, na.rm=TRUE)*100,
    mean3.5= mean(adverse_life_event_age_3y_6m, na.rm=TRUE)*100,
    mean4.75= mean(adverse_life_event_age_4y_9m, na.rm=TRUE)*100,
    mean5.75=mean(adverse_life_event_age_5y_9m, na.rm=TRUE)*100,
    mean8.5= mean(adverse_life_event_age_8y_7m, na.rm=TRUE)*100,
    mean16.5= mean(adverse_life_event_age_16y6m, na.rm=TRUE)*100,
    mean22= mean(adverse_life_event_age_22y, na.rm=TRUE)*100,
    mean23=mean(adverse_life_event_age_23y, na.rm=TRUE)*100) %>% 
  pivot_longer(cols=1:9, names_to="year", values_to="mean")

alspac %>%
  summarise(
    missing1.5=sum(is.na(adverse_life_event_age_1y_6m)),
    missing2.5= sum(is.na(adverse_life_event_age_2y_6m)),
    missing3.5= sum(is.na(adverse_life_event_age_3y_6m)),
    missing4.75= sum(is.na(adverse_life_event_age_4y_9m)),
    missing5.75=sum(is.na(adverse_life_event_age_5y_9m)),
    missing8.5= sum(is.na(adverse_life_event_age_8y_7m)),
    missing16.5= sum(is.na(adverse_life_event_age_16y6m)),
    missing22= sum(is.na(adverse_life_event_age_22y)),
    missing23=sum(is.na(adverse_life_event_age_23y))) %>% 
  pivot_longer(cols=1:9, names_to="year", values_to="num_missing")    

### Merging adverse life events scores

# This creates a variable for have ever experienced an event over the relevant waves. 

# To age 3
alspac %>% count(adverse_life_event_age_1y_6m)

alspac <- alspac %>% mutate(
  adverse_life_events_score_to_age_3 = case_when(adverse_life_event_age_1y_6m==1 | adverse_life_event_age_2y_6m==1 | adverse_life_event_age_3y_6m==1 ~ 1,
                                                 adverse_life_event_age_1y_6m==0 & adverse_life_event_age_2y_6m==0 & adverse_life_event_age_3y_6m==0 ~ 0))

alspac %>% count(adverse_life_events_score_to_age_3)
alspac %>% count(adverse_life_event_age_1y_6m, adverse_life_events_score_to_age_3)                                                 
alspac %>% count(adverse_life_event_age_2y_6m,adverse_life_events_score_to_age_3)                                                 
alspac %>% count(adverse_life_event_age_3y_6m,adverse_life_events_score_to_age_3)

# Age 4 to 8
alspac <- alspac %>% mutate(
  adverse_life_events_score_age_4_to_8 = case_when(adverse_life_event_age_4y_9m==1 | adverse_life_event_age_5y_9m==1 | adverse_life_event_age_8y_7m==1 ~ 1,
                                                 adverse_life_event_age_4y_9m==0 & adverse_life_event_age_5y_9m==0 & adverse_life_event_age_8y_7m==0 ~ 0))

alspac %>% count(adverse_life_events_score_age_4_to_8)                                                 
alspac %>% count(adverse_life_event_age_4y_9m, adverse_life_events_score_age_4_to_8)                                                 
alspac %>% count(adverse_life_event_age_5y_9m,adverse_life_events_score_age_4_to_8)                                                 
alspac %>% count(adverse_life_event_age_8y_7m,adverse_life_events_score_age_4_to_8)


################################################################################
### Sexual orientation ###
################################################################################

alspac %>% count(fh9140)
attributes(alspac$fh9140)
#From clinic data -age 15.5 years
alspac <- alspac %>% mutate(sexuality_age_15y_6m = case_when(fh9140 > 0 ~ fh9140))
alspac %>% count(sexuality_age_15y_6m)
alspac %>% summarise(sum(!is.na(sexuality_age_15y_6m)))
# Alternative - YPC0420 # 
# Not in data

################################################################################
### substance abuse ###
################################################################################

alspac %>% count(pp2017)
alspac %>% count(pp2018)
alspac %>% count(r2017)
alspac %>% count(r2018)

# The partner variable has a low base, check to see if mother variable about presence of partner is available at the right age. 
# pp is 11 years 2 months old
# Equivalent mother dataset is R

# There is no 'partner' variable in the R dataset. 
# There is a variable that asks whether the partner smokes, and a response option is 'don't have a partner'. 
# Var: r6040.
# We would need to request this. 

alspac <- alspac %>%
  mutate(
    family_partner_drug_addiction_ever_age_11y_6m = case_when(pp2017 > 0 & pp2017 < 3 ~ 1, pp2017 == 3 ~0),
    family_partner_alcoholism_ever_age_11y_6m = case_when(pp2018 > 0 & pp2018 < 3 ~ 1, pp2018 == 3 ~0),
    family_mother_drug_addiction_ever_age_11y_6m = case_when(r2017 > 0 & r2017 < 3 ~ 1, r2017 == 3 ~0),
    family_mother_alcoholism_ever_age_11y_6m = case_when(r2018 > 0 & r2018 < 3 ~ 1, r2018 == 3 ~0))

alspac %>% count(family_partner_drug_addiction_ever_age_11y_6m)
alspac %>% count(family_partner_alcoholism_ever_age_11y_6m)
alspac %>% count(family_mother_drug_addiction_ever_age_11y_6m)
alspac %>% count(family_mother_alcoholism_ever_age_11y_6m)

alspac %>%
  summarise(
    mean_drug_addiction=mean(family_mother_drug_addiction_ever_age_11y_6m, na.rm=TRUE)*100,
    mean_alcoholism= mean(family_mother_alcoholism_ever_age_11y_6m, na.rm=TRUE)*100) %>% 
  pivot_longer(cols=1:2, names_to="year", values_to="mean")

# Extremely rare, less than 1% of mothers report ever being an addict or alcoholic.

################################################################################
### Maternal bond ###
################################################################################

alspac %>% count(family_maternal_bond_age_0y8m) %>% print(n=40)
alspac %>% count(family_maternal_bond_age_2y9m) %>% print(n=40)
alspac %>% count(family_paternal_bond_age_0y8m) %>% print(n=40)
alspac %>% count(family_paternal_bond_age_1y9m) %>% print(n=40)

alspac %>% count(f500) %>% print(n=40)
alspac %>% count(g590) %>% print(n=40)

alspac <- alspac %>% mutate(
  family_paternal_bond_age_0y8m_backcode = case_when(family_paternal_bond_age_0y8m > 0 ~ family_paternal_bond_age_0y8m, f500 == 2 ~ 0),
  family_paternal_bond_age_1y9m_backcode = case_when(family_paternal_bond_age_1y9m > 0 ~ family_paternal_bond_age_1y9m, g590 == 3 ~ 0))

# Adds 477 cases, which I have coded as zero - given there is no partner present. 
alspac %>% count(family_paternal_bond_age_0y8m_backcode) %>% print(n=40)
# Adds 530 cases, coded in the same way.  
alspac %>% count(family_paternal_bond_age_1y9m_backcode) %>% print(n=40)


alspac %>%
  summarise(
    maternalbond_missing_0y8m = sum(is.na(family_maternal_bond_age_0y8m)),
    maternalbond_missing_2y9m = sum(is.na(family_maternal_bond_age_2y9m)),
    paternal_bond_missing_0y8m = sum(is.na(family_paternal_bond_age_0y8m_backcode)),
    paternal_bond_missing_1y9m = sum(is.na(family_paternal_bond_age_1y9m_backcode))) %>% 
  pivot_longer(cols=1:4, names_to="year", values_to="num_missing") 

################################################################################
### Parenting score ###
################################################################################

alspac %>% count(family_partner_parenting_score_age_3y2m) %>% print(n=40)
alspac %>% count(family_mother_parenting_score_age_3y2m) %>% print(n=40)
alspac %>% count(family_female_parenting_score_age_6y9m) %>% print(n=71)
alspac %>% count(family_male_parenting_score_age_6y9m)  %>% print(n=72)

parenting_score_missingness <- alspac %>%
  summarise(
    family_partner_parenting_score_age_3y2m = sum(is.na(family_partner_parenting_score_age_3y2m)),
    family_mother_parenting_score_age_3y2m = sum(is.na(family_mother_parenting_score_age_3y2m)),
    family_female_parenting_score_age_6y9m = sum(is.na(family_female_parenting_score_age_6y9m)),
    family_male_parenting_score_age_6y9m = sum(is.na(family_male_parenting_score_age_6y9m))) %>% 
  pivot_longer(cols=1:4, names_to="year", values_to="num_missing") 
parenting_score_missingness

# Higher missingness for partner parenting score
# -2 on original variable [kg240] indicates no partner...
alspac %>% count(kg240) %>% print(n=40)

alspac <- alspac %>% mutate(family_partner_parenting_score_age_3y2m = 
                        case_when(
                          family_partner_parenting_score_age_3y2m >= 0 ~ family_partner_parenting_score_age_3y2m, 
                          kg240 == -2 ~ 0))

# Adds 579 cases to the data as zero...
alspac %>% count(family_partner_parenting_score_age_3y2m) %>% print(n=40)

parenting_score_missingness <- alspac %>%
  summarise(
    family_partner_parenting_score_age_3y2m = sum(is.na(family_partner_parenting_score_age_3y2m)),
    family_mother_parenting_score_age_3y2m = sum(is.na(family_mother_parenting_score_age_3y2m)),
    family_female_parenting_score_age_6y9m = sum(is.na(family_female_parenting_score_age_6y9m)),
    family_male_parenting_score_age_6y9m = sum(is.na(family_male_parenting_score_age_6y9m))) %>% 
  pivot_longer(cols=1:4, names_to="year", values_to="num_missing") 
parenting_score_missingness

# Still slightly higher, but better. 

################################################################################
### Parenting interaction score ###
################################################################################
alspac %>% count(relationship_to_parents_partner_interaction_score_age_2y6m) %>% print(n=40)
alspac %>% count(relationship_to_parents_mother_interaction_score_age_2y6m) %>% print(n=40)

# Higher missingness for partner interaction score
# -2 on original variable [kj410] indicates no partner...
alspac %>% count(kj410)

# Back coding
alspac <- alspac %>% mutate(relationship_to_parents_partner_interaction_score_age_2y6m = 
                        case_when(
                          relationship_to_parents_partner_interaction_score_age_2y6m >= 0 ~ relationship_to_parents_partner_interaction_score_age_2y6m, 
                          kj410 == -2 ~ 0))

# Adds 631 cases as zero
alspac %>% count(relationship_to_parents_partner_interaction_score_age_2y6m) %>% print(n=40)

parenting_interaction_missingness <- alspac %>%
  summarise(
    relationship_to_parents_partner_interaction_score_age_2y6m = sum(is.na(relationship_to_parents_partner_interaction_score_age_2y6m)),
    relationship_to_parents_mother_interaction_score_age_2y6m = sum(is.na(relationship_to_parents_mother_interaction_score_age_2y6m))) %>% 
  pivot_longer(cols=1:2, names_to="year", values_to="num_missing") 
parenting_interaction_missingness
# Now very similar levels of missingness. 


################################################################################
### Parental closeness to child  ###
################################################################################
alspac %>% count(relationship_to_parents_mother_close_to_child_age_8y1m) 
alspac %>% count(relationship_to_parents_partner_close_to_child_age_8y1m)
alspac %>% count(relationship_to_parents_partner_very_close_to_study_child_age_6y1m)
alspac %>% count(relationship_to_parents_partner_very_close_child_age_7y1m)
alspac %>% count(relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m)

#Back coding partner variables 

# 8y1m
alspac %>% count(n8387)
alspac_all %>% count(n3000, n8387)
alspac <- alspac %>% mutate(relationship_to_parents_partner_close_to_child_age_8y1m = 
                              case_when(
                                relationship_to_parents_partner_close_to_child_age_8y1m == 1 ~ "Yes",
                                relationship_to_parents_partner_close_to_child_age_8y1m == 2 | relationship_to_parents_partner_close_to_child_age_8y1m == 3  ~  "No/sometimes/occassionally",
                                n3000 == 3 ~ "No partner"))
alspac %>% count(relationship_to_parents_partner_close_to_child_age_8y1m)

# 6y1m 
alspac %>% count(pj6054)
alspac %>% count(l6000, relationship_to_parents_partner_very_close_to_study_child_age_6y1m)
alspac <- alspac %>% mutate(relationship_to_parents_partner_very_close_to_study_child_age_6y1m = 
                              case_when(
                                relationship_to_parents_partner_very_close_to_study_child_age_6y1m == 1 ~ "Always",
                                relationship_to_parents_partner_very_close_to_study_child_age_6y1m == 2 | relationship_to_parents_partner_very_close_to_study_child_age_6y1m == 3  ~  "Never or sometimes",
                                l6000 == 3 ~ "No partner"))
alspac %>% count(relationship_to_parents_partner_very_close_to_study_child_age_6y1m)

# 7y1m
alspac %>% count(relationship_to_parents_partner_very_close_child_age_7y1m)
# M is relevant dataset for back coding - no partner variable to back code with.

# 12y1m
alspac %>% count(s3054)
alspac %>% count(s3000, s3054)
alspac <- alspac %>% mutate(relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m = 
                              case_when(
                                relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m == 1 ~ "Always",
                                relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m == 2 | relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m == 3  ~  "Never or sometimes",
                                s3000 == 4 ~ "No partner"))

alspac %>% count(relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m)

alspac %>%
  summarise(
    relationship_to_parents_partner_very_close_to_study_child_age_6y1m = sum(is.na(relationship_to_parents_partner_very_close_to_study_child_age_6y1m)),
    relationship_to_parents_partner_very_close_child_age_7y1m = sum(is.na(relationship_to_parents_partner_very_close_child_age_7y1m)), 
    relationship_to_parents_mother_close_to_child_age_8y1m = sum(is.na(relationship_to_parents_mother_close_to_child_age_8y1m)),
    relationship_to_parents_partner_close_to_child_age_8y1m = sum(is.na(relationship_to_parents_partner_close_to_child_age_8y1m)),
    relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m = sum(is.na(relationship_to_parents_mothers_partner_close_to_study_child_age_12y1m))) %>% 
  pivot_longer(cols=1:5, names_to="year", values_to="num_missing") 

# Waves 6y1m and 7y1m are from partner based source variables, so higher missingness, even with backcoding. 


alspac <- alspac %>% mutate(
  mother_marital_status_age_gest = case_when(a525 > 0 ~ a525),
  mother_marital_status_age_0y8m = case_when(f460 > 0 ~ f460),
  mother_marital_status_age_1y9m = case_when(g517 > 0 ~ g517),
  mother_marital_status_age_2y9m = case_when(h386 > 0 ~ h386),
  mother_marital_status_age_3y11m = case_when(j370 > 0 ~ j370),
  mother_marital_status_age_7y1m = case_when(m3040 > 0 ~ m3040),
  mother_marital_status_age_8y1m = case_when(n8040 > 0 ~ n8040),
  mother_marital_status_age_10y1m = case_when(q3040 > 0 ~ q3040),
  mother_marital_status_age_18y6m = case_when(t1050 > 0 ~ t1050))

alspac %>%
  summarise(
    mother_marital_status_age_gest = sum(is.na(mother_marital_status_age_gest)),
    mother_marital_status_age_0y8m = sum(is.na(mother_marital_status_age_0y8m)),
    mother_marital_status_age_1y9m = sum(is.na(mother_marital_status_age_1y9m)),
    mother_marital_status_age_2y9m = sum(is.na(mother_marital_status_age_2y9m)),
    mother_marital_status_age_3y11m = sum(is.na(mother_marital_status_age_3y11m)),
    mother_marital_status_age_7y1m = sum(is.na(mother_marital_status_age_7y1m)),
    mother_marital_status_age_8y1m = sum(is.na(mother_marital_status_age_8y1m)),
    mother_marital_status_age_10y1m = sum(is.na(mother_marital_status_age_10y1m)),
    mother_marital_status_age_18y6m = sum(is.na(mother_marital_status_age_18y6m))) %>%
       pivot_longer(cols=1:9, names_to="year", values_to="num_missing") 

###############################################################################
### Add SDQ score - at age 7 - minus the peer problems component
###############################################################################

# There is no SDQ score at age 7, closest is age 6y 9m (wave KQ). 

# Relevant variables to derive SDQ, minus peer problems score. 

  # KQ347F -  DV: Total Behavioural Difficulties Score (number of missing items).
      # If this is greater than zero, the Total Behavioural Difficulties Score should be -2 (missing). 
  
  # kq345f - DV: Total Behavioural Difficulties Score
      # Computed by summing the sub-scores. 

  # KQ345E - DV: Peer Problems Score. 
      # The DV for this sub-scale. 
      # Minus this from the overall score, would make sense to me (unless it is minus 2, in which case declare NA. 
      
# Unfortunately, we do not have the above final variable. Instead we have the pro-rated one. 

  # kq348f DV: SDQ total difficulties score (prorated).
    # Computed as the sum of the prorated individual scores - 

alspac %>% count(kq348a)
alspac %>% count(kq348b)
alspac %>% count(kq348c)
alspac %>% count(kq348d)
alspac %>% count(kq348e)

alspac <- alspac %>% mutate(
  kq348a_m = case_when(kq348a >= 0 ~ kq348a), 
  kq348b_m = case_when(kq348b >= 0 ~ kq348b), 
  kq348c_m = case_when(kq348c >= 0 ~ kq348c), 
  kq348d_m = case_when(kq348d >= 0 ~ kq348d), 
  kq348e_m = case_when(kq348e >= 0 ~ kq348e), 
)

alspac %>% count(kq348a, kq348a_m)
alspac %>% count(kq348b, kq348b_m)
alspac %>% count(kq348c, kq348c_m)
alspac %>% count(kq348d, kq348d_m)
alspac %>% count(kq348e, kq348e_m)

# Generate test total difficulties score. 

alspac <- alspac %>% mutate(
  SDQ_tot_diff = kq348b_m + kq348c_m + kq348d_m + kq348e_m
)
alspac %>% count(SDQ_tot_diff, kq348f) %>% print(n=40)

# Note:  "the prosocial score is not incorporated in the reverse direction into the total difficulties score since the absence of prosocial behaviours is conceptually different from the presence of psychological difficulties."
# See: https://acamh.onlinelibrary.wiley.com/doi/pdf/10.1111/j.1469-7610.1997.tb01545.x [pg 582]

alspac %>% count(kq348f) %>% print(n=40)
# Matches test score derived above. 

# Now, compute total difficulties score minus peer problems score. 
alspac <- alspac %>% mutate(
  SDQ_total_difficulties_age_6y9m = kq348b_m + kq348c_m + kq348d_m
)

alspac %>% count(SDQ_total_difficulties_age_6y9m) %>% print(n=40)
# Note: I did not minus the peer problem score, because this resulted in some minus values, which did not make sense. 
# Rather, this is the sum of the other items. 

###############################################################################
# Averaged predictors
###############################################################################

 # Note: when producing averaged predictors, if one variable is missing, that variable is excluded, and the average is taken based on the remaining waves. 

### Social cohesion

names(alspac %>% select(contains("cohesion")))

alspac %>% count(social_cohesion_age_0y8m) %>% print(n=40)
alspac %>% count(social_cohesion_age_1y9m) %>% print(n=40)
alspac %>% count(social_cohesion_age_2y9m) %>% print(n=40)
class(alspac$social_cohesion_age_2y9m)

alspac <- mutate(alspac, 
      social_cohesion_to_age_3y = rowMeans((select(alspac, c(social_cohesion_age_0y8m, social_cohesion_age_1y9m, social_cohesion_age_2y9m))),na.rm=TRUE),
      social_cohesion_age_5_to_7 = rowMeans((select(alspac, c(social_cohesion_age_5y1m, social_cohesion_age_7y1m))),na.rm=TRUE))

alspac %>% count(social_cohesion_to_age_3y) %>% print(n=100)

alspac %>% filter(is.na(social_cohesion_age_0y8m)) %>% count(social_cohesion_age_0y8m, social_cohesion_age_1y9m, social_cohesion_age_2y9m, social_cohesion_to_age_3y) %>% print(n=40)
alspac %>% filter(social_cohesion_age_0y8m == 6) %>% count(social_cohesion_age_0y8m, social_cohesion_age_1y9m, social_cohesion_to_age_3y) %>% print(n=40)
alspac %>% filter(is.na(social_cohesion_age_0y8m) & is.na(social_cohesion_age_1y9m) & is.na(social_cohesion_age_2y9m)) %>% count(social_cohesion_to_age_3y)

### Social discord

names(alspac %>% select(contains("discord")))

alspac %>% count(social_discord_age_0y8m)
alspac %>% count(social_discord_age_1y9m)
alspac %>% count(social_discord_age_2y9m)
alspac %>% count(social_discord_age_5y1m)
alspac %>% count(social_discord_age_7y1m)

alspac <- mutate(alspac, 
     social_discord_to_age_3y = rowMeans((select(alspac, c(social_discord_age_0y8m, social_discord_age_1y9m, social_discord_age_2y9m))),na.rm=TRUE),
     social_discord_age_5_to_7 = rowMeans((select(alspac, c(social_discord_age_5y1m, social_discord_age_7y1m))),na.rm=TRUE))

alspac %>% count(social_discord_to_age_3y) %>% print(n=100)

alspac %>% filter(social_discord_age_0y8m == 6) %>% count(social_discord_age_1y9m, social_discord_age_2y9m, social_discord_to_age_3y) %>% print(n=40)
alspac %>% filter(is.na(social_discord_age_5y1m) & is.na(social_discord_age_7y1m)) %>% count(social_discord_age_5_to_7)

### Friends score

names(alspac %>% select(contains("friends_score")))

alspac %>% count(friends_score_age_8y)
alspac %>% count(friends_score_age_10y)
sum(!is.na(alspac$friends_score_age_8y))
sum(!is.na(alspac$friends_score_age_10y))

alspac %>% count(friends_score_age_12y6m)
alspac %>% count(friends_score_age_13y6m)
sum(!is.na(alspac$friends_score_age_12y6m))
sum(!is.na(alspac$friends_score_age_13y6m))

alspac <- mutate(alspac, 
   friends_score_age_8_to_10 = rowMeans((select(alspac, c(friends_score_age_8y, friends_score_age_10y))),na.rm=TRUE),
   friends_score_age_12_to_13 = rowMeans((select(alspac, c(friends_score_age_12y6m, friends_score_age_13y6m))),na.rm=TRUE))

alspac %>% count(friends_score_age_8_to_10) %>% print(n=100)
alspac %>% filter(friends_score_age_8y == 8) %>% count(friends_score_age_10y, friends_score_age_8_to_10) %>% print(n=40)
alspac %>% filter(is.na(friends_score_age_10y) & is.na(friends_score_age_8y)) %>% count(friends_score_age_8_to_10)

### Peer problems score 

names(alspac %>% select(contains("peer")))

alspac %>% count(peer_problem_score_age_6y9m)
alspac %>% count(peer_problem_score_age_8y1m)
alspac %>% count(peer_problem_score_age_9y7m)
alspac %>% count(peer_problem_score_age_11y8m)
alspac %>% count(peer_problem_score_age_13y1m)

alspac <- mutate(alspac, 
    peer_problem_score_age_7_to_10 = rowMeans((select(alspac, c(peer_problem_score_age_6y9m, peer_problem_score_age_8y1m, peer_problem_score_age_9y7m))),na.rm=TRUE),
    peer_problem_score_age_12_to_13 = rowMeans((select(alspac, c(peer_problem_score_age_11y8m, peer_problem_score_age_13y1m))),na.rm=TRUE))

alspac %>% count(peer_problem_score_age_7_to_10) %>% print(n=50)
alspac %>% count(peer_problem_score_age_12_to_13) %>% print(n=50)
alspac %>% filter(peer_problem_score_age_6y9m == 0) %>% count(peer_problem_score_age_8y1m, peer_problem_score_age_9y7m, peer_problem_score_age_7_to_10) %>% print(n=40)
alspac %>% filter(is.na(peer_problem_score_age_11y8m) & is.na(peer_problem_score_age_13y1m)) %>% count(peer_problem_score_age_12_to_13)


###############################################################################
 # Binary indicator of mother's highest education level 
###############################################################################

# Referring to the documentation here, c645 is the basic variable, c645a has recoded cases which gave no information as 1 (CSE/none), on the assumption this categroy will fit them best.

alspac_all %>% count(c645)
alspac_all %>% count(c645a)

# Note: the factor for 1 does not seem to match the codebook, which suggests it should be 'CSE/None', rather than just 'CSE', so I have edited this. 

class(alspac$c645a)
alspac <- alspac %>% mutate(
  c645a_edited = 
    case_when(c645a > 0 ~ as_factor(c645a)))
class(alspac$c645a_edited)
alspac %>% count(c645a_edited)

alspac <- alspac %>% mutate(
  c645a_edited = fct_recode(c645a_edited,
      "CSE/None" = "CSE"))
alspac %>% count(c645a_edited)

# Ok, there was no point, we're recoding to binary: degree vs no degree. 

alspac <- alspac %>% mutate(mother_education_age_gest = 
                              case_when(as.numeric(c645a) == 5 ~ 1, 
                                        as.numeric(c645a) < 5 & c645a >= 1 ~ 0))
alspac %>% count(mother_education_age_gest)
alspac %>% count(c645a, mother_education_age_gest)

###############################################################################
# Save out final data
###############################################################################

### Before dropping final data - remove:
 # control variables imported, to avoid clash with control datasets, 
 # actually, just keep only the new variables that are needed, and merge to original dataaset
new_vars_to_add <- alspac %>% select(uniqid, 
      adverse_life_event_age_1y_6m, adverse_life_event_age_2y_6m, adverse_life_event_age_3y_6m,
      adverse_life_event_age_4y_9m, adverse_life_event_age_5y_9m, adverse_life_event_age_8y_7m,
      family_maternal_bond_age_0y8m, family_maternal_bond_age_2y9m, 
      adverse_life_events_score_age_4_to_8, adverse_life_events_score_to_age_3, 
      family_partner_parenting_score_age_3y2m, family_mother_parenting_score_age_3y2m, 
      SDQ_total_difficulties_age_6y9m, 
      mother_education_age_gest, 
      family_maternal_bond_age_0y8m, family_maternal_bond_age_2y9m,
      social_cohesion_to_age_3y, social_discord_to_age_3y, 
      social_cohesion_age_5_to_7, social_discord_age_5_to_7, 
      friends_score_age_8_to_10, peer_problem_score_age_7_to_10,  
      friends_score_age_12_to_13, peer_problem_score_age_12_to_13)

names(new_vars_to_add)

# Re-open existing data  
existing_data <- readRDS("alspac_reduced.rds")

# Drop updated variables from existing data
existing_data <- existing_data %>% select(!c(family_partner_parenting_score_age_3y2m, family_mother_parenting_score_age_3y2m, family_maternal_bond_age_0y8m, family_maternal_bond_age_2y9m))

# Merge data
alspac <- left_join(existing_data, new_vars_to_add, by = "uniqid")

names(alspac %>% select(contains(".x")))
names(alspac %>% select(contains(".y")))

saveRDS(alspac, file = "alspac_reduced_v2.rds")

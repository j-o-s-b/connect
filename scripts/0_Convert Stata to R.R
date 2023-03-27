
library(haven)
library(tidyverse)

#Import Stata file in the R environment
alspac <- read_dta("filename.dta") #enter the name of the file received from the ALSPAC team

#Save out the stata file in a R format
saveRDS(alspac, file = "alspac.rds")



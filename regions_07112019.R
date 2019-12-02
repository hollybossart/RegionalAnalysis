# LOAD IN PACKAGES
library(survey)
library(tidyverse)

# CHANGE WORKING DIRECTORY
setwd("C:/Users/12088/Dropbox/UkraineREU2019/data/regions")

# LOAD IN DATA SET
data <- read.csv("ukrmain_RU_061419.csv")

# MENTAL HEALTH PREVALENCE
## See Chernobyl paper (Bolt 2018) for classification of these DSM diagnoses into four categories.
data <- data %>%
  mutate(anx = ifelse(DSM_AGO == 1|DSM_SO == 1|DSM_GAD == 1|DSM_PDS == 1, 1, 0),
         ied = ifelse(DSM_IEDH == 1, 1, 0),
         aff = ifelse(DSM_MDE == 1|DSM_DYS == 1, 1, 0),
         alc = ifelse(DSM_ALA == 1|DSM_ALD == 1, 1, 0),
         any_disorder = ifelse(anx == 1|ied == 1|aff == 1|alc == 1, 1, 0))

# EIGHT BARRINGTON REGIONS
## See CIDI question IO15 for oblast codification, and page 60 of Barrington paper for distinction of which oblast is in each region.
data <- data %>%
  mutate(east8 = ifelse(io15 == 7 | io15 == 13, 1, 0),
         eastcentral8 = ifelse(io15 == 21 | io15 == 10 | io15 == 6, 1, 0),
         krym8 = ifelse(io15 == 1, 1, 0),
         south8 = ifelse(io15 == 22 | io15 == 15 | io15 == 16, 1, 0),
         northcentral8 = ifelse(io15 == 26 | io15 == 19 | io15 == 17 | io15 == 24 | io15 == 12 | io15 == 2 | io15 == 3, 1, 0),
         westcentral8 = ifelse(io15 == 23 | io15 == 8 | io15 == 4 | io15 == 18 | io15 == 5, 1, 0),
         west8 = ifelse(io15 == 11 | io15 == 14 | io15 == 20, 1, 0),
         southwest8 = ifelse(io15 == 25 | io15 == 9, 1, 0))

# DEMOGRAPHICS
data <- data %>%
  mutate(rus_speak = ifelse(io14 == 1, 1, 0),
         
         # For type and size of pop area, Barrington uses an unclear criterion for 4 distinctions of size. S, M, L, XL
         vsmall = ifelse(io16 == 1, 1, 0),
         small = ifelse(io16 == 2, 1, 0),
         med = ifelse(io16 == 3, 1, 0),
         large = ifelse(io16 == 4, 1, 0),
         vlarge = ifelse(io16 == 5, 1, 0),
         
         # education (in dm4a_1 there is one person who refused -- they received a 0 for all three)
         # we can recategorize these later. This is just following Barrington.
         lower_ed = ifelse(de20_1 <= 3 | dm4a_1 <= 3, 1, 0),
         comp_sec_ed = ifelse(de20_1 == 4 | dm4a_1 == 4, 1, 0),
         some_higher_ed = ifelse( (de20_1 >= 5 & de20_1 <= 8) | (dm4a_1 >=5 & dm4a_1 <=8), 1, 0),
         
         lower_ed = ifelse(is.na(lower_ed), 0, lower_ed),
         comp_sec_ed = ifelse(is.na(comp_sec_ed), 0, comp_sec_ed),
         some_higher_ed = ifelse(is.na(some_higher_ed), 0, some_higher_ed),
         
         # unemployment
         umemp = ifelse(!is.na(dm2_3) | !is.na(em8_3), 1, 0),
         
         # economic standard 
         vlow_econ = ifelse(fnu4 == 1 | dmf12 == 1, 1, 0),
         low_econ = ifelse(fnu4 == 2 | fnu4 == 3 | dmf12 == 2 | dmf12 == 3, 1, 0),
         
         ## this takes care of the NA comparison problem in R by assigning 0s to the NAs
         ## we know with certainty that no one refused or didn't know for economic standing, so we can safely assume all NAs are from
         ## differences between part 1 and part 2 folks.
         vlow_econ = ifelse(is.na(vlow_econ), 0, vlow_econ),
         low_econ = ifelse(is.na(low_econ), 0, low_econ))
         
         # marital status - use variable marcoh

         # we can't use a religion question unless we subset to part 2 folks 

# using weight1 because we are using people from part 1 and part 2
weight <- svydesign(ids=~secu, strata =~strata, weights =~weight1, nest = T, data=subset(data, data$weight1 >0))
options(survey.lonely.psu = "adjust")
         

summary(svyglm(any_disorder ~ east8 + eastcentral8 + south8 + northcentral8 + westcentral8 + west8 + southwest8 + rus_speak + small + med + large + vlarge + lower_ed + comp_sec_ed + unemp + vlow_econ + low_econ + SEXM + marcoh + age, design = weight, family = quasibinomial("logit")))

         
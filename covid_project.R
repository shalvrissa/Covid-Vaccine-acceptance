library(tidyverse)
library(haven)
library(skimr)
library(gmodels)
pew_data <- read_sav("ATP W67.sav")

# Recode COVID_GOVTEST_W67 to only include Federal and State options
# ( 1 = Federal, 0 = State) and remove missing values. 
recode_pew <- pew_data %>% 
  mutate(COVID_GOVTEST_W67  = ifelse(COVID_GOVTEST_W67  == 1, 1, COVID_GOVTEST_W67),
         COVID_GOVTEST_W67  = ifelse(COVID_GOVTEST_W67  == 2, 1, COVID_GOVTEST_W67),
         COVID_GOVTEST_W67  = ifelse(COVID_GOVTEST_W67  == 3, 0, COVID_GOVTEST_W67),
         COVID_GOVTEST_W67  = ifelse(COVID_GOVTEST_W67  == 4, 0, COVID_GOVTEST_W67)) %>%
  filter(COVID_GOVTEST_W67 != 99 
         & COVIDEGFP_a_W67 != 99 
         & COVIDEGFP_b_W67 != 99 
         & COVIDEGFP_c_W67 != 99 
         & COVIDEGFP_d_W67 != 99) %>%
  select(COVID_GOVTEST_W67, 
         COVIDEGFP_a_W67, 
         COVIDEGFP_b_W67,
         COVIDEGFP_c_W67,
         COVIDEGFP_d_W67)

# Setting up Contingency tables
trump_table <- table(recode_pew$COVID_GOVTEST_W67, recode_pew$COVIDEGFP_a_W67) 
colnames(trump_table) = c("Excellent", "Good", "Fair", "Poor")
rownames(trump_table) = c("State", "Federal")

state_table <- table(recode_pew$COVID_GOVTEST_W67, recode_pew$COVIDEGFP_b_W67)
colnames(state_table) = c("Excellent", "Good", "Fair", "Poor")
rownames(state_table) = c("State", "Federal")

local_table <- table(recode_pew$COVID_GOVTEST_W67, recode_pew$COVIDEGFP_c_W67)
colnames(local_table) = c("Excellent", "Good", "Fair", "Poor")
rownames(local_table) = c("State", "Federal")

cdc_table <- table(recode_pew$COVID_GOVTEST_W67, recode_pew$COVIDEGFP_d_W67)
colnames(cdc_table) = c("Excellent", "Good", "Fair", "Poor")
rownames(cdc_table) = c("State", "Federal")

#Contingency tables with row proportions 
CrossTable(trump_table, 
           prop.r = TRUE, 
           prop.c = FALSE, 
           prop.t = FALSE, 
           prop.chisq = FALSE)
CrossTable(state_table, 
           prop.r = TRUE, 
           prop.c = FALSE, 
           prop.t = FALSE, 
           prop.chisq = FALSE)
CrossTable(local_table, 
           prop.r = TRUE, 
           prop.c = FALSE, 
           prop.t = FALSE, 
           prop.chisq = FALSE)
CrossTable(cdc_table, 
           prop.r = TRUE, 
           prop.c = FALSE, 
           prop.t = FALSE, 
           prop.chisq = FALSE)

#Chi-square tests
chisq.test(trump_table)  
chisq.test(state_table)  
chisq.test(local_table)  
chisq.test(cdc_table)  

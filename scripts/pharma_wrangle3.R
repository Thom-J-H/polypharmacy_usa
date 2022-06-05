#pharma_wrangle3

library(tidyverse)
library(here)



load(here::here("data", "tidy_data", "Age_Range_Sex_Tidy.rda"))

load(here::here("data", "tidy_data", "age_range_data_both.rda"))

load(here::here("data", "tidy_data", "pharma_tidy.rda"))

names(pharma_tidy)

names(Age_Range_Sex_Tidy)

names(Age_Range_Tidy)



Age_Range_Tidy <- Age_Range_Tidy %>%
  mutate(CL_low = Pop_Percent - (Std_Err * 1.96), 
         CL_high = Pop_Percent + (Std_Err * 1.96) ) %>%
  mutate(across(where(is.numeric), round, 2))


pharma_tidy <- pharma_tidy %>%
  mutate(CL_low = Pop_Percent - (Std_Err * 1.96), 
         CL_high = Pop_Percent + (Std_Err * 1.96) ) %>%
  mutate(across(where(is.numeric), round, 2))


names(pharma_tidy)

names(Age_Range_Sex_Tidy)

names(Age_Range_Tidy)



pharma_tidy$drug_use <- factor(pharma_tidy$drug_use ,
                               levels = c("At least one",
                                          "At least three",
                                          "At least five"))



Age_Range_Tidy$drug_use <- factor(Age_Range_Tidy$drug_use ,
                                  levels = c("At least one",
                                             "At least three",
                                             "At least five"))



Age_Range_Sex_Tidy$drug_use <- factor(Age_Range_Sex_Tidy$drug_use ,
                                      levels = c("At least one",
                                                 "At least three",
                                                 "At least five"))



jump_range <- c("1988–1994", "2015–2018")

jump_range2 <- c("1999–2002", "2015–2018")




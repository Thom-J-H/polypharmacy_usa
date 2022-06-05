library(tidyverse)
library(here)




# One All -----------------------------------------------------------------
## Load from raw_data





One_Age_Range$drug_use <- "At least one"

One_Age_Range_a <- One_Age_Range[ , c(1, seq(2, 22, by = 2))]

One_Age_Range_a_long <- One_Age_Range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")




One_Age_Range_b <- One_Age_Range[ , c(1, seq(3, 21, by = 2))]



One_Age_Range_b_long <- One_Age_Range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


One_Age_Range_a_long$Std_Err <- One_Age_Range_b_long $Std_Err

One_Age_Range_Tidy <- One_Age_Range_a_long

del_these <- c("One_Age_Range_a", "One_Age_Range_b"  , 
               "One_Age_Range_a_long", "One_Age_Range_b_long" )

rm(list = del_these )



# Three All ---------------------------------------------------------------
## Load from raw_data

Three_Age_Range$drug_use <- "At least three"

Three_Age_Range_a <- Three_Age_Range[ , c(1, seq(2, 22, by = 2))]

Three_Age_Range_a_long <- Three_Age_Range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Three_Age_Range_b <- Three_Age_Range[ , c(1, seq(3, 21, by = 2))]


Three_Age_Range_b_long <- Three_Age_Range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


Three_Age_Range_a_long$Std_Err <- Three_Age_Range_b_long $Std_Err

Three_Age_Range_Tidy <- Three_Age_Range_a_long

del_these <- c("Three_Age_Range_a", "Three_Age_Range_b"  , 
               "Three_Age_Range_a_long", "Three_Age_Range_b_long" )

rm(list = del_these )



# Five All ----------------------------------------------------------------
## Load from raw_data

Five_Age_range





Five_Age_range$drug_use <- "At least five"

Five_Age_range_a <- Five_Age_range[ , c(1, seq(2, 22, by = 2))]

Five_Age_range_a_long <- Five_Age_range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Five_Age_range_b <- Five_Age_range[ , c(1, seq(3, 21, by = 2))]


Five_Age_range_b_long <- Five_Age_range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


Five_Age_range_a_long$Std_Err <- Five_Age_range_b_long $Std_Err

Five_Age_range_Tidy <- Five_Age_range_a_long

del_these <- c("Five_Age_range_a", "Five_Age_range_b"  , 
               "Five_Age_range_a_long", "Five_Age_range_b_long" )

rm(list = del_these )


# Join --------------------------------------------------------------------
## Tidy

Age_Range_Tidy <- bind_rows(One_Age_Range_Tidy,
                            Three_Age_Range_Tidy, 
                            Five_Age_range_Tidy )

Age_Range_Tidy$Sex <- "Both"


age_range_data_both <- c("Age_Range_Tidy", "One_Age_Range_Tidy",
                         "Three_Age_Range_Tidy", "Five_Age_range_Tidy")

## Save and cleanup
save(list = age_range_data_both , file = here::here("data", 
                                                    "tidy_data",
                                                    "age_range_data_both.rda") )
cleanup <- ls()

rm(list = cleanup[c(1, 3:9)])





# By Biological Sex -------------------------------------------------------
## The above, stratified by sex


# One Male Ages -----------------------------------------------------------
## Load from raw_data


One_Male_Age_Range




One_Male_Age_Range$drug_use <- "At least one"

One_Male_Age_Range_a <- One_Male_Age_Range[ , c(1, seq(2, 22, by = 2))]

One_Male_Age_Range_a_long <- One_Male_Age_Range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



One_Male_Age_Range_b <- One_Male_Age_Range[ , c(1, seq(3, 21, by = 2))]


One_Male_Age_Range_b_long <- One_Male_Age_Range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


One_Male_Age_Range_a_long$Std_Err <- One_Male_Age_Range_b_long $Std_Err

One_Male_Age_Range_Tidy <- One_Male_Age_Range_a_long

del_these <- c("One_Male_Age_Range_a", "One_Male_Age_Range_b"  , 
               "One_Male_Age_Range_a_long", "One_Male_Age_Range_b_long",
               "One_Male_Age_Range")

rm(list = del_these )

One_Male_Age_Range_Tidy$Sex <- 'Male'


# One Female Ages ---------------------------------------------------------
## Load from raw_data
library(readxl)
One_Female_Age_Range <- read_excel("data/raw_data/One_Female_Age_Range.xlsx")


One_Female_Age_Range


One_Female_Age_Range$drug_use <- "At least one"

One_Female_Age_Range_a <- One_Female_Age_Range[ , c(1, seq(2, 22, by = 2))]

One_Female_Age_Range_a_long <- One_Female_Age_Range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



One_Female_Age_Range_b <- One_Female_Age_Range[ , c(1, seq(3, 21, by = 2))]


One_Female_Age_Range_b_long <- One_Female_Age_Range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


One_Female_Age_Range_a_long$Std_Err <- One_Female_Age_Range_b_long $Std_Err

One_Female_Age_Range_Tidy <- One_Female_Age_Range_a_long

del_these <- c("One_Female_Age_Range_a", "One_Female_Age_Range_b"  , 
               "One_Female_Age_Range_a_long", "One_Female_Age_Range_b_long",
               "One_Female_Age_Range")

rm(list = del_these )

One_Female_Age_Range_Tidy$Sex <- 'Female'


# Three_Male_Ages ---------------------------------------------------------
## Load from raw_data


Three_Male_Age_Range


Three_Male_Age_Range$drug_use <- "At least three"

Three_Male_Age_Range_a <- Three_Male_Age_Range[ , c(1, seq(2, 22, by = 2))]

Three_Male_Age_Range_a_long <- Three_Male_Age_Range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Three_Male_Age_Range_b <- Three_Male_Age_Range[ , c(1, seq(3, 21, by = 2))]


Three_Male_Age_Range_b_long <- Three_Male_Age_Range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


Three_Male_Age_Range_a_long$Std_Err <- Three_Male_Age_Range_b_long $Std_Err

Three_Male_Age_Range_Tidy <- Three_Male_Age_Range_a_long

del_these <- c("Three_Male_Age_Range_a", "Three_Male_Age_Range_b"  , 
               "Three_Male_Age_Range_a_long", "Three_Male_Age_Range_b_long",
               "Three_Male_Age_Range")

rm(list = del_these )

Three_Male_Age_Range_Tidy$Sex <- 'Male'



# Three Female Ages -------------------------------------------------------

Three_Female_Age_Range <- read_excel("data/raw_data/Three_Female_Age_Range.xlsx")

Three_Female_Age_Range



Three_Female_Age_Range$drug_use <- "At least three"

Three_Female_Age_Range_a <- Three_Female_Age_Range[ , c(1, seq(2, 22, by = 2))]

Three_Female_Age_Range_a_long <- Three_Female_Age_Range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Three_Female_Age_Range_b <- Three_Female_Age_Range[ , c(1, seq(3, 21, by = 2))]


Three_Female_Age_Range_b_long <- Three_Female_Age_Range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


Three_Female_Age_Range_a_long$Std_Err <- Three_Female_Age_Range_b_long $Std_Err

Three_Female_Age_Range_Tidy <- Three_Female_Age_Range_a_long

del_these <- c("Three_Female_Age_Range_a", "Three_Female_Age_Range_b"  , 
               "Three_Female_Age_Range_a_long", "Three_Female_Age_Range_b_long",
               "Three_Female_Age_Range")

rm(list = del_these )

Three_Female_Age_Range_Tidy$Sex <- 'Female'




# Five Male Ages ----------------------------------------------------------
## Load from raw_data


Five_Male_Age_Range



Five_Male_Age_Range$drug_use <- "At least five"

Five_Male_Age_Range_a <- Five_Male_Age_Range[ , c(1, seq(2, 22, by = 2))]

Five_Male_Age_Range_a_long <- Five_Male_Age_Range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Five_Male_Age_Range_b <- Five_Male_Age_Range[ , c(1, seq(3, 21, by = 2))]


Five_Male_Age_Range_b_long <- Five_Male_Age_Range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


Five_Male_Age_Range_a_long$Std_Err <- Five_Male_Age_Range_b_long $Std_Err

Five_Male_Age_Range_Tidy <- Five_Male_Age_Range_a_long

del_these <- c("Five_Male_Age_Range_a", "Five_Male_Age_Range_b"  , 
               "Five_Male_Age_Range_a_long", "Five_Male_Age_Range_b_long",
               "Five_Male_Age_Range")

rm(list = del_these )

Five_Male_Age_Range_Tidy$Sex <- 'Male'




# Five Female Ages --------------------------------------------------------
## Load from raw_data

Five_Female_Age_Range <- read_excel("data/raw_data/Five_Female_Age_Range.xlsx")


Five_Female_Age_Range$drug_use <- "At least five"

Five_Female_Age_Range_a <- Five_Female_Age_Range[ , c(1, seq(2, 22, by = 2))]

Five_Female_Age_Range_a_long <- Five_Female_Age_Range_a %>%
  pivot_longer(cols = 2:11, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Five_Female_Age_Range_b <- Five_Female_Age_Range[ , c(1, seq(3, 21, by = 2))]


Five_Female_Age_Range_b_long <- Five_Female_Age_Range_b %>%
  pivot_longer(cols = 2:11, 
               names_to = "Error_years",
               values_to = "Std_Err")


Five_Female_Age_Range_a_long$Std_Err <- Five_Female_Age_Range_b_long $Std_Err

Five_Female_Age_Range_Tidy <- Five_Female_Age_Range_a_long

del_these <- c("Five_Female_Age_Range_a", "Five_Female_Age_Range_b"  , 
               "Five_Female_Age_Range_a_long", "Five_Female_Age_Range_b_long",
               "Five_Female_Age_Range")

rm(list = del_these )

Five_Female_Age_Range_Tidy$Sex <- 'Female'


# Join --------------------------------------------------------------------



Male_Age_Range_Tidy <- bind_rows(One_Male_Age_Range_Tidy,
                                 Three_Male_Age_Range_Tidy,
                                Five_Male_Age_Range_Tidy)




Female_Age_Range_Tidy <- bind_rows(One_Female_Age_Range_Tidy,
                                 Three_Female_Age_Range_Tidy,
                                 Five_Female_Age_Range_Tidy)

##
##  Join again
##


Age_Range_Sex_Tidy <- bind_rows(Female_Age_Range_Tidy,
                                Male_Age_Range_Tidy )

## Calculate CL and round off

Age_Range_Sex_Tidy <- Age_Range_Sex_Tidy %>%
  mutate(CL_low = Pop_Percent - (Std_Err * 1.96), 
         CL_high = Pop_Percent + (Std_Err * 1.96) ) %>%
  mutate(across(where(is.numeric), round, 2))
                                
##  SAVE!


save(Age_Range_Sex_Tidy , 
     file = here::here("data", "tidy_data", "Age_Range_Sex_Tidy.rda"))

back_ups <- c("Male_Age_Range_Tidy", "Female_Age_Range_Tidy")

save(list = back_ups , 
     file = here::here("data", "tidy_data", "Age_Range_Sex_Tidy_backup.rda"))


# Write CSV versions ------------------------------------------------------

write_csv(Age_Range_Sex_Tidy , 
          file = here::here("data", "tidy_data",
                            "csv_ver", "Age_Range_Sex_Tidy.csv") )


write_csv(pharma_tidy , 
          file = here::here("data", "tidy_data",
                            "csv_ver", "pharma_tidy.csv") )




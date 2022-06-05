library(tidyverse)
library(here)






# One Both Sexes ----------------------------------------------------------
## Load from raw_data

Atleastone$drug_use <- "At least one"


Atleastone_a <- Atleastone[ , seq(1, 21, by = 2)]


Atleastone_a$Sex <- ""
Atleastone_a$Sex[1] <-"Both"
Atleastone_a$Sex[2] <-"Male"
Atleastone_a$Sex[3] <-"Female"





Atleastone_a_long <- Atleastone_a %>%
  pivot_longer(cols = 1:10, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Atleastone_b <- Atleastone[ , seq(2, 20, by = 2)]


Atleastone_b$Sex <- ""
Atleastone_b$Sex[1] <-"Both"
Atleastone_b$Sex[2] <-"Male"
Atleastone_b$Sex[3] <-"Female"




Atleastone_b_long <- Atleastone_b %>%
  pivot_longer(cols = 1:10, 
               names_to = "Error_years",
               values_to = "Std_Err")

Atleastone_a_long$Std_Err <- Atleastone_b_long$Std_Err




# Three Both Sexes --------------------------------------------------------
## Load from raw_data



Atleastthree

Atleastthree$drug_use <- "At least three"

Atleastthree_a <- Atleastthree[ , seq(1, 21, by = 2)]


Atleastthree_a$Sex <- ""
Atleastthree_a$Sex[1] <-"Both"
Atleastthree_a$Sex[2] <-"Male"
Atleastthree_a$Sex[3] <-"Female"



Atleastthree_a_long <- Atleastthree_a %>%
  pivot_longer(cols = 1:10, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Atleastthree_b <- Atleastthree[ , seq(2, 20, by = 2)]


Atleastthree_b$Sex <- ""
Atleastthree_b$Sex[1] <-"Both"
Atleastthree_b$Sex[2] <-"Male"
Atleastthree_b$Sex[3] <-"Female"



Atleastthree_b_long <- Atleastthree_b %>%
  pivot_longer(cols = 1:10, 
               names_to = "Error_years",
               values_to = "Std_Err")

Atleastthree_a_long$Std_Err <- Atleastthree_b_long$Std_Err




# Five Both Sexes ---------------------------------------------------------
## Load from raw_data


Atleastfive

Atleastfive$drug_use <- "At least five"

Atleastfive_a <- Atleastfive[ , seq(1, 21, by = 2)]


Atleastfive_a$Sex <- ""
Atleastfive_a$Sex[1] <-"Both"
Atleastfive_a$Sex[2] <-"Male"
Atleastfive_a$Sex[3] <-"Female"



Atleastfive_a_long <- Atleastfive_a %>%
  pivot_longer(cols = 1:10, 
               names_to = "Year_Range",
               values_to = "Pop_Percent")



Atleastfive_b <- Atleastfive[ , seq(2, 20, by = 2)]


Atleastfive_b$Sex <- ""
Atleastfive_b$Sex[1] <-"Both"
Atleastfive_b$Sex[2] <-"Male"
Atleastfive_b$Sex[3] <-"Female"



Atleastfive_b_long <- Atleastfive_b %>%
  pivot_longer(cols = 1:10, 
               names_to = "Error_years",
               values_to = "Std_Err")

Atleastfive_a_long$Std_Err <- Atleastfive_b_long$Std_Err



# Tidy and Combine --------------------------------------------------------
## Bind Row & Add CI 


pharma_tidy <- bind_rows(Atleastone_a_long, 
                         Atleastthree_a_long,
                         Atleastfive_a_long )


pharma_tidy <- pharma_tidy %>%
  mutate(CL_low = Pop_Percent - (Std_Err * 1.96),
         CL_high = Pop_Percent + (Std_Err * 1.96))

pharma_tidy <- pharma_tidy  %>%
  mutate(across(where(is.numeric), round, 2))
  
  
save(pharma_tidy, file = here::here("data", "tidy_data", "pharma_tidy.rda"))

# save.image("~/R_STUDIO/Misc_Sub/data/raw_data/pharma_mess_all.RData")


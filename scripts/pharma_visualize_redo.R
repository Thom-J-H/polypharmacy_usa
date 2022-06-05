
# Pharma visualize one: by Year Range, and Biological Sex ---------------------------

# Pharma visualize two: by Year Range, Age Group, and Biological Sex ----------------


#### Data Source 

## Table 39. Prescription drug use in the past 30 days, 
## by sex, race and Hispanic origin, and age: 
## United States, selected years
## 1988–1994 through 2015–2018

### https://www.cdc.gov/nchs/data/hus/2019/039-508.pdf

### https://www.cdc.gov/nchs/hus/contents2019.htm#Table-039



# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
library(glue)



# Data and touch up -------------------------------------------------------

load(here::here("data", "tidy_data", "Pharma_Data.RData"))




# Strategy for Plotting ---------------------------------------------------

## By Usage: 1+, 3+, 5+
## General -- both sexes
## By Sex



# At least one ------------------------------------------------------------

## Both sexes
both_one <- pharma_tidy %>%
  filter(drug_use == "At least one" , Sex == "Both") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Person used at least one (1) prescription drug in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.2) +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5)) 


both_one



pharma_tidy %>%
  filter(drug_use == "At least one" , Sex == "Both") %>%
  select(Year_Range, Pop_Percent) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent")


## By Sex
sex_one <- pharma_tidy %>%
  filter(drug_use == "At least one" , Sex != "Both") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Person used at least one (1) prescription drug in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.2) 


sex_one



## By Age 

sex_one_18  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "Under 18 years", 
         drug_use == "At least one") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group under 18: At least one prescription used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 


sex_one_18







sex_one_44 <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "18–44 years", 
         drug_use == "At least one") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 18–44: At least one prescription used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_one_44







sex_one_64 <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years",
         drug_use == "At least one") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 80, by = 10))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 45–64: At least one prescription used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_one_64




sex_one_65plus <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         drug_use == "At least one") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 100, by = 10))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 65 & over: At least one prescription used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_one_65plus 








# At least three ----------------------------------------------------------



## Both sexes
both_three <- pharma_tidy %>%
  filter(drug_use == "At least three" , Sex == "Both") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  facet_wrap(~Sex)  +
  scale_fill_gradient(low = "blue",  high = "red") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Person used at least three (3) prescription drug in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.2) 


both_three




## By Sex
sex_three <- pharma_tidy %>%
  filter(drug_use == "At least three" , Sex != "Both") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  facet_wrap(~Sex) +
  scale_fill_gradient(low = "blue",  high = "red") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Person used at least three (3) prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)" ) +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.2) 

sex_three





## By Age 

sex_three_18  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "Under 18 years", 
         drug_use == "At least three") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group under 18: At least three prescriptions used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 


sex_three_18







sex_three_44 <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "18–44 years", 
         drug_use == "At least three") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 18–44: At least three prescriptions used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_three_44







sex_three_64 <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years",
         drug_use == "At least three") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 80, by = 10))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 45–64: At least three prescriptions used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_three_64




sex_three_65plus <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         drug_use == "At least three") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 100, by = 10))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 65 & over: At least three prescriptions used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_three_65plus 






# At least five -----------------------------------------------------------

## All Sexes
both_five <- pharma_tidy %>%
  filter(drug_use == "At least five", Sex == "Both") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                     breaks = seq(0,14, by = 2))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Person used at least five (5) prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.2) 



both_five 




## By Sex
sex_five <- pharma_tidy %>%
  filter(drug_use == "At least five", Sex != "Both") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                     breaks = seq(0, 15, by = 3))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_wrap(~Sex)  +
  labs(y = " Percentage of USA Population", x = "",
       title = "Person used at least five (5) prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.2) 



sex_five 





## By Age 

sex_five_18  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "Under 18 years", 
         drug_use == "At least five") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group under 18: At least five prescriptions used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 


sex_five_18







sex_five_44 <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "18–44 years", 
         drug_use == "At least five") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 18–44: At least five prescriptions used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_five_44







sex_five_64 <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years",
         drug_use == "At least five") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 80, by = 10))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 45–64: At least five prescriptions used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_five_64




sex_five_65plus <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         drug_use == "At least five") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 100, by = 10))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~.) +
  labs(y = " Percentage of USA Population", x = "",
       title = "Age Group 65 & over: At least five prescriptions used in past 30 days.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.1) 

sex_five_65plus 




# Jump Ranges for 1 - 3 - 5 -----------------------------------------------



jump_both <- pharma_tidy %>%
  filter(Sex == "Both", Year_Range %in% jump_range)  %>%
  ggplot(aes(x = drug_use, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_wrap(~Year_Range) +
  labs(x = "Prescripts (#)", y = " Percentage of USA Population",
       title = "Person used prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.2) 




jump_both


jump_sex <- pharma_tidy %>%
  filter(Sex != "Both", Year_Range %in% jump_range)  %>%
  ggplot(aes(x = drug_use, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Sex ~ Year_Range) +
  labs(x = "Prescripts (#)", y = " Percentage of USA Population",
       title = "Person used prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =  Pop_Percent), size = 3, 
             color= "white",  hjust = 1.2) 


jump_sex 


save.image("~/R_STUDIO/Misc_Sub/data/tidy_data/Pharma_VIZ.RData")


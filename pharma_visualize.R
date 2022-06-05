
# Pharma visualize one: by Year Range, and Biological Sex -----------------

#### Data Source 

## Table 39. Prescription drug use in the past 30 days, 
## by sex, race and Hispanic origin, and age: 
## United States, selected years
## 1988–1994 through 2015–2018

### https://www.cdc.gov/nchs/data/hus/2019/039-508.pdf

### https://www.cdc.gov/nchs/hus/contents2019.htm#Table-039

####




# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
library(glue)



# Data and touch up -------------------------------------------------------


load(here::here("data", "tidy_data", "pharma_tidy.rda"))

names(pharma_tidy)

pharma_tidy$drug_use <- factor(pharma_tidy$drug_use ,
                                 levels = c("At least one",
                                            "At least three",
                                            "At least five"))



save.image("~/R_STUDIO/Misc_Sub/data/tidy_data/Pharma_Data.RData")







# At least one ------------------------------------------------------------


## All Sexes
both_one <- pharma_tidy %>%
  filter(drug_use == "At least one" , Sex == "Both") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 55, by = 5))+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Person used at least one (1) prescription drug in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.3) 


both_one



## By Sex
gender_one <- pharma_tidy %>%
  filter(drug_use == "At least one" , Sex != "Both") %>%
  ggplot(aes(x = Year_Range, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  facet_wrap(~Sex) +
  scale_fill_gradient(low = "blue",  high = "red") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Person used at least one (1) prescription drug in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.3) 



gender_one




# Three or more -----------------------------------------------------------


## All Sexes
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
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.3) 



## By Sex
gender_three <- pharma_tidy %>%
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
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.3) 




# Five or more ------------------------------------------------------------


## All Sexes
both_five <- pharma_tidy %>%
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
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.3) 



## By Sex
gender_five <- pharma_tidy %>%
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
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.3) 




# Jump Graphs -------------------------------------------------------------
## Let's emphasize changes



jump_range <- c("1988–1994", "2015–2018")

## All Sexes
jump_both <- pharma_tidy %>%
  filter(Sex == "Both", Year_Range %in% jump_range)  %>%
  ggplot(aes(x = drug_use, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_wrap(~Year_Range) +
  labs(x = "Past 30 Days", y = " Percentage of USA Population",
       title = "Person used prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.3) 

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
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.1) 


jump_sex 



pharma_tidy %>%
  filter(Year_Range %in% jump_range)  %>%
  ggplot(aes(x = drug_use, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Year_Range ~ Sex) +
  labs(x = "Past 30 Days", y = " Percentage of USA Population",
       title = "Biological male used prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.1) 



## By Sex :: Male
jump_male <- pharma_tidy %>%
  filter(Sex == "Male", Year_Range %in% jump_range)  %>%
  ggplot(aes(x = drug_use, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_wrap(~Year_Range) +
  labs(x = "Past 30 Days", y = " Percentage of USA Population",
       title = "Biological male used prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.1) 



## By Sex :: Female
jump_female <- pharma_tidy %>%
  filter(Sex == "Female", Year_Range %in% jump_range)  %>%
  ggplot(aes(x = drug_use, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_wrap(~Year_Range) +
  labs(x = "Past 30 Days", y = " Percentage of USA Population",
       title = "Biological female used prescription drugs in past 30 days",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.1) 



## Data table 
jump_chart <- pharma_tidy %>%
  filter(Year_Range %in% jump_range) %>% 
  group_by(drug_use, Sex, Year_Range) %>% 
  summarize(Pop_Percent = Pop_Percent)

## Let's get the differences
jump_chart_early <- jump_chart %>% filter(Year_Range == "1988–1994")
jump_chart_late <- jump_chart %>% filter(Year_Range != "1988–1994")


## Here?
jump_chart_late$Diff <- jump_chart_late$Pop_Percent - jump_chart_early$Pop_Percent



## No, add them back to main Jump data
diff_vec <- c(-6.6, 6.6, -4.5, 4.5, -9.0, 9.0, -9.5, 9.5, 
              -8.7, 8.7,  -10.6 ,10.6 , -7.2, -7.2, -6.7, 6.7 , -7.9, 7.9)

jump_chart$Diff <- diff_vec 



## Difference Plot
diff_plot <- jump_chart_late %>% 
  filter(Sex != "Both") %>% 
  ggplot( aes(x = drug_use, y = Diff, fill = Sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("green", "purple") ) +
  labs(title = "Change from 1988–1994 to 2015–2018 in Prescription Drug Usage",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)" ,
       x = "In the past 30 days, number used ...", 
       y = "Increase in Population %") 



 jump_chart_late %>% 
  filter(Sex != "Both") %>% 
  ggplot( aes(x = drug_use, y = Diff, fill = Diff)) +
  geom_col(position = "dodge") +
   scale_fill_gradient(low = "blue",  high = "red") +
   facet_grid(Sex ~.)+ 
   guides(fill = "none") +
   coord_flip() +
  labs(title = "Change from 1988–1994 to 2015–2018 in Prescription Drug Usage",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)" ,
       x = "In the past 30 days, number used ...", 
       y = "Increase in Population %") 
 
 
 
 
# See the jump tables -----------------------------------------------------

library(reactable)


## All sexes
jump_chart %>% 
  filter(Sex == "Both")  %>% 
  reactable(., highlight = TRUE, 
            striped = TRUE,
            pageSizeOptions = c(10, 25, 50, 100),
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 


## By Sex
jump_chart %>% 
  filter(Sex != "Both")  %>% 
  reactable(., highlight = TRUE, 
            striped = TRUE,
            pageSizeOptions = c(12),
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 


## Clean up a bit
jump_chart %>% 
  rename(Drug_Use = drug_use) %>% 
  filter(Year_Range == "2015–2018",
         Sex == "Both")  %>% 
  reactable(., highlight = TRUE, 
            striped = TRUE,
            pageSizeOptions = c(10, 25, 50, 100),
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 


## Clean up a bit again
jump_chart %>% 
  rename(Drug_Use = drug_use) %>% 
  filter(Year_Range == "2015–2018",
         Sex != "Both")  %>% 
  reactable(., highlight = TRUE, 
            striped = TRUE,
            pageSizeOptions = c(10, 25, 50, 100),
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 






# List of ggplots ---------------------------------------------------------


both_one 

gender_one


both_three

gender_three


both_five

gender_five


jump_both 

jump_female 

jump_male 


diff_plot



# Save --------------------------------------------------------------------


pharma_one_results  <- c("both_five", "both_one" ,"both_three" ,"diff_plot",
                  "gender_five", "gender_one","gender_three",
                  "jump_both", "jump_male", "jump_female",
                  "jump_chart" , "pharma_tidy")

save(list = pharma_one_results, file = here::here("data", 
                                                  "tidy_data", 
                                                  "pharma_one_results.rda"))
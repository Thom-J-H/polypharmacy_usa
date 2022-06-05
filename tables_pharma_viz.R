
library(tidyverse)
library(here)
library(reactable)


pharma_tidy$drug_use <- factor(pharma_tidy$drug_use ,
                                       levels = c("At least one",
                                                  "At least three",
                                                  "At least five"))

jump_range <- c("1988–1994", "2015–2018")

jump_range2 <- c("1999–2002", "2015–2018")

one_all_table <- pharma_tidy %>%
  filter(drug_use == "At least one" , Sex == "Both") %>%
  select(Year_Range, Pop_Percent) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 


one_all_table %>%
  reactable(., highlight = TRUE, 
          striped = TRUE,
          compact = TRUE,
          theme = reactableTheme(
            stripedColor = "#EDEDED",
            highlightColor = "#FFE4E1") ) 


one_all_table 






one_sex_table  <- pharma_tidy %>%
  filter(drug_use == "At least one" , Sex != "Both") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 



one_sex_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )

one_sex_table



one_sex_18_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "Under 18 years", 
         drug_use == "At least one") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

one_sex_18_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )

one_sex_18_table




one_sex_44_table <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "18–44 years", 
         drug_use == "At least one") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

one_sex_44_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )


one_sex_44_table 





one_sex_64_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years",
         drug_use == "At least one") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent")

one_sex_64_table  %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )


one_sex_64_table 







one_sex_65_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         drug_use == "At least one") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

one_sex_65_table  %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )

one_sex_65_table 



# At least three ----------------------------------------------------------



three_all_table <- pharma_tidy %>%
  filter(drug_use == "At least three" , Sex == "Both") %>%
  select(Year_Range, Pop_Percent) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent")

three_all_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 


three_all_table 


three_sex_table  <- pharma_tidy %>%
  filter(drug_use == "At least three" , Sex != "Both") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

three_sex_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )

three_sex_table



three_sex_18_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "Under 18 years", 
         drug_use == "At least three") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

three_sex_18_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )

three_sex_18_table




three_sex_44_table <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "18–44 years", 
         drug_use == "At least three") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

three_sex_44_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )


three_sex_44_table 





three_sex_64_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years",
         drug_use == "At least three") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

three_sex_64_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )


three_sex_64_table 




three_sex_65_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         drug_use == "At least three") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

three_sex_65_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )


three_sex_65_table 


# At least five -----------------------------------------------------------




five_all_table <- pharma_tidy %>%
  filter(drug_use == "At least five" , Sex == "Both") %>%
  select(Year_Range, Pop_Percent) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 


five_all_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 


five_all_table 


five_sex_table  <- pharma_tidy %>%
  filter(drug_use == "At least five" , Sex != "Both") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

five_sex_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )

five_sex_table



five_sex_18_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "Under 18 years", 
         drug_use == "At least five") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

five_sex_18_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )

five_sex_18_table




five_sex_44_table <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "18–44 years", 
         drug_use == "At least five") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

five_sex_44_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )


five_sex_44_table 





five_sex_64_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years",
         drug_use == "At least five") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

five_sex_64_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )


five_sex_64_table 





five_sex_65_table  <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         drug_use == "At least five") %>%
  select(Sex, Year_Range, Pop_Percent) %>%
  arrange(Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

five_sex_65_table %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )



five_sex_65_table 




# Jump Table --------------------------------------------------------------



jump_all_table <- pharma_tidy %>%
  filter(Sex == "Both", Year_Range %in% jump_range) %>%
  rename(Prescripts = drug_use) %>%
  select(Prescripts, Year_Range, Pop_Percent) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent")

jump_all_table2 <-  jump_all_table %>%
  mutate(Delta = `2015–2018` - `1988–1994`,
         Ratio = `2015–2018` /`1988–1994`) %>%
  mutate(across(where(is.numeric), round, 2)) 

jump_all_table2 %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )

jump_all_table 

jump_all_table2 

jump_sex_table <- pharma_tidy %>%
  filter(Sex != "Both", Year_Range %in% jump_range) %>%
  rename(Prescripts = drug_use) %>%
  select(Prescripts, Sex, Year_Range, Pop_Percent) %>%
  arrange(Prescripts, Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 

jump_sex_table2 <-  jump_sex_table  %>%
  mutate(Delta = `2015–2018` - `1988–1994`,
         Ratio = `2015–2018` /`1988–1994`) %>%
  mutate(across(where(is.numeric), round, 2)) 

jump_sex_table2 %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            compact = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") )


pharma_delta <- pharma_tidy %>%
  filter(Sex != "Both", Year_Range %in% jump_range) %>%
  rename(Prescripts = drug_use) %>%
  select(Prescripts, Sex, Year_Range, Pop_Percent) %>%
  arrange(Prescripts, Sex) %>%
  pivot_wider(names_from = "Year_Range", values_from = "Pop_Percent") 


pharma_delta$Prescripts <- factor(pharma_delta$Prescripts ,
                                  levels = c("At least one",
                                             "At least three",
                                             "At least five"))


pdelta <- (pharma_delta[, 4] - pharma_delta[, 3]) %>% pull()

pratio <- (pharma_delta[, 4] / pharma_delta[, 3]) %>% pull()

pharma_delta$Delta <- pdelta

pharma_delta$Ratio <- pratio 



pharma_delta <- pharma_delta %>%
  mutate(across(where(is.numeric), round, 2)) 

pharma_delta %>%
  arrange(Prescripts) %>%
  knitr::kable(., caption = "Delta and Ratio")





# List tables -------------------------------------------------------------

jump_all_table
jump_all_table2

jump_sex_table 
jump_sex_table2 

one_all_table

one_sex_table
one_sex_18_table 
one_sex_44_table 
one_sex_64_table 
one_sex_65_table 


three_all_table

three_sex_table

three_sex_18_table 

three_sex_44_table 

three_sex_64_table 

three_sex_65_table 



five_all_table

five_sex_table

five_sex_18_table 

five_sex_44_table 

five_sex_64_table 

five_sex_65_table 



tab_dash <- c("jump_all_table",
              "jump_all_table2",
              'jump_sex_table',  
              'jump_sex_table2',
              "one_sex_table",
              "one_sex_18_table", 
              "one_sex_44_table", 
              "one_sex_64_table",  
              "one_sex_65_table",
              "three_sex_table",
              "three_sex_18_table", 
              "three_sex_44_table", 
              "three_sex_64_table",  
              "three_sex_65_table",
              "five_sex_table",
              "five_sex_18_table", 
              "five_sex_44_table", 
              "five_sex_64_table",  
              "five_sex_65_table",
              "one_all_table",'three_all_table',
              "five_all_table",
              "pharma_delta")



save(list = tab_dash, 
     file = here::here("data", "tidy_data", "pharma_tabs.rda"))
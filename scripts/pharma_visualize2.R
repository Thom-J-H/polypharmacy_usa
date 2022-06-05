
# Pharma visualize two: by Year Range, Age Group, and Biological Sex -----------------

#### Data Source 

## Table 39. Prescription drug use in the past 30 days, 
## by sex, race and Hispanic origin, and age: 
## United States, selected years
## 1988–1994 through 2015–2018

### https://www.cdc.gov/nchs/data/hus/2019/039-508.pdf

### https://www.cdc.gov/nchs/hus/contents2019.htm#Table-039

####


library(tidyverse)
library(here)
library(glue)



# Data and touch up -------------------------------------------------------


load(here::here("data", "tidy_data", "Age_Range_Sex_Tidy.rda"))

load(here::here("data", "tidy_data", "age_range_data_both.rda"))



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

# Begin plots -------------------------------------------------------------

names(Age_Range_Sex_Tidy)

unique(Age_Range_Sex_Tidy$Age_Range)
#  "Under 18 years"  "18–44 years"  "45–64 years" "65 years and over"


unique(Age_Range_Sex_Tidy$Year_Range)
# 1988–1994" "1999–2002" "2001–2004" "2003–2006" "2005–2008"
#"2007–2010" "2009–2012" "2011–2014" "2013–2016" "2015–2018"




# Under 18 years ----------------------------------------------------------


# Fill by drug usage ------------------------------------------------------


Age_Range_Tidy %>%
  filter(Age_Range == "Under 18 years" ) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = "none") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Prescription drug use past 30 days. Age Group: Under 18 years old",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") 




# Jump Fill by Usage ------------------------------------------------------



Age_Range_Tidy %>%
  filter(Age_Range == "Under 18 years",
         Year_Range %in% jump_range2) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = "none") +
  labs(y = "", x = "Difference after 16 Years",
       title = "Prescription drug use past 30 days. Age Group: Under 18 years old",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)")





# Jump Fill Gender --------------------------------------------------------


Age_Range_Sex_Tidy %>%
  filter(Age_Range == "Under 18 years",
         Year_Range %in% jump_range2) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(y = NULL, x = NULL, 
       title = "Prescription drug use past 30 days. Age Group: Under 18 years old",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)",
       fill = "Prescripts (#):") +
  facet_wrap(~Sex) +
  theme(legend.position = "bottom" )





grid_jump2 <- Age_Range_Sex_Tidy %>%
  filter(Age_Range == "Under 18 years",
         Year_Range %in% jump_range2) %>%
  ggplot(aes(x = drug_use, y = Pop_Percent, fill =  Pop_Percent) )  +
  geom_col() +
  guides(fill = "none")  +
  scale_y_continuous(labels = scales::label_percent(scale = 1) )+
  coord_flip() +
  scale_fill_gradient(low = "blue",  high = "red") +
  facet_grid(Year_Range ~ Sex) +
  labs(x = "Past 30 Days", y = " Percentage of USA Population",
       title = "Prescription drug use past 30 days. Age Group: Under 18 years old.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") +
  geom_text( aes(label =   paste0({Pop_Percent},"%")), size = 3, 
             color= "white",  hjust = 1.1) 

# 18-44 years -------------------------------------------------------------



Age_Range_Tidy %>%
  filter(Age_Range == "18–44 years" ) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = "none") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Prescription drug use past 30 days. Age Group: 18–44 years.",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") 


Age_Range_Tidy %>%
  filter(Age_Range == "18–44 years" ,
         Year_Range %in% jump_range) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = "none") +
  labs(y = "Percentage of US population", 
       x = "Difference after 21 Years",
       title = "Prescription drug use past 30 days. Age Group: 18–44 years",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)")




Age_Range_Sex_Tidy %>%
  filter(Age_Range == "18–44 years", Sex == "Female" ) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(       title = "Prescription drug use past 30 days. Sex: Female. Age Group: 18–44 years",
              x = "",
       caption ="Data Humanist, CC0 (Public Domain)",
       y = "Source: cdc.gov/nchs/hus/contents2019.htm#Table-039",
       fill = "Scripts:") 





Age_Range_Sex_Tidy %>%
  filter(Age_Range == "18–44 years" ,
         Year_Range %in% jump_range) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(y = NULL, x = NULL, 
       title = "Prescription drug use past 30 days. Age Group: 18–44 years",
       caption ="Data Humanist, CC0 (Public Domain)",
       subtitle = "Source: cdc.gov/nchs/hus/contents2019.htm#Table-039",
       fill = "Prescripts (#):") +
  facet_wrap(~Sex) +
  theme(legend.position = "bottom" )




# Age 45–64 years ---------------------------------------------------------


Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years") %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = "none") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Prescription drug use past 30 days. Age Group: 45–64 years",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") 


Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years",
         Year_Range %in% jump_range) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = "none") +
  labs(y = "", x = "Difference after 21 Years",
       title = "Prescription drug use past 30 days. Age Group: 45–64 years",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)")



Age_Range_Sex_Tidy %>%
  filter(Age_Range == "45–64 years",
         Year_Range %in% jump_range ) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(y = NULL, x = NULL,
       title = "Prescription drug use past 30 days. Age Group: 45–64 years",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)", 
       fill = "Prescripts (#):") +
  facet_wrap(~Sex) +
  theme(legend.position = "bottom" )




# 65 years and over -------------------------------------------------------


Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over") %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = "none") +
  labs(y = " Percentage of USA Population", x = "",
       title = "Prescription drug use past 30 days. Age Group: 65 years and over",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)") 


Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         Year_Range %in% jump_range) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = "none") +
  labs(y = "", x = "Difference after 21 Years",
       title = "Prescription drug use past 30 days. Age Group: 65 years and over",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)")



Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         Year_Range %in% jump_range ) %>%
  ggplot( aes(x = Year_Range, y = Pop_Percent, fill =  drug_use)) +
  geom_col( position = "dodge") +
  facet_wrap(~ drug_use, scales = "free") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(y = NULL, x = NULL,
       title = "Prescription drug use past 30 days. Age Group: 65 years and over",
       subtitle = 'Source: cdc.gov/nchs/hus/contents2019.htm#Table-039',
       caption ="Data Humanist, CC0 (Public Domain)", 
       fill = "Prescripts (#):") +
  facet_wrap(~Sex) +
  theme(legend.position = "bottom" )


Age_Range_Sex_Tidy %>%
  filter(Age_Range == "65 years and over",
         Year_Range %in% jump_range ) %>%
  group_by(Sex) %>%
  select(everything()  )


# Hung-Wei Chang 
# document occupational segregation
# for example executives.. what is the share of white, black, hispanic
# share of men/women and their evolution over time
# using the data from here 
# https://www.bls.gov/cps/tables.htm


library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)
library(patchwork)
library(glue)
library(pals)
library(RColorBrewer)
library(ggpubr)
library(stringr)



occu_list <- c(
'Management, professional, and related occupations',
'Service occupations',
'Sales and office occupations',
'Natural resources, construction, and maintenance occupations',
'Production, transportation, and material moving occupations'
)

df <- read.csv('data_2003_to_2019.csv')

df_plotting  <- df  %>% filter(occupation %in% occu_list) %>%
  mutate(
         race = str_replace(race, 'Black or African American', "Afr. Ameri."),
         race = str_replace(race, 'Hispanic or Latino ethnicity', "Hisp. or Lat."),
         occupation = str_replace(occupation, 'Management, professional, and related occupations', "Mng, pro, related"),
         occupation = str_replace(occupation, 'Service occupations', "Service"),
         occupation = str_replace(occupation, 'Sales and office occupations', "Sales and office"),
         occupation = str_replace(occupation, 'Natural resources, construction, and maintenance occupations', "Nat., constr., maint."),
         occupation = str_replace(occupation, 'Production, transportation, and material moving occupations', "Prod., trans., mat.")
         ) %>%
  mutate(across(occupation, factor, levels=c(  "Mng, pro, related",
                                               "Service",
                                               "Sales and office",
                                               "Nat., constr., maint.",
                                               "Prod., trans., mat."
                                               )))

p1 <- ggplot(data = df_plotting, aes(year, men)) +
  geom_line(color = "steelblue", size = 0.8) +
  labs(title = "Employed men by occupation, race",
       subtitle = '1. Management, professional, and related occupations 2. Service occupations 3. Sales and office occupations\n4. Natural resources, construction, and maintenance occupations 5. Production, transportation, and material moving occupations',
       y = "Percent of total employed ", x = "") + 
  facet_grid( race~ occupation) 

ggsave(path = 'plots'  ,'occupational_segregation_by_race(men).pdf', 
       width = 12, height = 5)


p2 <- ggplot(data = df_plotting, aes(year, women)) +
  geom_line(color = "steelblue", size = 0.8) +
  labs(title = "Employed women by occupation, race",
      # subtitle = "(limited to characters with more than 100 appearances)",
       y = "Percent of total employed", x = "") + 
  facet_grid( race~ occupation) 

ggsave(path = 'plots'  ,'occupational_segregation_by_race(women).pdf', 
       width = 12, height = 5)


ggarrange(p1, p2,
          ncol = 1, nrow = 2)


ggsave(path = 'plots'  ,'occupational_segregation_by_race(full).pdf', 
       width = 12, height = 10)



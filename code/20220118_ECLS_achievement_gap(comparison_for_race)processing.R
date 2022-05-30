# Hung-Wei Chang 
# Reading in raw data 


# I downloaded the data as SPSS data (.sav), so I have to use other packages to read the data 
library(foreign)
library(tidyverse)
library(haven)
library(sjmisc)
library(dplyr)
library(ggplot2)
library(glue)

#-----------------------
# raw data (1G +) is huge so it takes some time to read
# I comment it out in case professor doesn't want to wait 
#-----------------------
# df = read_sav('28023-0001-Data.sav')
# write.csv(df, 'data_full(kindergarten_to_grade8).csv', row.names=FALSE)


#-----------------------
# select the data we need 
# select math scores, reading scores for each grade and each race 
#-----------------------

#df_math_score <- df[, c('RACE', 'C1R4MSCL', 'C2R4MSCL', 'C3R4MSCL', 'C4R4MSCL', 'C5R4MSCL', 'C6R4MSCL', 'C7R4MSCL')	]
#df_reading_score <- df[, c( 'RACE' ,'C1R4RSCL', 'C2R4RSCL', 'C3R4RSCL', 'C4R4RSCL', 'C5R4RSCL', 'C6R4RSCL', 'C7R4RSCL')	]

# write.csv(df_math_score, 'data_math_score(kindergarten_to_grade8).csv', row.names=FALSE)
# write.csv(df_reading_score, 'data_reading_score(kindergarten_to_grade8).csv', row.names=FALSE)


#-----------------------
# read the csv data I created from above 
# it can save a lot of time 
#-----------------------
df_math_score <- read.csv('data_math_score(kindergarten_to_grade8).csv')
df_reading_score_noNA <- read.csv('data_reading_score(kindergarten_to_grade8).csv')


#-----------------------
# remove any rows with NA values 
#-----------------------

df_math_score_noNA <- na.omit(df_math_score) 
df_reading_score_noNA <- na.omit(df_reading_score) 

new_colnames_year_specification <- c('race', 
                                     'kindergarten_fall',
                                     'kindergarten_spring',
                                     'grade1_fall',
                                     'grade1_spring',
                                     'grade3',
                                     'grade5',
                                     'grade8') 

colnames(df_math_score_noNA)= new_colnames_year_specification
colnames(df_reading_score_noNA)= new_colnames_year_specification

#-----------------------
# pivot longer (for different grades) to plot
#-----------------------
df_math_plotting <- df_math_score_noNA %>%
  pivot_longer(!race, names_to = "grade", values_to = "math_scores")

df_math_plotting$grade <- factor(df_math_plotting$grade, levels= new_colnames_year_specification)



df_reading_plotting <- df_reading_score_noNA %>%
  pivot_longer(!race, names_to = "grade", values_to = "reading_scores")

df_reading_plotting$grade <- factor(df_reading_plotting$grade, levels= new_colnames_year_specification)



#-------------
# Creating a race label list to plot everything using for loop 
#------------


# 1	WHITE, NON-HISPANIC
# 2	BLACK OR AFRICAN AMERICAN, NON-HISPANIC
# 3	HISPANIC, RACE SPECIFIED
# 4	HISPANIC, RACE NOT SPECIFIED
# 5	ASIAN
# 6	NATIVE HAWAIIAN, OTHER PACIFIC ISLANDER
# 7	AMERICAN INDIAN OR ALASKA NATIVE
# 8	MORE THAN ONE RACE, NON HISPANIC


race_label_list = list(
  c(1, 'WHITE, NON-HISPANIC'),
  c(2, 'BLACK OR AFRICAN AMERICAN, NON-HISPANIC'),
  c(3, 'HISPANIC, RACE SPECIFIED'),
  c(4, 'HISPANIC, RACE NOT SPECIFIED'),
  c(5, 'ASIAN'),
  c(6, 'NATIVE HAWAIIAN, OTHER PACIFIC ISLANDER'),
  c(7, 'AMERICAN INDIAN OR ALASKA NATIVE'),
  c(8, 'MORE THAN ONE RACE, NON HISPANIC')
)

#----------------
# new edition 
#----------------

# 20220118
df_math_plotting_for_grade <- df_math_plotting 
df_reading_plotting_for_grade <- df_reading_plotting %>% filter( !is.na(race),
                                                                 !is.na(reading_scores))

for (i in race_label_list){  
df_math_plotting_for_grade$race[df_math_plotting_for_grade$race == i[1]] <- i[2]
df_reading_plotting_for_grade$race[df_reading_plotting_for_grade$race == i[1]] <- i[2]
}




#-----------------------
# plotting by grade
# To compare between race 
#-----------------------

#-----------------------
# plotting (by grade) function for math score 
#----------------------- 
plot_score_distribution_math_race_comp <- function(df_plotting, cur_grade ){
  
  df_test <- df_plotting %>% 
    filter(grade == cur_grade)
  
  df_test$math_scores = as.numeric(df_test$math_scores)
  
  p <- ggplot(df_test, aes(x=race, y=math_scores, color = race)) + 
    geom_boxplot(lwd=2) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+ 
    labs(title=glue("Math Scores Comparison Between Racial Groups ({cur_grade})"),
         x ="Race", y = "Math Scores")
  
  ggsave(filename =  glue('math_scores_comparison_between_race_{cur_grade}.pdf')  , path = 'plots_compare_race/math', width = 10, height = 5.6)

  }




#-----------------------
# plotting (by grade) function for reading score 
#----------------------- 
plot_score_distribution_reading_race_comp <- function(df_plotting, cur_grade ){
  
  df_test <- df_plotting %>% 
    filter(grade == cur_grade)
  
  df_test$reading_scores = as.numeric(df_test$reading_scores)
  
  p <- ggplot(df_test, aes(x=race, y=reading_scores, color = race)) + 
    geom_boxplot(lwd=2) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+ 
    labs(title=glue("Reading Scores Comparison Between Racial Groups ({cur_grade})"),
         x ="Race", y = "Reading Scores")
  
  ggsave(filename =  glue('reading_scores_comparison_between_race_{cur_grade}.pdf')  , path = 'plots_compare_race/reading', width = 10, height = 5.6)
  
}


#-----------------------
# plot everything 
#----------------------


year_to_plot <- c( 'kindergarten_fall',
                  'kindergarten_spring',
                  'grade1_fall',
                  'grade1_spring',
                  'grade3',
                  'grade5',
                  'grade8') 


for (i in year_to_plot){
  plot_score_distribution_math_race_comp(df_math_plotting_for_grade, i)
  plot_score_distribution_reading_race_comp(df_reading_plotting_for_grade, i)
  }
















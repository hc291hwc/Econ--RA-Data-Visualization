# Hung-Wei Chang 
# Plotting educational attainment from 
# https://www.census.gov/data/tables/time-series/demo/educational-attainment/cps-historical-time-series.html
# Instructions 
    # 1- create the most comprehensive figure of education attainment in the US over the longest time period
    # 2- create the most comprehensive (detailed education levels) in the US - even if it means shorter time frames
    # 3- do both by race
    # 4-do both by gender

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)
library(patchwork)
library(glue)
library(pals)
library(RColorBrewer)

#==================
# Reading in the processed csv data 
# I adjusted the raw data so it can be read in R as a csv file 
# Raw data is saved in excel format 
#==================

df_col_attainment <- read.csv('pct_completed_4yrs_of_college.csv')
df_hs_attainment <- read.csv('pct_completed_4yrs_of_highschool.csv')


df_col_attainment  <- data.frame(lapply(df_col_attainment,as.numeric))
df_hs_attainment  <- data.frame(lapply(df_hs_attainment,as.numeric))


genders <- c('male','female')
races <- c('white', 
           'non.hispanic.white', 
           'black', 
           'asian', 
           'hispanic.any.race', 
           'white.alone.or.in.combination',
           'non.hispanic.white.alone.or.in.combination',
           'black.alone.or.in.combination',
           'asian.alone.or.in.combination') 
edu_levels <- c('College', 'High School')

#==================
# plotting functions by gender
#==================

plt_by_gender <- function(gender, df, edu = c('College', 'High School')){
  df_col_plt <- df %>%
    pivot_longer(
      cols = ends_with(glue('_{gender}')),
      names_to = "race",
      values_to = "pct",
      values_drop_na = FALSE
    )
  
  colourCount = length(unique(df_col_plt$race))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  plt_col <- df_col_plt%>% 
    ggplot(aes(year, pct, color = race)) +
    geom_line(aes(group = race), size = 0.8, alpha= 0.55) + 
    labs(title = glue('Percentage of Attaining 4 Years of {edu} or More By Gender ({gender})'), 
         x = "Time",
         y = "Percentage" )  + 
    scale_color_manual(values = getPalette(colourCount))

    
  ggsave(glue('pct_attaining_4yrs_of_{edu}_or_more({gender}).pdf'), path = 'plots_edu_attainment_by_gender', width = 10, height = 5.6)
}


#================
# plotting functions by races
#================

plt_by_race <- function(race, df, edu = c('College', 'High School')){
  df_col_plt <- df %>%
    pivot_longer(
      cols = starts_with(race),
      names_to = "race",
      values_to = "pct",
      values_drop_na = FALSE
    )
  
  colourCount = length(unique(df_col_plt$race))
  getPalette = colorRampPalette(brewer.pal(4, "Set1"))
  
  plt_col <- df_col_plt%>% 
    ggplot(aes(year, pct, color = race)) +
    geom_line(aes(group = race), size = 0.8, alpha= 0.55) + 
    labs(title = glue('Percentage of Attaining 4 Years of {edu} or More By Race'), 
         x = "Time",
         y = "Percentage"
    ) + scale_color_manual(values = getPalette(colourCount))
  ggsave(glue('pct_attaining_4yrs_of_{edu}_or_more({race}).pdf'), path = 'plots_edu_attainment_by_race', width = 10, height = 5.6)
}



#===================
# Calling the function and plot 
#===================

plt_by_gender('male', df_col_attainment, 'College')
plt_by_gender('male', df_hs_attainment, 'High School')
plt_by_gender('female', df_col_attainment, 'College')
plt_by_gender('female', df_hs_attainment, 'High School')

for (i in races){
  plt_by_race(i, df_col_attainment, 'College')
  plt_by_race(i, df_hs_attainment, 'High School')  
}



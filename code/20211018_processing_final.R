# Hung-Wei Chang 
# Get educational shares in the US for each race and gender combinations over time
# The data from data.census.gov includes "1 year" and "5 year" 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(glue)
library(stringr)

#=======================
# At first, I tried to read in data from all 2010 to 2019
   # ACSST1Y2010.S1501_data_with_overlays_2021-10-18T225734
   # ACSST1Y2011.S1501_data_with_overlays_2021-10-18T225734
   # ACSST1Y2012.S1501_data_with_overlays_2021-10-18T225734
# I left merged all data on the variable code and found something strange 
# a. the variable code changes starting from 2015 
# b. from 2010 to 2014, the data did not include the races and gender breakdown that professor want
# c. Hence, I just used the data after 2015  
# d. I commented out this chunk, becaues here is the part I found the variable code changes 
#=======================

# years <- seq(2010, 2018)
# for (year in years){
# if (year == 2010) {
#   df_start <- read.csv(glue('edu_attainment_1yr/ACSST1Y2010.S1501_data_with_overlays_2021-10-18T225734.csv'))
#   df_start <- t(df_start) 
#   df_start <- cbind(code = rownames(df_start), df_start)
#   rownames(df_start) <- 1:nrow(df_start)
#   colnames(df_start) <- c('code', 'var', glue('val_2010')) 
#   
#   df_temp <- read.csv(glue('edu_attainment_1yr/ACSST1Y{year+1}.S1501_data_with_overlays_2021-10-18T225734.csv'))
#   df_temp <- t(df_temp) 
#   df_temp <- cbind(code = rownames(df_temp), df_temp)
#   rownames(df_temp) <- 1:nrow(df_temp)
#   colnames(df_temp) <- c('code', 'var', glue('val_{year+1}'))  
#   df_merge <-  merge(x=df_start, y=df_temp_right[,c(1, 3)], by="code")  }
#   
# else {
#   df_temp <- read.csv(glue('edu_attainment_1yr/ACSST1Y{year+1}.S1501_data_with_overlays_2021-10-18T225734.csv'))
#   df_temp <- t(df_temp) 
#   df_temp <- cbind(code = rownames(df_temp), df_temp)
#   rownames(df_temp) <- 1:nrow(df_temp)
#   colnames(df_temp) <- c('code', 'var', glue('val_{year+1}'))  
#   df_merge <-  merge(x=df_merge, y=df_temp[,c(1, 3)], by="code")
#   
#   
# }  
# }


#=======================
# Start from here
# from 2015 to 2018 
#=======================

# var_ref_2015 <- read.csv("edu_attainment_1yr/ACSST1Y2015.S1501_metadata_2021-10-18T225734.csv")

years <- seq(2015, 2018)
for (year in years){
  if (year == 2015) {
    df_start <- read.csv(glue('edu_attainment_1yr/ACSST1Y{year}.S1501_data_with_overlays_2021-10-18T225734.csv'))
    df_start <- t(df_start) 
    df_start <- cbind(code = rownames(df_start), df_start)
    rownames(df_start) <- 1:nrow(df_start)
    colnames(df_start) <- c('code', 'var', glue('val_2015')) 
    
    df_temp <- read.csv(glue('edu_attainment_1yr/ACSST1Y{year+1}.S1501_data_with_overlays_2021-10-18T225734.csv'))
    df_temp <- t(df_temp) 
    df_temp <- cbind(code = rownames(df_temp), df_temp)
    rownames(df_temp) <- 1:nrow(df_temp)
    colnames(df_temp) <- c('code', 'var', glue('val_{year+1}'))  
    df_merge <-  merge(x=df_start, y=df_temp[,c(1, 3)], by="code")  }
  
  else {
    df_temp <- read.csv(glue('edu_attainment_1yr/ACSST1Y{year+1}.S1501_data_with_overlays_2021-10-18T225734.csv'))
    df_temp <- t(df_temp) 
    df_temp <- cbind(code = rownames(df_temp), df_temp)
    rownames(df_temp) <- 1:nrow(df_temp)
    colnames(df_temp) <- c('code', 'var', glue('val_{year+1}'))  
    df_merge <-  merge(x=df_merge, y=df_temp[,c(1, 3)], by="code")
    
    
  }  
}

#=======================
# All characteristics is contained in the 'id' column as a string format in the raw csv data 
# so I split this column and mutate to other columns so I can do other filtering
# eg, filter by gender, races, percentage, educational attainment 
#=======================

df_final <- df_merge[-c(1,2),] %>% dplyr::rowwise() %>% dplyr::mutate(gender = strsplit(var, split="!!")[[1]][1],
                                                                      percent = strsplit(gender, split=' ')[[1]][1],
                                                                      var_type = strsplit(var, split="!!")[[1]][2],
                                                                      var_desc = strsplit(var, split="!!")[[1]][3],
                                                                      var_edu = strsplit(var, split="!!")[[1]][4])


df_final$percent[ df_final$percent != 'Percent'] <- NA   
df_final$gender[df_final$gender == "Percent"] <- NA

df_final <-  df_final%>% 
  mutate(gender = str_replace(gender, "Percent ", ""))



#=======================
# Briefly summarize important variable by counting 
# eg, race column, educational attainment column 
#=======================
df_final %>% 
  count(var_desc)    

# 1 American Indian or Alaska Native alone                                                      36
# 2 Asian alone                                                                                 36
# 3 Black alone                                                                                 36
# 4 Hispanic or Latino Origin                                                                   36
# 6 Native Hawaiian and Other Pacific Islander alone                                            36
# 17 Some other race alone                                                                       36
# 18 Two or more races                                                                           36
# 19 White alone, not Hispanic or Latino                                                         36

df_final %>% 
  count(var_edu)   

# var_edu                                         n
# <chr>                                       <int>
# 1 9th to 12th grade, no diploma                  12
# 2 Associate's degree                             12
# 3 Bachelor's degree                              12
# 4 Bachelor's degree or higher                   168
# 5 Graduate or professional degree                12
# 6 High school graduate (includes equivalency)    36
# 7 High school graduate or higher                144
# 8 Less than 9th grade                            12
# 9 Less than high school graduate                 24
# 10 Population 25 years and over with earnings     72
# 11 Some college or associate's degree             24
# 12 Some college, no degree                        12
# 13 White alone                                    36
# 14 NA                                            192


race_category <- c('American Indian or Alaska Native alone',
                   'Asian alone',
                   'Black alone',
                   'Hispanic or Latino Origin',
                   'Native Hawaiian and Other Pacific Islander alone',
                   'Some other race alone',
                   'Two or more races',
                   'White alone, not Hispanic or Latino')

gender_list <- c('Males', 'Females')

#=======================
# create the dataframe including different genders and races 
#=======================

df_by_gender_race <- data.frame()
for (g in gender_list) {
  for (race in race_category){
  df_by_gender_race_temp <- filter(df_final, 
                       gender == g,
                       var_type == "Estimate",
                       var_desc == race )
  
  df_by_gender_race <- rbind(df_by_gender_race, df_by_gender_race_temp )}

}

df_by_gender_race <- select(df_by_gender_race, -c('var'))

df_by_gender_race <- select(df_by_gender_race, c("code" ,
                                                 "gender",
                                                 "var_desc",
                                                 "var_edu",
                                                 "var_type",
                                                 "percent",
                                                 "val_2015",
                                                 "val_2016",
                                                 "val_2017",
                                                 "val_2018",
                                                 "val_2019"))

df_by_gender_race <- df_by_gender_race %>% 
        filter(val_2015 != '(X)')

write.csv(df_by_gender_race, 'processed_csv/edu_attainment_by_race_gender.csv', row.names = FALSE)

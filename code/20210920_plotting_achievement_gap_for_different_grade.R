# Sep 20, 2021
# Hung-Wei Chang 
# plotting the standard deviation lag compared to the metropolitan Northeast average white
# plotting for different regions and different races 
library(ggplot2)
library(dplyr)

#=====================================   
# plotting the standard deviation lag compared to the metropolitan Northeast average white
#=====================================   
filename_lis  <- c('table3.121.1_stddev_below_average_white(verbal).csv',
                   'table3.121.2_stddev_below_average_white(reading).csv',
                   'table3.121.3_stddev_below_average_white(math).csv')
plotname_lis <- c('Verbal', 'Reading', 'Math')


plot_dotplot <- function (filename, plotname){
df <- read.csv(filename) 

grps <- as.factor(df$category)
my_cols <- c("#999999", "#E69F00", "#56B4E9", '#00008B', '#9932CC')


pdf(file = paste(paste('plots', plotname, sep="/"), 'score_gap.pdf', sep = '_'),   # The directory you want to save the file in
    width = 8.3, # The width of the plot in inches
    height = 11.7) # The height of the plot in inches
# save it in A4 format

dotchart(df$stddev_diff, labels = df$race_and_grade,
         groups = grps, gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.6,  pch = 19, xlab = paste(paste("Avg Score Difference (standard deviation) in", plotname, sep = ' '), 'Compared to Northeastern Metropolitan White Avg', sep = ' ')) 

dev.off()
}


for (i in seq(1,3)) {
plot_dotplot(filename_lis[i], plotname_lis[i])
}



#=====================================   
# Another plot
# total standard deviation (variation in the raw data) between schools
#=====================================   
df_school_var <- read.csv('table3.22.1_percent_of_total_variance_in_individual_verbal_achievement_scores_that_lies_between_schools.csv')

df_school_var <- df_school_var  %>% mutate(
  score_stddev_between_schools = score_variance_between_schools ^ 0.5
)

df_school_var$grade <- factor(df_school_var$grade, levels = c("grade1", "grade3", "grade6", "grade9", "grade12"))

ggplot(data = df_school_var, aes(x = race, y = score_stddev_between_schools, fill = grade)) + 
  geom_bar(stat = "identity", position = "dodge2", alpha = 0.75)   + coord_flip() +scale_fill_grey() + ylab('standard deviation of scores between schools (percentage)')

ggsave(filename =  'verbal_score_stddev_between_schools(barplot).pdf', path = 'plots', width = 10, height = 5.6)

  

#=====================================   
# Another type of graph (dotplot)
# I thought dotplot would be a better representation since the data only has one dimension
#=====================================   
grps <- as.factor(df_school_var$race)
my_cols <- c("#999999", "#E69F00", "#56B4E9", '#00008B', '#9932CC', '#8B4513', '#BA55D3', '#483D8B')

pdf(file = 'plots/verbal_score_stddev_between_schools(dotplot).pdf',   # The directory you want to save the file in
   width = 10, # The width of the plot in inches
   height = 5.6) # The height of the plot in inches
# save it as a normal slide format

dotchart(df_school_var$score_stddev_between_schools, labels = df_school_var$grade,
        groups = grps, gcolor = my_cols,
        color = my_cols[grps],
        cex = 0.6,  pch = 19, xlab = 'Verbal Score Std Dev Between Schools by Races') 

dev.off()


# Another graph 
# the variance of parents' desires for childs' education 

df_expectation_var <- read.csv('table3.221.4_variance_of_parents_desires_for_childrens_education.csv')

df_expectation_var <- df_expectation_var  %>% mutate(
  exp_stddev_between_race = exp_var ^ 0.5
)


grps <- as.factor(df_expectation_var$race)
my_cols <- c("#999999", "#E69F00", "#56B4E9", '#00008B', '#9932CC', '#8B4513', '#BA55D3', '#483D8B', '#A9A9A9', '#B0C4DE')

pdf(file = 'plots/expectation_stddev_between_races(dotplot).pdf',   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 5.6) # The height of the plot in inches
# save it as a normal slide format


dotchart(df_expectation_var$exp_stddev_between_race, labels = df_expectation_var$grade,
         groups = grps, gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.6,  pch = 19, xlab = 'Standard deviation of parents expectation by races (percentage)')

dev.off()




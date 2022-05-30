# Hung-Wei Chang
# Sep 10, 2021 Note
# Plotting distribution of test scores among racial groups
# Updating the csv data 
# Updating the code to organize plotting with functions 

library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)
library(patchwork)



plot_wage_distribution <- function(data, test_type){
    # ethnicity <- c('White', "Black", "Hispanic", 'Asian')
    ethnicity <- unique(data$ethnicity)
    
    len = 1000
    m <- matrix(0, ncol = length(ethnicity), nrow = len)
    x <- seq(-5, 5, length=len)
    
    current_data <- filter(data, variable == test_type)
    
    for (i in seq(1: length(ethnicity))){
    y <- dnorm(x, mean= data$test_score_mean[i], sd= data$test_score_var[i])
    m[1:len, i] = y 
    }
    df_plotting <- data.frame(m)
    colnames(df_plotting) <- ethnicity
    
    df_plotting$index = x 
    
    
    df_plotting <- df_plotting %>%
      pivot_longer(
        cols = 1 : ( length(ethnicity) ),
        names_to = "Ethnicity",
        names_prefix = "exp",
        values_to = "density",
        values_drop_na = TRUE
      )
    
    
    graph <- df_plotting%>% 
      ggplot(aes(index, density, color = Ethnicity)) +
      geom_line(aes(), size = 1.2) + 
      labs(title = paste('Raw test scores among different ethnicities for', test_type , sep =" "), 
           x = "test score (normalized)",
           y = "density") +
      theme(axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.title = element_text(size = 25),
            plot.title = element_text(size = 25)) +
      theme_classic()
    
    ggsave(filename = paste(test_type, 'pdf', sep="."), path = 'plots', width = 12, height = 4)

}


# Loading in data
# The csv data has to be organized in this particular way so that the plotting function can work
data <- read.csv("data.csv")
test_type_list <- unique(data$variable)

# Plotting the racial groups among different test at different age 
for (i in test_type_list){
plot_wage_distribution(data, i)
}





final_plot <- (graph / graph_2) 

final_plot

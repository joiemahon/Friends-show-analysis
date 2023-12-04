## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
mean(data$gender_of_main_character)
sd(data$gender_of_main_character)
table(data$gender_of_main_character)
describe(data$gender_of_main_character)
summary(data$gender_of_main_character)

mean(data$rating)
sd(data$rating)
table(data$rating)
describe(data$rating)
summary(data$rating)

mean(data$season)
sd(data$season)
table(data$season)
describe(data$season)
summary(data$season)

mean(data$topic_of_convo)
sd(data$topic_of_convo)
table(data$topic_of_convo)
describe(data$topic_of_convo)
summary(data$topic_of_convo)

mean(data$main_character_appearances)
sd(data$main_character_appearances)
table(data$main_character_appearances)
describe(data$main_character_appearances)
summary(data$main_character_appearances)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$gender_of_main_character,data$season)
table(data$gender_of_main_character,data$topic_of_convo)
table(data$season,data$topic_of_convo)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(data$gender_of_main_character,data$season)
chisq.test(data$gender_of_main_character,data$topic_of_convo)
chisq.test(data$season,data$topic_of_convo)

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(gender_of_main_character ~ rating, data = data)
summary(anova_adapted)
anova_adapted <- aov(gender_of_main_character ~ main_character_appearances, data = data)
summary(anova_adapted)
anova_adapted <- aov(season ~ rating, data = data)
summary(anova_adapted)
anova_adapted <- aov(season ~ main_character_appearances, data = data)
summary(anova_adapted)
anova_adapted <- aov(topic_of_convo ~ rating, data = data)
summary(anova_adapted)
anova_adapted <- aov(topic_of_convo ~ main_character_appearances, data = data)
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$rating,data$main_character_appearances)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(rating ~ main_character_appearances, data = data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
linear_plot <- plot(data$gender_of_main_character, data$rating)
print(linear_plot)
linear_relationship <- lm(gender_of_main_character ~ rating, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")
abline(h=mean(data$main_character_appearances))
abline(h=mean(data$rating))

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$main_character_appearances, residuals(linear_relationship))
abline(v = 8.2,col = "green")
mean(residuals(linear_relationship))
abline(h = 1.5,col = "green")
abline(linear_relationship, col = "red")

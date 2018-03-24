
library(dplyr)
setwd("D:/ujjwal/Tutorial/DataScience/R-practice/Wages Problem")

###
# Get the data in to R
###
dataset = read.csv('data/Wage_1', stringsAsFactors = FALSE)

colnames(dataset)
summary(dataset)
str(dataset)


dataset <- data.frame(lapply(dataset, trimws), stringsAsFactors = TRUE)


dataset <- dataset %>%
  mutate(jobclass = ifelse(jobclass == "Industrial", 1, 2),
         race = ifelse(race == 'Asian', 1, ifelse(race == 'Black',2,3)),
         sex = ifelse(sex == 'Male', 1, 2))

head(dataset)

dataset$sex <- trimws(dataset$sex)


1:length(levels(dataset$maritl))
as.numeric(dataset$maritl)


dataset$maritl <- trimws(dataset$maritl)
dataset$race <- trimws(dataset$race) 
dataset$education <- trimws(dataset$education)
dataset$region <- trimws(dataset$region)
dataset$health <- trimws(dataset$health)
dataset$health_ins <- trimws(dataset$health_ins)
dataset$jobclass <- trimws(dataset$jobclass) 


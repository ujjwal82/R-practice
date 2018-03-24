
library(dplyr)
setwd("D:/ujjwal/Tutorial/DataScience/R-practice/Wages Problem")

###
# Get the data in to R
###
dataset_raw = read.csv('data/Wage_1', stringsAsFactors = FALSE)

## Take a copy of the original data
dataset <- dataset_raw

## Remove any leadign spaces from all the columns (apply only to character values columns)
dataset[sapply(dataset, is.character)] <- data.frame(lapply(dataset[sapply(dataset, is.character)], trimws), stringsAsFactors = TRUE)

## Now convert all the factors (converted in factors in previous step) into numeric
dataset[sapply(dataset, is.factor)] <- data.frame(lapply(dataset[sapply(dataset, is.factor)], as.numeric))

## Now convert all columns to factor, except last two columns (lowwages and wages, which will be our dependent variables)
dataset[1:(ncol(dataset)-2)] <- lapply(dataset[1:(ncol(dataset)-2)], as.factor)

###
# Step #1 to 4 can be applied to any dataset, once we have removed all missing values
# Step #5 is specific to problem, need to apply with index of dependent variable.
###
summary(dataset)

plot(dataset$wage)
hist(x = dataset$wage)
library(plotly)


###
# Visualizing data
###


df <- dataset %>%
  filter(year == 2004, race == 1) %>%
  group_by(age, maritl) %>%
  select(age, maritl, wage)

df %>%
  plot_ly()%>%
  add_trace(x = ~age, y = ~wage, type = 'bar',
            text = df$wage, textposition = 'auto') %>%
  # add_trace(x = ~maritl, y = ~wage, type = 'bar',
  #           text = df$wage, textposition = 'auto') %>%
  layout(title = "Wages grouped by Age and Marital Status(Year 2004)",
         barmode = 'group',
         xaxis = list(title = "Month"),
         yaxis = list(title = "Wages"))


###
# Split data
###
set.seed(54321)
indexes <- createDataPartition( dataset$wage,
                                times = 1,
                                p = 0.7,
                                list = FALSE)
dataset.train <- dataset[indexes,]
dataset.test <-  dataset[-indexes,]








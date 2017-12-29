# Predict whether the flight will be deloyed or not.


# Download the flights data:
#     https://drive.google.com/drive/folders/1um8ovwEa7_lwKdLKPXBcG13ZwdJ5SigW

# install.packages("hflights")
# install.packages("dplyr")
# install.packages("rpart")
library(dplyr)

#source('RFunctions.R')

# Importing the dataset and select useful columns only

###
# Format the flight date, set new column Deloayed 1 
# if DepDelayed more than 15 0 otherwise 
###
dataset_raw <- read.csv('hflights/data/Flight1987.csv')

dataset <- dataset_raw %>%
  filter(is.na(DayOfWeek) != TRUE) %>%
  mutate(Date = paste(Year, Month, DayofMonth, sep = "-"),
         ActualElapsedTime= ifelse(is.na(ActualElapsedTime), 0, ActualElapsedTime),
         ArrDelay = ifelse(is.na(ArrDelay), 0, ArrDelay),
         DepDelay = ifelse(is.na(DepDelay), 0, DepDelay),
         Delayed = ifelse(is.na(DepDelay), 0, ifelse(dataset$DepDelay > 15, 1, 0))) %>%
  select(Date, DayOfWeek, DepTime, ArrTime, 
         ActualElapsedTime, ArrDelay, DepDelay, 
         Origin, Dest, Distance, Delayed)

head(dataset)

dependent_var <- 'Delayed'

###
# Load the airport list
###
raw <- read.delim('https://raw.githubusercontent.com/opentraveldata/opentraveldata/master/data/IATA/archives/iata_airport_list_20171208.csv', 
                  sep = '^')

airport_ds <- select(raw, city_code, city_name, country_code, tz_code, por_code, por_name)
head(airport_ds)

##
# replce origin and destination city names
##
dataset$Origin <- airport_ds[match(dataset$Origin, airport_ds$por_code),'city_name']
dataset$Dest <- airport_ds[match(dataset$Dest, airport_ds$por_code),'city_name']

dataset$Date <- factor(dataset$Date)
dataset$Delayed <- factor(dataset$Delayed)
dataset$DayOfWeek <- factor(dataset$DayOfWeek)

dataset <- dataset %>%
  filter(is.na(DepTime) == FALSE &
           is.na(ArrTime) == FALSE)
#write.csv(dataset, file = "hflights/data/dataset.csv")
summary(dataset)

###
# Summarized delayed flight data flying from different origins and destination
###

dataset %>%
  group_by(Origin) %>%
  summarise(avg_delay = round(x = mean(DepDelay), digits = 0),
            min_deplay = min(DepDelay),
            max_delay = max(DepDelay),
            total = n())

dataset %>%
  group_by(Dest) %>%
  summarise(avg_delay = round(x = mean(DepDelay), digits = 0),
            min_deplay = min(DepDelay),
            max_delay = max(DepDelay),
            total = n())

###
# Summarized delayed flight data flying on different weekdays/weekends
###
dataset %>%
  group_by(DayOfWeek) %>%
  summarise(avg_delay = round(x = mean(DepDelay), digits = 0),
            min_deplay = min(DepDelay),
            max_delay = max(DepDelay),
            total = n())



###
# Summarized delayed flight data flying from Atlanta
###
dataset %>%
  filter(Origin == 'Atlanta' & Delayed == 1) %>%
  group_by(Origin, Dest) %>%
  summarise(avg_delay = round(x = mean(DepDelay), digits = 0),
            min_deplay = min(DepDelay),
            max_delay = max(DepDelay),
            total = n())


##------- Predictions  ---------##

###
# Split the dataset into training and test data
###
set.seed(123)
flt_sample <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.95, 0.05))
training_set <- dataset[flt_sample == 1, ]
test_set <- dataset[flt_sample == 2, ]


###
# Let's create a dataframe to compare the predicted values
# (from different classification models) with actuals
###
Comp_pred <- data.frame('predicted' = test_set[dependent_var])

###
# Create the formula, we will be using it in all of the classification model
###
lm_formula <- as.formula(paste('DepDelay', ' ~ DayOfWeek', sep = ''))


###--------------------###
# Linear regression      #
###--------------------###
# Fitting Linear Regression model on training set
classifier <- lm(formula = lm_formula, data = training_set)
summary(classifier)

###
# Predict the results using test data
###
y_pred <- predict(classifier, test_set)
Comp_pred$L_R <- ifelse(y_pred > 0.5, 1, 0)


###--------------------###
# Support Vector Machine #
###--------------------###
# Fitting SVM model on training set
# library(e1071)
# classifier <- svm(formula = lm_formula, data = training_set)
# 
# # Predicting the test set results
# y_pred <- predict(classifier, newdata = test_set)
# Comp_pred$SVM <- ifelse(y_pred > 0.5, 1, 0)

###--------------------###
# Decision Tree          #
###--------------------###
# Fitting Decision Tree model on training set
library(party)
classifier <- ctree(formula = lm_formula, data = training_set, controls = ctree_control(mincriterion = 0.9, minsplit = 1000))
# plot(classifier)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$DecisionTree <- ifelse(y_pred > 0.5, 1, 0)

###--------------------###
# Random Forest          #
###--------------------###
# Fitting Random FOrest model on training set
library(randomForest)
classifier <- randomForest(formula = lm_formula, data = training_set)
# plot(classifier)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$RandomForest <- ifelse(y_pred > 0.5, 1, 0)

###---------------------###
# K-Nearest Neighbor(KNN) #
###---------------------###
# remove the dependent variable
training_set1 <- training_set[, -22]
test_set1 <- test_set[, -22]

ctg_train_lbl <- training_set[, 22]

library(class)
y_pred <- knn(train = training_set1,  test = test_set1, cl = ctg_train_lbl, k = 1000)
Comp_pred$KNN <- ifelse(y_pred > 0.5, 1, 0)

# Name the rows correctly (as squence of numbers)
rownames(Comp_pred) <- c(1: length(y_pred)) 

# R does not have a standard in-built function to calculate mode. So we 
# create a user function to calculate mode of a data set in R. This 
# function takes the vector as input and gives the mode value as output.

# Create a function to calculate and return the mode of values
getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

###
# Finally we want to take the vote of outcomes coming from all these 
# classsification techniques and declare the our verdict.
#
# Note: we don't want to consider the actual results, mode should 
# be applied only on caluculated/predicted results.
###
Comp_pred$Verdict <- apply(Comp_pred[-1], 1, getMode)
glimpse(Comp_pred)


###
# Confussion matrixes for different calssifications
###
cm.L_R <- table(Comp_pred$L_R,test_set$Delayed)
cm.DecisionTree <- table(Comp_pred$DecisionTree, test_set$Delayed)
cm.KNN <- table(Comp_pred$KNN,test_set$Delayed)
cm.RandomForest <- table(Comp_pred$RandomForest,test_set$Delayed)
cm.Verdict <- table(Comp_pred$Verdict,test_set$Delayed)


###
# Calculate accuracy
###
acc.L_R<- sum(diag(cm.L_R))/sum(cm.L_R)*100
acc.DecisionTree <- sum(diag(cm.DecisionTree))/sum(cm.DecisionTree)*100
acc.KNN<- sum(diag(cm.KNN))/sum(cm.KNN)*100
acc.RandomForest<- sum(diag(cm.RandomForest))/sum(cm.RandomForest)*100
acc.Verdict<- sum(diag(cm.Verdict))/sum(cm.Verdict)*100

print(paste('Logistic Regressor : ', acc.L_R))
print(paste('Decision Tree : ', acc.DecisionTree))
print(paste('Random Forest : ', acc.RandomForest))
print(paste('KNN : ', acc.KNN))
print(paste('Final verdict : ', acc.Verdict))


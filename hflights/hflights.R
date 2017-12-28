# Predict whether the flight will be deloyed or not.


# install.packages("hflights")
# install.packages("dplyr")
# install.packages('party')
# install.packages('randomForest')
library(dplyr)
library(hflights)

data("hflights")

# Importing the dataset
dataset <- as.data.frame(hflights) 
dependent_var <- 'Delayed'

###
# Add new column which will indicate that the flight was deloyed (Y/N)
# we determine this by DepDelay being greater that 15
###

# Find all flights which delayed by more than 15 and mark them as DELAYED 
dataset$Delayed <- ifelse(is.na(dataset$DepDelay), 0, ifelse(dataset$DepDelay > 15, 1, 0))
#head(dataset)

###
# Handle the NA values, replace then to 0 where ever needed
###
# dataset <- mutate(dataset, replace = TRUE,  ActualElapsedTime= ifelse(is.na(ActualElapsedTime), 0, ActualElapsedTime),
#                   AirTime = ifelse(is.na(AirTime), 0, AirTime),
#                   ArrDelay = ifelse(is.na(ArrDelay), 0, ArrDelay),
#                   DepDelay = ifelse(is.na(DepDelay), 0, DepDelay)
#                   )


###
# Rmove all the cancelled flights, if any
###
dataset<- dataset %>%
  filter(CancellationCode == '')
head(dataset)
nrow(dataset)

###
# Split the dataset into training and test data
###
set.seed(123)
flt_sample <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.9, 0.1))
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
lm_formula <- as.formula(paste('Delayed', ' ~ ActualElapsedTime + ArrDelay + ArrTime + 
                   AirTime + ArrDelay + Distance + TaxiIn', sep = ''))


###--------------------###
# Logistic regression    #
###--------------------###
# Fitting Logistic Regression model on training set
classifier <- glm(formula = lm_formula, family = binomial,data = training_set)
summary(classifier)

###
# Predict the results using test data
###
y_pred <- predict(classifier, test_set, type = "response")
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


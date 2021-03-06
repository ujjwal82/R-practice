# Generate and compare multiple models on same dataset
dataFilePath <- 'Classification Techniques\\CTG.csv'
dependent_var <- 'NSP'
split_size <- 0.9


## Below package used for SVM
#install.packages('e1071')

## Below packages used for decision tree
#install.packages('rpart')
#install.packages('party')

## Below packages used for Random Forest
# install.packages('randomForest')

# Importing dataset
dataset <- read.csv(dataFilePath)

summary(dataset)

# Encoding the target feature as factor
dataset[dependent_var] <- factor(dataset[,dependent_var])

# Splitting the dataset into training and test dataset
set.seed(123)
ctg_sample <- sample(2, nrow(dataset), replace = TRUE, prob = c(split_size, 1 - split_size))

training_set <- dataset[ctg_sample == 1, ]
test_set <- dataset[ctg_sample == 2, ]


###
# Lets create a dataframe to compare the predicted values
# (from different classification models) with actuals
###
Comp_pred <- data.frame('predicted' = test_set[dependent_var])

###
# Create the formula, we will be using it in all of the classification model
###
lm_formula <- as.formula(paste(dependent_var, ' ~ .', sep = ''))

###--------------------###
# Support Vector Machine #
###--------------------###
# Fitting SVM model on training set
library(e1071)
classifier <- svm(formula = lm_formula, data = training_set)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$SVM <- y_pred

###--------------------###
# Naive Bayes            #
###--------------------###
# Fitting Naive Bayes model on training set
classifier <- naiveBayes(formula = lm_formula, data = training_set)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$NaiveBayes <- y_pred

###--------------------###
# Decision Tree          #
###--------------------###
# Fitting Decision Tree model on training set
library(party)
classifier <- ctree(formula = lm_formula, data = training_set, controls = ctree_control(mincriterion = 0.9, minsplit = 458))
# classifier <- ctree(formula = NSP ~ LB + FM + AC, data = training_set)
# plot(classifier)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$DecisionTree <- y_pred

###--------------------###
# Random Forest          #
###--------------------###
# Fitting Random FOrest model on training set
library(randomForest)
classifier <- randomForest(formula = lm_formula, data = training_set)
# plot(classifier)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$RandomForest <- y_pred

###---------------------###
# K-Nearest Neighbor(KNN) #
###---------------------###
# remove the dependent variable
training_set1 <- training_set[, -4]
test_set1 <- test_set[, -4]

ctg_train_lbl <- training_set[, 4]

library(class)
y_pred <- knn(train = training_set1,  test = test_set1, cl = ctg_train_lbl, k = 10)
Comp_pred$KNN <- y_pred

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


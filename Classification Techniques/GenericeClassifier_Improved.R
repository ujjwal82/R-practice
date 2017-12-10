# Generate and compare multiple models on same dataset

## Below packages used for decision tree
#install.packages('rpart')
#install.packages('party')

## Below packages used for Random Forest
# install.packages('randomForest')

# Importing dataset
dataset <- read.csv("..\\Day7\\CTG.csv")

summary(dataset)

# Encoding the target feature as factor
dataset$NSP <- factor(dataset$NSP)

# Splitting the dataset into training and test dataset
set.seed(123)
ctg_sample <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.9, 0.1))

training_set <- dataset[ctg_sample == 1, ]
test_set <- dataset[ctg_sample == 2, ]


###
# Lets create a dataframe to compare the predicted values
# (from different classification models) with actuals
###
Comp_pred <- data.frame('predicted' = test_set[4])

###
# Support Vector Machine
###
# Fitting SVM model on training set
library(e1071)
classifier <- svm(NSP ~ LB + FM + AC, data = training_set)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$svm <- y_pred

# Fitting Naive Bayes model on training set
classifier <- naiveBayes(NSP ~ LB + FM + AC, data = training_set)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$nb <- y_pred

###
# Decision Tree
###
# Fitting Decision Tree model on training set
library(party)
classifier <- ctree(formula = NSP ~ LB + FM + AC, data = training_set, controls = ctree_control(mincriterion = 0.9, minsplit = 458))
# classifier <- ctree(formula = NSP ~ LB + FM + AC, data = training_set)
# plot(classifier)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$dt <- y_pred

###
# Random Forest
###
# Fitting Random FOrest model on training set
library(randomForest)
classifier <- randomForest(formula = NSP ~ LB + FM + AC, data = training_set)
# plot(classifier)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$rf <- y_pred

###
# K-Nearest Neighbor(KNN)
###
# remove the dependent variable
training_set1 <- training_set[, -4]
test_set1 <- test_set[, -4]

ctg_train_lbl <- training_set[, 4]

library(class)
y_pred <- knn(train = training_set1,  test = test_set1, cl = ctg_train_lbl, k = 10)
Comp_pred$knn <- y_pred
rownames(Comp_pred) <- c(1: length(y_pred)) 

apply(Comp_pred, 1, mode)


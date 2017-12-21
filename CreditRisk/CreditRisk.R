
# Logistic Regression

library(dplyr)
source('CreditRisk\\RFunctions.R')

# Importing the dataset
dataset <- read.csv("CreditRisk\\CreditRisk.csv")
dependent_var <- 'Loan_Status'

dataset <- dataset[-1]

###
# Remove the spaces if any in the column names
###
colnames(dataset) <- gsub(" ", "", colnames(dataset), fixed = TRUE)


# Encoding the target feature as factor
#dataset$Loan_Status = factor(dataset$Loan_Status, levels = c(0, 1))

###
# use mutate to add additional column to translate categorical data
###
dataset <- mutate(dataset, Gender = ifelse(is.na(Gender), 1, ifelse(Gender == 'Male', 1, 0)),
                  Married = ifelse(Married == 'Yes', 1,0),
                  Dependents = ifelse(is.na(Dependents), 0, Dependents),
                  Education = ifelse(Education == 'Graduate', 1, 0),
                  Self_Employed = ifelse(Self_Employed == 'No', 0, 1),
                  Property_Area = ifelse(Property_Area == 'Urban', 2, ifelse(Property_Area == 'Semiurban', 1, 0)),
                  LoanAmount = ifelse(is.na(LoanAmount), 0, LoanAmount),
                  Loan_Amount_Term = ifelse(is.na(Loan_Amount_Term), 1, Loan_Amount_Term),
                  Credit_History = ifelse(is.na(Credit_History), 0, Credit_History),
                  Loan_Status = ifelse(Loan_Status == 'Y',1,0))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Loan_Status, SplitRatio = 0.9)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

###
# Lets create a dataframe to compare the predicted values
# (from different classification models) with actuals
###
Comp_pred <- data.frame('predicted' = test_set[dependent_var])

###--------------------###
# Logistic regression    #
###--------------------###
# Fitting Logistic Regression model on training set
func_ret <- getBestSuitableRegressor(dataset = training_set, dependent_var = dependent_var)
classifier <- func_ret$classifier

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$L_R <- ifelse(y_pred > 0.5, 1, 0)

###
# We have the fomula for best suitable Logistic regression modle.
# we will use the same formula for other classification models
###
lm_formula <- func_ret$formula

###--------------------###
# Support Vector Machine #
###--------------------###
# Fitting SVM model on training set
library(e1071)
classifier <- svm(formula = lm_formula, data = training_set)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$SVM <- ifelse(y_pred > 0.5, 1, 0)

###--------------------###
# Decision Tree          #
###--------------------###
# Fitting Decision Tree model on training set
library(party)
classifier <- ctree(formula = lm_formula, data = training_set, controls = ctree_control(mincriterion = 0.9, minsplit = 458))
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
training_set1 <- training_set[, -4]
test_set1 <- test_set[, -4]

ctg_train_lbl <- training_set[, 4]

library(class)
y_pred <- knn(train = training_set1,  test = test_set1, cl = ctg_train_lbl, k = 10)
Comp_pred$KNN <- y_pred

# Name the rows correctly (as squence of numbers)
rownames(Comp_pred) <- c(1: length(y_pred)) 

###
# Finally we want to take the vote of outcomes coming from all these 
# classsification techniques and declare the our verdict.
#
# Note: we don't want to consider the actual results, mode should 
# be applied only on caluculated/predicted results.
###
Comp_pred$Verdict <- apply(Comp_pred[-1], 1, getMode)


###
# Calculate accuracy
###
acc.L_R <- calcAccuracy(Comp_pred$L_R,test_set$Loan_Status)
acc.DecisionTree <- calcAccuracy(Comp_pred$DecisionTree, test_set$Loan_Status)
acc.KNN <- calcAccuracy(Comp_pred$KNN,test_set$Loan_Status)
acc.RandomForest <- calcAccuracy(Comp_pred$RandomForest,test_set$Loan_Status)
acc.Verdict <- calcAccuracy(Comp_pred$Verdict,test_set$Loan_Status)

print(paste('Logistic Regressor : ', acc.L_R))
print(paste('Decision Tree : ', acc.DecisionTree))
print(paste('Random Forest : ', acc.RandomForest))
print(paste('KNN : ', acc.KNN))
print(paste('Final verdict : ', acc.Verdict))



library(dplyr)

# Importing the dataset
data_raw <- read.csv("HR_data.csv", stringsAsFactors = TRUE)

# Take a look at raw data
summary(data_raw)
colnames(data_raw)

# Encoding the target feature as factor
dataset <- data_raw

shouldBeCategorical <- c('number_project', 'time_spend_company', 
                         'promotion_last_5years', 'Work_accident',
                         'left','department', 'salary')
for(v in shouldBeCategorical) {
  dataset[[v]] <- as.factor(dataset[[v]])
}

# Have a summarized look for fully encoded dataaset.
summary(dataset)

# plot(dataset)
dataset <- dataset %>%
  select(left, everything())

head(reorder_df)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$left, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Applying PCA
# install.packages('caret')
library(caret)
library(e1071)

pca = preProcess(training_set[-1], method = 'pca', pcaComp = 2)
training_set = predict(pca, training_set)
training_set = select(training_set, PC1, PC2, left)
test_set = predict(pca, test_set)
test_set = select(test_set, PC1, PC2, left)


# Fitting kernel SVM to the Training set
library(e1071)
classifier <- svm( formula = left ~ .,
                   data = training_set,
                   kernel = 'radial',
                   type = 'C-classification')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
accuracy = (sum(diag(cm))/sum(cm)) *100


# Applying k-fold Cross validation
library(caret)
folds = createFolds(training_set$left, k = 10)
str(folds)


cv = lapply(X = folds, FUN = function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x,]
  classifier <- svm( formula = left ~ .,
                     data = test_fold,
                     kernel = 'radial',
                     type = 'C-classification')
  
  # Predicting the Test set results
  y_pred = predict(classifier, newdata = test_fold[-3])
  
  # Making the Confusion Matrix
  cm = table(test_fold[, 3], y_pred)
  accuracy = (sum(diag(cm))/sum(cm)) *100
  return(accuracy)
})

accuracy = mean(as.numeric(cv))
accuracy

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
# library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
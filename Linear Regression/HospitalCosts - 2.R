# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Linear Regression\\HospitalCosts.csv')

summary(dataset)

dataset$FEMALE <- as.factor(dataset$FEMALE)
dataset$RACE <- as.factor(dataset$RACE)
dataset$LOS <- as.factor(dataset$LOS)
dataset$APRDRG <- as.factor(dataset$APRDRG)
table(dataset$APRDRG)


## ------------------------------------------

dataset_640 <- subset(x = dataset, APRDRG == 640)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# install.packages('caret')
library(caret)
set.seed(54321)
Indexes <- createDataPartition(dataset_640$TOTCHG,
                               times = 1,
                               p = 0.7,
                               list = FALSE)


train_640 <- dataset_640[Indexes, ]
test_640 <- dataset_640[-Indexes, ]

prop.table(table(dataset_640$FEMALE) )
prop.table(table(train_640$FEMALE) )
prop.table(table(test_640$FEMALE) )

###
# Set up caret to perform 10-forld cross validation repeated 3 timed 
# and to use a grid search for optima model hyperparamer.
###
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")

###
# Leverage the grid search of hyperparameters for xgboost.
###
tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50, 75, 100),
                         max_depth = 6:8,
                         min_child_weight = c(1.0, 2.25, 2.5),
                         colsample_bytes = c(0.3, 0.4, 0.5),
                         gamma = 0,
                         subsample = 1)



cl <- makeCluster(3, type = "SOCK")

# register cluster so that caret will know to train in parallel.
registerDoSNOW(cl)


# Train the xgboost model using 10-fold CV repeated 3 times
# and a hyperparameter frid search to train the optimal model.
caret.cv.XGB <- train( TOTCHG ~ .,
                       data = train_640,
                       method = "xgbTree",
                       tunGrid = tune.grid,
                       trControl = train.control)

caret.cv.LM <- train( TOTCHG ~ .,
                       data = train_640,
                       method = "lm",
                       tunGrid = tune.grid,
                       trControl = train.control)
stopCluster(cl)

# Examine caret's processing result
caret.cv.XGB

# Make predictions on the test set using a xgboost model
# trained on 625 rows of the training set using the found 
# optimal hyperparameter values.
preds <- predict(caret.cv.XGB, test_640)


# user caret's confusionmatrix() to estimate the 
# effectiveness of this model on unseen, new data.
confusionMatrix(as.integer(preds), test_640[, 5])
RMSE(pred = as.integer(preds), obs = test_640[, 5], na.rm = TRUE)



library(caTools)
set.seed(123)
split = sample.split(train_640$TOTCHG, SplitRatio = 0.8)
training_set = subset(aa, split == TRUE)
test_set = subset(aa, split == FALSE)

model_640 <- lm(formula = TOTCHG ~ AGE + FEMALE + LOS + RACE, data = training_set)
summary(model_640)

model_640 <- lm(formula = TOTCHG ~ AGE + FEMALE + LOS, data = training_set)
summary(model_640)
y_pred <- predict(model_640, test_set)

## ---------------------------

aa <- subset(x = dataset, APRDRG == 754)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(aa$TOTCHG, SplitRatio = 0.8)
training_set = subset(aa, split == TRUE)
test_set = subset(aa, split == FALSE)

model_754 <- lm(formula = TOTCHG ~ AGE + FEMALE + LOS + RACE, data = training_set)
summary(model_754)

model_754 <- lm(formula = TOTCHG ~ AGE + LOS + RACE, data = training_set)
summary(model_754)
y_pred <- predict(model_754, test_set)

## ---------------------------




df <- as.data.frame(c(y_pred, test_set[5]))
####
# Do a side by side comparision of actual and predited data
###
df <- data.frame(y_pred,  test_set$TOTCHG)

colnames(df) <- c('Predited', 'Actual')

##
unique( dataset$APRDRG)
table( dataset$APRDRG)

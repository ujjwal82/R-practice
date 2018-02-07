

#install.packages(c("caret", "doSNOW", "ipred", "xgboost", "mice"))

library(dplyr)
library(doSNOW)
library(caret)
library(mice)

train <- read.csv("train.csv")


table(train$Embarked)

train$Embarked[train$Embarked == ""] <- "S"

train$FamilySize = 1 + train$SibSp + train$Parch
train$MissingAge <- ifelse( is.na(train$Age), "Y", "N")


# Set up factors
train$Survived <- as.factor(train$Survived)
train$Pclass<- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$MissingAge <- as.factor(train$MissingAge)

# Subset data to features we wish to keep/use.
train <- train %>%
  select(Survived, Pclass, Sex, Age, MissingAge, 
         SibSp, Parch, Fare, Embarked, FamilySize)

str(train)

# Impute the missing Ages using caret package.

dummy.vars <- dummyVars(~., data = train[, -1])
train.dummy <- predict(dummy.vars, train[, -1])

# Now impute
pre.process <- preProcess(train.dummy, method = "bagImpute")
imputed.vars <- predict(pre.process, train.dummy)
View(imputed.vars)

train$Age <- imputed.vars[,6]
glimpse(train)


###
# Split data
###
set.seed(54321)
indexes <- createDataPartition( train$Survived,
                                times = 1,
                                p = 0.7,
                                list = FALSE)

titanic.train <- train[indexes,]
titanic.test <-  train[-indexes,]


# Examin the proportions of the survived class lable across
# the dataset
prop.table(table(train$Survived))
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))

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
View(tune.grid)


cl <- makeCluster(10, type = "SOCK")

# register cluster so that caret will know to train in parallel.
registerDoSNOW(cl)


# Train the xgboost model using 10-fold CV repeated 3 times
# and a hyperparameter frid search to train the optimal model.
caret.cv <- train( Survived ~ .,
                   data = titanic.train,
                   method = "xgbtree",
                   tunGrid = tune.grid,
                   trControl = train.control)
stopCluster(cl)




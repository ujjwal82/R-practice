# pkgs <- c("e1071", "caret", "doSNOW", "ipred", "xgboost", "mice")
# install.packages(pkgs)

library(caret)
library(doSNOW)

library(dplyr)
library(mice)

###
# Load data
###

train <- read.csv('train.csv')
View(train)

###
# Handle the missing values.
###
summary(train)

# Find what all feature values are missing
md.pattern(x = train)

# Replace missing Embarked
table(train$Embarked)
train$Embarked[train$Embarked == ""] <- "S"

# Add a feature for tracking missing age.
train$Age[is.na(train$Age)]
train$MissingAge <- ifelse(is.na(train$Age), "Y", "N")

# Add a feature for family size
train$FamilySize <- 1 + train$SibSp + train$Parch

# Setup factors
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$MissingAge <- as.factor(train$MissingAge)

# Subset data to features we wish to keep/use.
train <- train %>%
  select(Survived, Pclass, Age, Sex, Embarked, MissingAge, 
         SibSp, Parch, Fare, FamilySize)
str(train)

###
# Impute missing Ages
###

# Caret supports a number of machanisum for imputing (predicting)
# missing values. Leverage bagged decision trees to impute missing 
# values of Ages.

# First transform all feature to dummy variables.
dummy.vars <- dummyVars(~ . , data = train[, -1])
train.dummy <- predict(dummy.vars, train[, -1])
View(train.dummy)

# Now, impute
pre.process <- preProcess(train.dummy, method = "bagImpute")
imputed.data <- predict(pre.process, train.dummy)
View(imputed.data)


train$Age <- imputed.data[, 4]
summary(train)


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


cl <- makeCluster(3, type = "SOCK")

# register cluster so that caret will know to train in parallel.
registerDoSNOW(cl)


# Train the xgboost model using 10-fold CV repeated 3 times
# and a hyperparameter frid search to train the optimal model.
caret.cv.XGB <- train( Survived ~ .,
                   data = titanic.train,
                   method = "xgbTree",
                   tunGrid = tune.grid,
                   trControl = train.control)
caret.cv.RF <- train( Survived ~ .,
                      data = titanic.train,
                      method = "rf",
                      tunGrid = tune.grid,
                      trControl = train.control)

caret.cv.xgbDART <- train( Survived ~ .,
                      data = titanic.train,
                      method = "xgbDART",
                      tunGrid = tune.grid,
                      trControl = train.control)

stopCluster(cl)

# Examine caret's processing result
caret.cv.XGB

# Make predictions on the test set using a xgboost model
# trained on 625 rows of the training set using the found 
# optimal hyperparameter values.
preds <- predict(caret.cv.XGB, titanic.test)


# user caret's confusionmatrix() to estimate the 
# effectiveness of this model on unseen, new data.
confusionMatrix(preds, titanic.test[, 1])


confusionMatrix(predict(caret.cv.XGB, titanic.test), titanic.test[, 1])
confusionMatrix(predict(caret.cv.RF, titanic.test), titanic.test[, 1])
confusionMatrix(predict(caret.cv.xgbDART, titanic.test), titanic.test[, 1])



library(ggplot2)

###
# What was the survival rate?
###
ggplot(titanic.train, aes(x = Survived)) +
  geom_bar()


# Add some customization for labels and theme.
ggplot(titanic.train, aes(x = Survived)) +
  theme_bw() +
  geom_bar() +
  labs( y = "Passenger Count",
        title = "Titanic survival Rates")

###
# Second question: what was the survival rate by gender?
###
ggplot(titanic.train, aes(x = Sex, fill = Survived)) +
  theme_bw() +
  geom_bar() +
  labs(y="Passenger Count",
       title = "Titanic survival reates by Sex")


###
# Third question: what was the survival rate by ticket?
###
ggplot(titanic.train, aes(x = Sex, fill = Survived)) +
  theme_bw() +
  geom_bar() +
  labs(y="Passenger Count",
       title = "Titanic survival reates by tickets")


###
# Forth question: what was the survival rate by class of ticket 
#                 and gender?
###
ggplot(titanic.train, aes(x = Sex, fill = Survived)) +
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  labs(y="Passenger Count",
       title = "Titanic survival reates by Sex")

###
# Fifth question: What is the distribution of passenger's age?
#
# The Histogram is staple of visualization of numerical data as 
# it very powerfully communicates the distribution of a variable
###
ggplot(titanic.train, aes(x = Age)) +
  theme_bw() +
  geom_histogram( binwidth = 5) +
    labs(y="Passenger Count",
         x = "Age (bandwidth = 5)",
       title = "Titanic Age distribution")


###
# Sixth question: what was the survival rate by Age?
###
ggplot(titanic.train, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram( binwidth = 5) +
  labs(y="Passenger Count",
       title = "Titanic survival reates by Age")

###
# Seventh question: what was the survival rate by Age when
#                   segmented by Sex and class of ticket?
###
ggplot(titanic.train, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  # geom_density( alpha = 0.5) +
  geom_histogram( bandwidth = 5) +
  labs(y="Age",
       x = "Survived",
       title = "Titanic survival reates by Age, PClass and Sex")


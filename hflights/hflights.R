# Predict whether the flight will be deloyed or not.


# Download the flights data:
#     https://drive.google.com/drive/folders/1um8ovwEa7_lwKdLKPXBcG13ZwdJ5SigW

# install.packages("hflights")
# install.packages("dplyr")
# install.packages("rpart")
# install.packages('plotly')
# install.packages('httpuv')
# install.packages('randomForest')
suppressWarnings(library(dplyr))
suppressWarnings(library(hflights))
suppressWarnings(library(plotly))
suppressWarnings(library(party))
suppressWarnings(library(randomForest))

source('hflights/common.R')

# Importing the dataset and select useful columns only

###
# Format the flight date, set new column Deloayed 1 
# if DepDelayed more than 15 0 otherwise 
###
dataset <- hflights %>%
  mutate(Date = paste(Year, Month, DayofMonth, sep = "-"),
         ActualElapsedTime= ifelse(is.na(ActualElapsedTime), 0, ActualElapsedTime),
         AirTime = ifelse(is.na(AirTime), 0, AirTime),
         ArrDelay = ifelse(is.na(ArrDelay), 0, ArrDelay),
         DepDelay = ifelse(is.na(DepDelay), 0, DepDelay),
         Delayed = ifelse(is.na(DepDelay), 0, ifelse(DepDelay > 15, 1, 0))) %>%
  select(Date, Year, Month, DayofMonth, DayOfWeek, DepTime, ArrTime, 
         ActualElapsedTime, AirTime, ArrDelay, DepDelay, 
         Origin, Dest, Distance, TaxiIn, TaxiOut, 
         Cancelled, CancellationCode, Diverted, Delayed)

head(dataset)

dependent_var <- 'Delayed'

# Find all flights which delayed by more than 15 and mark them as DELAYED 
dataset$Delayed <- ifelse(is.na(dataset$DepDelay), 0, ifelse(dataset$DepDelay > 15, 1, 0))

###
# Add airport city name (Origin and Destination both)
###
airport_ds <- read.csv('airportList.csv', header = FALSE)
colnames(airport_ds) <- c('airport_name', 'code')
head(airport_ds)
airport_ds[,'airport_name']


###
# Derive the Full date
###
dataset$Date <- paste(dataset$Year, dataset$Month, dataset$DayofMonth, sep = '-')


###
# All filghts flying on 1-Jan and delayed 
###
dataset %>%
  filter(Month == 1 , DayofMonth == 1) %>%
  filter(Delayed == 1)


###
# Derive the Full date
###
dataset$Date <- paste(dataset$Year, dataset$Month, dataset$DayofMonth, sep = '-')


###
# All filghts flying on 1-Jan and delayed 
###
dataset %>%
  filter(Month == 1 , DayofMonth == 1) %>%
  filter(Delayed == 1)

dataset %>%
  filter(Cancelled == 0) %>%
  group_by(Origin) %>%
  summarise(avg_delay = round(x = mean(DepDelay), digits = 0),
            min_deplay = min(DepDelay),
            max_delay = max(DepDelay),
            total = n())

dataset %>%
  filter(Cancelled == 0) %>%
  group_by(Dest) %>%
  summarise(avg_delay = round(x = mean(DepDelay), digits = 0),
            min_deplay = min(DepDelay),
            max_delay = max(DepDelay),
            total = n())

##
# replce origin and destination city names
##
# dataset$Origin <- airport_ds[match(dataset$Origin, airport_ds$por_code),'city_name']
# dataset$Dest <- airport_ds[match(dataset$Dest, airport_ds$por_code),'city_name']

dataset$Origin <- factor(dataset$Origin)
dataset$Dest <- factor(dataset$Dest)

dataset$Date <- factor(dataset$Date)
dataset$Year <- factor(dataset$Year)
dataset$Month <- factor(dataset$Month)
dataset$DayofMonth <- factor(dataset$DayofMonth)
dataset$Delayed <- factor(dataset$Delayed)
dataset$DayOfWeek <- factor(dataset$DayOfWeek)

dataset <- dataset %>%
  filter(is.na(DepTime) == FALSE &
           is.na(ArrTime) == FALSE)
#write.csv(dataset, file = "hflights/data/dataset.csv")
summary(dataset)


unique(dataset$Origin)
unique(dataset$Dest)


df <- 
  dataset %>%
  filter(Delayed == 1) %>%
  group_by(Year, Month) %>%
  summarise( avg_delay = round(x = mean(DepDelay, na.rm = TRUE), digits = 0),
             max_delay = max(DepDelay)) %>%
  arrange(Year, Month) %>%
  mutate(Year_Month = paste(Month, Year, sep = '/')) %>%
  ungroup() %>%
  select(Year_Month, avg_delay, max_delay)

head(df)

###
# Visualizing data
###

df %>%
  plot_ly()%>%
  add_trace(x = ~Year_Month, y = ~avg_delay, type = 'bar',
            text = df$avg_delay, textposition = 'auto') %>%
  add_trace(x = ~Year_Month, y = ~max_delay, type = 'bar',
            text = df$max_delay, textposition = 'auto') %>%
  layout(title = "Monthly delayed flights",
         barmode = 'group',
         xaxis = list(title = "Month"),
         yaxis = list(title = "Delay in minutes"))


##------- Predictions  ---------##

###
# Split the dataset into training and test data
###
set.seed(123)
flt_sample <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.9, 0.1))
training_set <- dataset[flt_sample == 1, ]
test_set <- dataset[flt_sample == 2, ]


###
# Create the formula, we will be using it in all of the classification model
###
lm_formula <- as.formula(paste('Delayed', ' ~ DepDelay + ActualElapsedTime + ArrDelay + 
                  ArrTime + AirTime + ArrDelay + Distance + TaxiIn', sep = ''))


glm_model <- classificationModel(train_set = training_set, 
                                 test_set = test_set,
                                 model_formula = lm_formula, 
                                 model_name = 'lr')

svm_model <- classificationModel(train_set = training_set, 
                    test_set = test_set,
                    model_formula = lm_formula, 
                    model_name = 'svm')

nb_model <- classificationModel(train_set = training_set, 
                                 test_set = test_set,
                                 model_formula = lm_formula, 
                                 model_name = 'nb')
dt_model <- classificationModel(train_set = training_set, 
                                 test_set = test_set,
                                 model_formula = lm_formula, 
                                 model_name = 'dt')
rf_model <- classificationModel(train_set = training_set, 
                                 test_set = test_set,
                                 model_formula = lm_formula, 
                                 model_name = 'rf')

###
# Let's create a dataframe to compare the predicted values
# (from different classification models) with actuals
###
Comp_pred <- data.frame('predicted' = test_set[dependent_var])


###--------------------###
# Logistic regression    #
###--------------------###
# Fitting Logistic Regression model on training set
classifier <- glm(formula = lm_formula, family = binomial,data = training_set)

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
classifier <- ctree(formula = lm_formula, data = training_set, controls = ctree_control(mincriterion = 0.9, minsplit = 1000))
# plot(classifier)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$DecisionTree <- y_pred

###--------------------###
# Random Forest          #
###--------------------###
# Fitting Random FOrest model on training set
classifier <- randomForest(formula = lm_formula, data = training_set)
# plot(classifier)

# Predicting the test set results
y_pred <- predict(classifier, newdata = test_set)
Comp_pred$RandomForest <- y_pred

###---------------------###
# K-Nearest Neighbor(KNN) #
###---------------------###
# remove the dependent variable

training_set1 <- select(training_set, -Delayed)
test_set1 <- select(test_set, -Delayed)

ctg_train_lbl <- training_set[, 'Delayed']

suppressWarnings(library(class))
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


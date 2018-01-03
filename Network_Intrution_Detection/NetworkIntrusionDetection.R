# Network Intrution Detection

library(dplyr)

source('CommonFunc.R')

###
# Importing the dataset, read the compressed file directly into dataset
###
dataset_raw <- read.csv(gzfile('data/Network_Intrusion_Train_data.csv.xz' ), stringsAsFactors = TRUE)
dependent_var <- 'class'

###
# use mutate to add additional column to translate categorical data
###
dataset <- dataset_raw %>%
              mutate( protocol_type = ifelse(protocol_type == 'tcp', 0, ifelse(protocol_type == 'udp', 1, 2)),
                  class = ifelse(class == 'normal',1,0))

# Encoding the target feature as factor
dataset$service <- as.numeric(factor(dataset$service, levels = unique(dataset$service)))
dataset$flag <- as.numeric(factor(dataset$flag, levels = unique(dataset$flag)))

dataset$protocol_type <- factor(dataset$protocol_type)
dataset$service <- factor(dataset$service)
dataset$flag <- factor(dataset$flag)
dataset$class <- factor(dataset$class)


summary (dataset)

###
# Split the dataset into training and test data
###
set.seed(123)
flt_sample <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.9, 0.1))
training_set <- dataset[flt_sample == 1, ]
test_set <- dataset[flt_sample == 2, ]
nrow(training_set)
nrow(test_set)


###--------------------###
# Logistic regression    #
###--------------------###
# Fitting Logistic Regression model on training set
func_ret <- getBestSuitableRegressor(dataset = training_set, 
                                     dependent_var = dependent_var,
                                     ignore_vars = c('protocol_type', 'service', 'flag'))
classifier <- func_ret$classifier

summary(classifier)

###
# Predict the results using test data
###
y_pred <- predict(classifier, test_set, type = "response")
y_pred <- ifelse(y_pred > 0.5, 'normal', 'anormaly')

cm <- table(test_set$class, y_pred)
accuracy <- (sum(diag(cm)) / sum(cm)) * 100

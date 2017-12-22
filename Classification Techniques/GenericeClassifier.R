# Generate and compare multiple models on same dataset

## Below package used for SVM
#install.packages('e1071')

## Below packages used for decision tree
#install.packages('rpart')
#install.packages('party')

## Below packages used for Random Forest
# install.packages('randomForest')

my_model <- function(split_size, model_name){
  # Support Vector Machine
  
  # Importing dataset
  dataset <- read.csv("CTG.csv")
  
  summary(dataset)
  
  # Encoding the target feature as factor
  dataset$NSP <- factor(dataset$NSP)
  
  # Splitting the dataset into training and test dataset
  set.seed(123)
  ctg_sample <- sample(2, nrow(dataset), replace = TRUE, prob = c(split_size, 1 - split_size))
  
  training_set <- dataset[ctg_sample == 1, ]
  test_set <- dataset[ctg_sample == 2, ]
  
  if(model_name == 'svm'){
    # Fitting SVM model on training set
    library(e1071)
    classifier <- svm(NSP ~ LB + FM + AC, data = training_set)
  }
  if(model_name == 'nb'){
    # Fitting Naive Bayes model on training set
    classifier <- naiveBayes(NSP ~ LB + FM + AC, data = training_set)
  }
  if(model_name == 'dt'){
    # Fitting Decision Tree model on training set
    library(party)
    classifier <- ctree(formula = NSP ~ LB + FM + AC, data = training_set, controls = ctree_control(mincriterion = 0.9, minsplit = 458))
    # classifier <- ctree(formula = NSP ~ LB + FM + AC, data = training_set)
    # plot(classifier)
  }
  if(model_name == 'rf'){
    # Fitting Random FOrest model on training set
    library(randomForest)
    classifier <- randomForest(formula = NSP ~ LB + FM + AC, data = training_set)
    # plot(classifier)
  }
  
  # Predicting the test set results
  y_pred <- predict(classifier, newdata = test_set)
  
  # Side by side comparision of predicted and actual values
  # df <- data.frame(test_set[,4], y_pred)
  
  ###
  # Let's caculate different parameter, which 
  # may be used to compare the models
  ###
  # Confussion matrix
  cm <- table(test_set[,4], y_pred)
  
  # Calculate other parameters for the model
  model <- as.data.frame(0)
  
  model$accuracy <- (sum(diag(cm))/sum(cm))*100
  
  model$TPR <- (cm[1] / (cm[1] + cm[2]))*100 
  model$FPR <- (cm[2] / (cm[1] + cm[2]))*100 
  model$FNR <- (cm[3] / (cm[3] + cm[4]))*100 
  model$TNR <- (cm[4] / (cm[3] + cm[4]))*100 
  
  model$Recall <- cm[1]/ (cm[1]+cm[3])
  model$Precision <- cm[1]/ (cm[1]+cm[2])
  model$F1Score <- 2*((model$Recall* model$Precision)/(model$Recall + model$Precision))
  model$G_measure <- sqrt(model$Recall*model$Precision)
  
  return(model)
}

my_model(0.9, 'svm')
my_model(0.9, 'nb')
my_model(0.9, 'dt')
my_model(0.9, 'rf')



# install.packages('e1071')
classificationModel <- function(train_set, test_set, model_formula, model_name){

  # Testing code:
  # Splitting the dataset into training and test dataset
  # set.seed(123)
  # ctg_sample <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
  # 
  # train_set <- dataset[ctg_sample == 1, ]
  # test_set <- dataset[ctg_sample == 2, ]
  # 
  # model_formula <- lm_formula
  
  # Find the dependent feature from the formula
  dependent_var <- as.character(model_formula)[2]
  
  if(model_name == 'lr'){
    # Fitting SVM model on training set
    classifier <- glm(formula = model_formula, 
                      family = 'binomial',
                      maxit = 100,
                      na.action = na.omit,
                      data = train_set)
    # Predicting the test set results
    y_pred <- predict(classifier, test_set, type = "response")
    y_pred <- ifelse(y_pred > 0.9, 1, 0)
  }

  if(model_name == 'svm'){
    # Fitting SVM model on training set
    suppressWarnings(library(e1071))
    classifier <- svm(formula = model_formula, data = train_set, 
                                  scale = TRUE,
                                  type = 'C-classification',
                                  kernel= 'linear')
    # Predicting the test set results
    y_pred <- predict(classifier, newdata = test_set)
  }
  if(model_name == 'nb'){
    # Fitting Naive Bayes model on training set
    classifier <- naiveBayes(formula = model_formula, data = train_set)
    # Predicting the test set results
    y_pred <- predict(classifier, newdata = test_set)
  }
  if(model_name == 'dt'){
    # Fitting Decision Tree model on training set
    suppressWarnings(library(party))
    classifier <- ctree(formula = model_formula, data = train_set, 
                        controls = ctree_control(mincriterion = 0.95, 
                                                 minsplit = 800))
    # Predicting the test set results
    y_pred <- predict(classifier, newdata = test_set)
  }
  if(model_name == 'rf'){
    # Fitting Random FOrest model on training set
    suppressWarnings(library(randomForest))
    system.time(classifier <- randomForest(formula = model_formula, 
                                           data = train_set,
                                           keep.forest =  TRUE,
                                           nodesize=4200,
                                           do.trace = FALSE,
                                           ntree = 500, 
                                           proximity=FALSE, 
                                           oob.prox=TRUE))
    # Predicting the test set results
    y_pred <- predict(classifier, newdata = test_set)
  }
  

  # Side by side comparision of predicted and actual values
  # df <- data.frame(test_set[,4], y_pred)
  
  ###
  # Let's caculate different parameter, which 
  # may be used to compare the models
  ###
  # Confussion matrix
  cm <- table(test_set[, dependent_var], y_pred)
  cm
  
  # Calculate other parameters for the model
  model <- as.data.frame(0)
  
  model$accuracy <- (sum(diag(cm))/sum(cm))*100
  
  model$TPR <- (cm[1] / (cm[1] + cm[2]))*100 
  model$FPR <- (cm[2] / (cm[1] + cm[2]))*100 
  model$FNR <- (cm[3] / (cm[3] + cm[4]))*100 
  model$TNR <- (cm[4] / (cm[3] + cm[4]))*100 
  
  model$Recall <- cm[1]/ (cm[1]+cm[2])
  model$Precision <- cm[1]/ (cm[1]+cm[3])
  model$F1Score <- 2*((model$Recall* model$Precision)/(model$Recall + model$Precision))
  model$G_measure <- sqrt(model$Recall*model$Precision)
  
  return(model)
}
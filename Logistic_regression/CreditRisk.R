
# Logistic Regression

library(dplyr)

# Importing the dataset
dataset <- read.csv("CreditRisk.csv")
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

# Feature Scaling
training_set[-12] = scale(training_set[-12])
test_set[-12] = scale(test_set[-12])

# Loop through all the independent variable to find bext suitable logistic model



###
# Let's apply a while loop to iterate throught the list of columns and find out 
# the best model for independent variables.
# in every iteration will reduce one independent variable and will record the Adjusted R-squared
# the model with maximum adj R-Squared will be our best choice
###
factors <- colnames(dataset)
itemToRemove <- dependent_var

lm_formula_lst <- character(0)
sum_reg_lst <- list()

null.deviances <- numeric(0)
deviance.residuals <- numeric(0)   # additional vector for better comparision
aics <- numeric(0)          # additional vector for better comparision  

###
# Iterate the while loop until we are left with two independent variables
###
j <- 1
while (length(factors) > 2) {
  factors <- factors[!factors == itemToRemove]
  lm_formula <- as.formula(paste(paste(dependent_var, ' ~', sep = ''), 
                                 paste(factors, collapse=" + ", sep = '')))
  lm_formula_lst <- c(lm_formula_lst, paste(paste(dependent_var, ' ~', sep = ''),
                                            paste(factors, collapse=" + ", sep = '')))
  
  
  ###
  #Create logistic regresion model
  ###
  classifier  <- glm(formula = lm_formula, family = binomial('logit'), data = training_set, maxit = 100)
  
  sum_reg <- summary(classifier)

    
  # Predicting the Test set results
  prob_pred = predict(classifier, type = 'response', newdata = test_set)
  
  
  y_pred = ifelse(prob_pred > 0.5, 1, 0)
  
  
  # Making the Confusion Matrix
  cm = table(test_set[, 12], y_pred)
  
  cm
  model <- as.data.frame(0)
  
  #model$cm  <-  c(11212, 45) #c(table(test_set[, 12], y_pred)) 
  model$aic <- sum_reg$aic
  model$accuracy <- (sum(diag(cm))/sum(cm))*100
  
  model$TPR <- (cm[1] / (cm[1] + cm[2]))*100 
  model$FPR <- (cm[2] / (cm[1] + cm[2]))*100 
  model$FNR <- (cm[3] / (cm[3] + cm[4]))*100 
  model$TNR <- (cm[4] / (cm[3] + cm[4]))*100 
  
  model$Recall <- cm[1]/ (cm[1]+cm[2])
  model$Precision <- cm[1]/ (cm[1]+cm[3])
  model$F1Score <- 2*((model$Recall* model$Precision)/(model$Recall + model$Precision))
  model$G_measure <- sqrt(model$Recall*model$Precision)
  
  sum_reg_lst[[j]] <- model
  j <- j + 1
  # find out which variable has maximum P-value, we will remove it in next iteration.
  itemToRemove <- rownames(sum_reg$coefficients)[apply(sum_reg$coefficients , 2, which.max)[4]] 
  
  if(all(sum_reg$coefficients[, 4]< 0.05))
    break
  
}


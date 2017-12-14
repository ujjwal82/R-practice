
# Logistic Regression

library(dplyr)

getBestSuitableRegressor <- function(dataset, dependent_var){
  # dataset is our training_set to build the regression model

  ###
  # Remove the spaces if any in the column names
  ###
  colnames(dataset) <- gsub(" ", "", colnames(dataset), fixed = TRUE)
  
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
  return_lst = list()

  ###
  # Iterate the while loop until we are left with two independent variables,
  # or we can remove any more (independent) variables
  ###
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
    
    

    # find out which variable has maximum P-value, we will remove it in next iteration.
    itemToRemove <- rownames(sum_reg$coefficients)[apply(sum_reg$coefficients , 2, which.max)[4]] 
    
    if(all(sum_reg$coefficients[, 4]< 0.05))
      break
    
  }
  
  return_lst = list(classifier, lm_formula)
  return(return_lst)
}

# R does not have a standard in-built function to calculate mode. So we 
# create a user function to calculate mode of a data set in R. This 
# function takes the vector as input and gives the mode value as output.

# Create a function to calculate and return the mode of values
getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create a function to calculate and return the Accuracy of a model
calcAccuracy <- function(y_pred, actuals){
  cm <- table(y_pred, actuals)
  sum(diag(cm))/sum(cm)*100
}

library(MASS)
# install.packages('neuralnet')
library(neuralnet)


set.seed(123)
dataset <- Boston

# Histogram of the medv
hist(dataset$medv)

#dataset <- as.data.frame(scale(dataset,center = minValue, scale = maxValue))

# Splitting the dataset into training set and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$medv, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

training_set[-14] <- as.data.frame(scale(training_set[-14]))
test_set[-14] <- as.data.frame(scale(training_set[-14]))

# Fitting the NeuralNet model

# Let's take some configuration for neural network
# 13-4-2-1
# Variables: 13
# Hidden units in first layer: 4
# Hidden units in first layer: 2
# output : 1

allvars <- colnames(dataset)
allvars <- allvars[! allvars %in% 'medv']
formula <- as.formula(paste('medv ~', paste(allvars, collapse = " + ")))

neuralModel <- neuralnet(formula = formula, hidden = c(4, 2),
                         linear.output = T,
                         data = training_set
                         )

# Visulazing the model
plot(neuralModel)

# Predicting the test set results
predictions <- compute(neuralModel, test_set[, 1:13])

predictions <- predictions$net.result
actualValues <- test_set[14]

MSE <- sum((predictions - actualValues)^2)/nrow(test_set)
MSE

plot(test_set$medv, predictions, col = 'blue', main = "Real vs Predicted",
     pch = 1, cex = 0.9, type ='p')


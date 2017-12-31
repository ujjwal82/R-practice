library(MASS)
# install.packages('neuralnet')
library(neuralnet)


set.seed(123)
dataset <- Boston

# Structure of the dataset
str(dataset)

# Histogram of the medv
hist(dataset$medv)


maxValue <- apply(dataset, 2, max)
minValue <- apply(dataset, 2, min)

dataset <- as.data.frame(scale(dataset,center = minValue, scale = maxValue))

# Splitting the dataset into training set and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$medv, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)


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

plot(neuralModel)

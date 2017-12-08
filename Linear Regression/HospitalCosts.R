# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('HospitalCosts.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$TOTCHG, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
#training_set = scale(training_set[c(-5,-6)])
#test_set = scale(test_set[c(-5,-6)])

regressor <- lm(formula = TOTCHG ~ ., data = training_set)
y_pred <- predict(regressor, test_set)

summary(regressor)


df <- as.data.frame(c(y_pred, test_set[5]))
####
# Do a side by side comparision of actual and predited data
###
df <- data.frame(y_pred,  test_set$TOTCHG)

colnames(df) <- c('Predited', 'Actual')


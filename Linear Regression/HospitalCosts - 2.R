# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('HospitalCosts.csv')

dataset$APRDRG <- factor(dataset$APRDRG)
table(dataset$APRDRG)


## ------------------------------------------

aa <- subset(x = dataset, APRDRG == 640)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(aa$TOTCHG, SplitRatio = 0.8)
training_set = subset(aa, split == TRUE)
test_set = subset(aa, split == FALSE)

model_640 <- lm(formula = TOTCHG ~ AGE + FEMALE + LOS + RACE, data = training_set)
summary(model_640)

model_640 <- lm(formula = TOTCHG ~ AGE + FEMALE + LOS, data = training_set)
summary(model_640)
y_pred <- predict(model_640, test_set)

## ---------------------------

aa <- subset(x = dataset, APRDRG == 754)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(aa$TOTCHG, SplitRatio = 0.8)
training_set = subset(aa, split == TRUE)
test_set = subset(aa, split == FALSE)

model_754 <- lm(formula = TOTCHG ~ AGE + FEMALE + LOS + RACE, data = training_set)
summary(model_754)

model_754 <- lm(formula = TOTCHG ~ AGE + LOS + RACE, data = training_set)
summary(model_754)
y_pred <- predict(model_754, test_set)

## ---------------------------




df <- as.data.frame(c(y_pred, test_set[5]))
####
# Do a side by side comparision of actual and predited data
###
df <- data.frame(y_pred,  test_set$TOTCHG)

colnames(df) <- c('Predited', 'Actual')

##
unique( dataset$APRDRG)
table( dataset$APRDRG)

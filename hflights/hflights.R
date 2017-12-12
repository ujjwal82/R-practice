# Predict whether the flight will be deloyed or not.


# install.packages("hflights")
library(hflights)

data("hflights")
nrow(hflights)
colnames(hflights)

###
# Add new column which will indicate that the flight was deloyed (Y/N)
# we determine this by DepDelay being greater that 15
###

# Find all flights which delayed by more than 15 and mark them as DELAYED 
hflights$Delayed <- ifelse(is.na(hflights$DepDelay),'N',ifelse(hflights$DepDelay > 15, 'Y', 'N'))
head(hflights)


###
# Check if we have sufficiently non-biased data, based on Deloyed column
###
Delayed_Y_N <- table(hflights$Delayed)
flightDelayedRate <- Delayed_Y_N[2] *100 /sum(Delayed_Y_N)


###
# Remove the flight records which were cancelled
###
hflights <- hflights[hflights$Cancelled == 0, ]
nrow(hflights)

###
# Remove the columns which are not much significant.
###
hflights <- hflights[, colnames(hflights) != c('UniqueCarrier', 'FlightNum', 'TailNum')]
head(hflights)


###
# List flights on January 1st, which were delayed
###
library(dplyr)
hflights %>%
  group_by()%>%
  filter( Month == 1) %>%
  filter( DayofMonth == 1) %>%
  filter( Delayed == 'Y')


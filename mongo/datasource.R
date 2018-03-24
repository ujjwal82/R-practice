library(mongolite)


datasour <- function(query) {
  
  m <- mongo("DeviceData", 
             url = "mongodb://savenindia:saven1ndia@54.196.58.142:27017/DeviceData")
  
  print(m)
  
  
  result <- m$find('{ "A":date1,"B":"1"}')
  result1 <- m$find('{ "A":date2,"B":"1"}')
  #subs= m$count(query="{}")
  #query <- data(
  #  '{ "A":"20170915150018","B":"1"}'
  #)
  
  print(result)
  print(subs)
  
  
  rm(m)
  gc()
  
}
setwd("/Users/burakberkerbasergun/Downloads/")

data <- read.csv("chatXDD.csv",sep = ";",dec = ",")
?read.csv

id <- 474551
set.seed(id)
myData <- as.data.frame(data[sample(1:10000,200,replace=FALSE),])

library(tidyverse)
library(dplyr)
#Before 
str(myData)

myData$breakdownTime <- as.Date(myData$breakdownTime)
# Employees stay the same
myData$siteTraffic <- as.integer(myData$siteTraffic)
myData$errorCode <- as.factor(myData$errorCode)
myData$topic <- as.factor(myData$topic)

#After
str(myData)



#### 2 ####
colnames(myData)[1] <- "indicentTime"

#### 3 ####
myData$topic <- as.factor(myData$topic)


library(tidyr)


head(myData)
#### 4 ####
# Change microservice to factor
myData <- separate(myData, col = errorCode ,  
                   into = c("errorNumber", "microservice"),  
                   sep = "-") 


#### 5 ####


econTrend <- myData[myData$topic =="economics",]
summary(econTrend)
# 30



myData %>% 
  filter(topic=='economics') %>%
  summarise(max(employees, na.rm = TRUE))

#### 6 ####



max(myData$hoursToImprove[myData$employees>=16] , na.rm = TRUE)

#### 7 ####
myData$minutesFixing<- myData$hoursToImprove * 60
head(myData)

#### 8 ####
boxplot(myData$minutesFixing ~ myData$microservice, xlab = "Microservice Type", ylab = "Minutes Fixing")

#### 9 ####
myModel <- lm(hoursToImprove ~ employees + siteTraffic + topic + microservice, data= myData)
coefficientEmployee <- myModel$coefficients["employees"]
print(coefficientEmployee)


#### 10 ####

# Use summary to create summarise table
# Use 
summaryTable <- myData %>% 
  group_by(microservice, topic) %>% 
  summarise(averageEmployees = mean(employees, na.rm = T), 
            maximumFixTimeInMinutes = max(minutesFixing, na.rm = T)) %>%
  arrange(microservice, topic)

print(summary_table)

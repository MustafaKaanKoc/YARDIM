data <- read.csv("chatXDD.csv", sep=";" ,dec = ",",  header = TRUE)
id <- 476456 # YOUR ID HERE 
set.seed(id) 
myData <- as.data.frame(data[sample(1:10000,200,replace=FALSE),])
#### TASK 1
str(myData)
#breakdownTime - char / date
#employees - int / num
#siteTraffic - num / num
#errorCode - char / factor
#topic - char / factor
#hoursToImprove - num / num
myData$breakdownTime <- as.Date(myData$breakdownTime, format = "%Y-%m-%d")
myData$employees <- as.numeric(myData$employees)
myData$siteTraffic <- as.numeric(myData$siteTraffic)
myData$errorCode <- as.factor(myData$errorCode)
myData$topic <- as.factor(myData$topic)
myData$hoursToImprove <- as.numeric(myData$hoursToImprove)
head(myData)

#### TASK 2
colnames(myData)[colnames(myData) == "breakdownTime"] = "indicentTime"
colnames(myData)

#### TASK 3
myData$topic <- as.factor(myData$topic)
str(myData)

#### TASK 4
library(tidyr)
myData <- separate(myData, col = errorCode, into = c("errorNumber", "microservice"), sep = "-")

### For No Reason
myData$errorNumber <- as.numeric(myData$errorNumber)
myData$microservice <- as.factor(myData$microservice)


str(myData)

#### TASK 5

max(myData$employees[myData$topic == "economics"], na.rm = TRUE)

#### TASK 6

max(myData$hoursToImprove[myData$employees>=16] , na.rm = TRUE)

#### TASK 7

myData$minutesFixing<- myData$hoursToImprove * 60
head(myData)

#### TASK 8

boxplot(myData$minutesFixing ~ myData$microservice, xlab="Microservice type")

#### TASK 9

myModel<- lm(hoursToImprove ~ employees + siteTraffic + topic + microservice, data=myData)
myModel$coefficients["employees"]
coef(myModel)["employees"] # GPT VERSION

#### TASK 10

library(dplyr)

summary_table <- myData %>%
  group_by(microservice, topic) %>%
  summarise(
    averageEmployees = mean(employees, na.rm = TRUE),
    maximumFixTimeInMinutes = max(minutesFixing, na.rm = TRUE)
  ) %>%
  arrange(microservice, topic)

print(summary_table)


data <- read.csv("/Users/burakberkerbasergun/Downloads/galacticTravel.csv", sep = ";", dec=",")# READ DATA HERE
# IMPORTANT!! Remember about separator and decimal!!!
id <- 474551 # YOUR ID HERE
set.seed(id)
myData <- as.data.frame(data[sample(1:10000,500,replace=FALSE),])

############### TASK 1
str(myData)

head(myData)
# TravelDate : chr - date
# Destination: chr - factor
# Species    : chr - factoır
# TravelClass: chr - factor
# Duration   : int - num
# PromoTokens: int - num
# Price      : chr - num

                                ### ???????### ???????### ???????

myData$Destination <- as.factor(myData$Destination)
myData$Species <- as.factor(myData$Species)
myData$TravelClass <- as.factor(myData$TravelClass)
myData$TravelDate <- as.Date(myData$TravelDate, format = "%Y_%m_%d")
myData$Price <- as.numeric(myData$Price)                                ### ???????### ???????### ???????
myData$PromoTokens <- as.numeric(myData$PromoTokens)
myData$Duration <- as.numeric(myData$Duration) 
str(myData)


##### TASK 2


colnames(myData)[colnames(myData) == "PromoTokens"] <- "PromotionTokens"
head(myData)


#### TASK 3


# It contains a name of planet and also a letter describes as a planet or a station

library(tidyr)
myData <- separate(myData, col = Destination ,  
                   into = c("DestinationPlace", "StationOrPlanet"),  
                   sep = "_") 
head(myData)

######### TASK 4 Transform the ‘TravelDate’ variable to the appropriate type. Make sure your changes affect the
####.     data.frame that you are operating on


myData$TravelDate <- as.Date(myData$TravelDate, format = "%Y_%m_%d")
str(myData)

##### TASK 5. Among the travels, which received at least 8 promotion tokens, assess how individuals of different
##### Species undertook them (TIP – remember how to take a look inside of a factor variable). 


atLeast_8 <- myData[myData$PromotionTokens >= 8,]
summary(atLeast_8)
# Andromedan:39 , Human     :36, Jovian    :34, Martian   :44, Saturnian :42, Venutian  :45


#### TASK 6.  Add a new variable to the dataset, which will store the price of a trip calculated per one Standard
#### Galactic Day (SGD). Name it “PricePerDay”. TIP – divide (and conquer J).
head(myData)
myData$price_per_SGD <- myData$Price / myData$Duration
head(myData)



#### TASK 7: Calculate the median price of travel to Venus in Luxury class. 
is.null(myData$Price)

median(myData$Price[myData$DestinationPlace=="Venus" &  myData$TravelClass == "Luxury"])

### TASK 8:Create a plot which will store TWO histograms side by side, which will show the distribution of
### travel time towards Mars and Venus. Change the color of bars for the first plot to “red”, and color
### of bars in the second plot to “blue”. 
unique(myData$DestinationPlace)
myData$DestinationPlace <- as.factor(myData$DestinationPlace)
myData$StationOrPlanet <- as.factor(myData$StationOrPlanet)




venusPlot  <- myData$Duration[myData$DestinationPlace == "Venus"]
marsPlot  <- myData$Duration[myData$DestinationPlace == "Mars"]

par(mfrow=c(1, 2))
hist(venusPlot, main="Histogram of Venus", xlab="Duration", 
    ylab="Frequency", col="blue")


#hist(myData$Duration[myData$DestinationPlace == "Venus"], col = "red")

hist(marsPlot, main="Histogram of Mars", xlab="Duration", 
     ylab="Frequency", col="red")


##### TASK 9 Build a linear model “myModel” (function lm()) that explains the price of the travel as a function
# of the trip destination (both variables if you did the task 3 successfully), travel class, travel
# duration, used promotional tokens and Species that undertook the travel. Extract the coefficient for
# trip duration and print it out. 
head(myData)
myModel <- lm(Price ~ DestinationPlace + StationOrPlanet + TravelClass + Duration + PromotionTokens + Species, data = myData)

myModel$coefficients


### TASK 10:


library(dplyr)

summary_table <- myData %>%
  group_by(Species, TravelClass) %>%
  summarise(
    averagePrice = mean(Price, na.rm = TRUE),
    maxPromoTokens = max(PromotionTokens , na.rm = TRUE)
  ) %>%
  arrange(Species, TravelClass)

print(summary_table)
## task 11:

myData<-myData %>% mutate(MySpeciesAverage = ave(Price, Species, FUN = mean))
head(myData)



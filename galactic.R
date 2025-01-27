data <- read.csv("galacticTravel.csv", sep=";", dec = ",",  header = TRUE)
id <- 476456 # YOUR ID HERE 
set.seed(id) 
myData <- as.data.frame(data[sample(1:10000,500,replace=FALSE),]) 

##### Task 1
str(myData)
# travel date- char / date
# species - char / factor
# travel class- char / factor
# duration - int / num
# price - num / num
myData$TravelDate <- as.Date(myData$TravelDate, format = "%Y_%m_%d")
myData$Destination <- as.factor(myData$Destination)
myData$Species <- as.factor(myData$Species)
myData$TravelClass <- as.factor(myData$TravelClass)
myData$Duration <- as.numeric(myData$Duration)

######  TASK 2
head(myData)
colnames(myData)[colnames(myData) == "PromoTokens"] = "PromotionTokens"
colnames(myData)
######  TASK 3
library(tidyr)
myData <- separate(myData, col = Destination, into = c("DestinationPlace", "StationOrPlanet"), sep = "_")
str(myData)
######  TASK 4
myData$TravelDate <- as.Date(myData$TravelDate, format = "%Y_%m_%d")
str(myData)
######  TASK 5
eight_or_more<- myData[myData$PromotionTokens >= 8,]
summary(eight_or_more)
#### A=44, H=36, J=38, M=41, S=32, V=40
# andromedan <- eight_or_more[eight_or_more$Species == "Andromedan",]
# martian <- eight_or_more[eight_or_more$Species == "Martian",]
# Jovian <- eight_or_more[eight_or_more$Species == "Jovian",]
# Venutian <- eight_or_more[eight_or_more$Species == "Venutian",]
# Saturnian <- eight_or_more[eight_or_more$Species == "Saturnian",]
# Human <- eight_or_more[eight_or_more$Species == "Human",]

####### TASK 6
myData$PricePerDay<- myData$Price/myData$Duration
head(myData)

######  TASK 7 **************************
median(myData$Price[myData$TravelClass=="Luxury" & myData$DestinationPlace =="Venus"], na.rm = TRUE)

######  TASK 8 **************************
par(mfrow = c(1, 2))
mars_travel_time<- myData$Duration[myData$DestinationPlace=="Mars"]
hist(myData$Duration[myData$DestinationPlace=="Mars"],  col = "red")
hist(myData$Duration[myData$DestinationPlace=="Venus"], col = "blue")


######  TASK 9
myModel <- lm(Price ~ DestinationPlace + StationOrPlanet + TravelClass + Duration + PromotionTokens + Species, data = myData)
myModel$coefficients["Duration"]

######  Task 10 GPT BABAAAAAGGG
library(dplyr)

summary_table <- myData %>%
  group_by(Species, TravelClass) %>%
  summarise(
    averagePrice = mean(Price, na.rm = TRUE),
    maxPromoTokens = max(PromotionTokens, na.rm = TRUE)
  ) %>%
  arrange(Species, TravelClass)

print(summary_table)

######   Task 11 GPT BABAAAAAGGG

myData<-myData %>% mutate(MySpeciesAverage = ave(Price, Species, FUN = mean))
head(myData)

######   BONUS

#Optimal Pricing Strategy:
#  Using insights from the linear model, the following data-driven pricing strategy is proposed for the Galactic Travel Agency:
#  
#  Adjust Prices by Destination and Duration:
#  
#  The coefficient for Duration indicates that longer trips significantly increase costs. Adjust prices to include a base fare plus a variable charge per day of travel.
#Introduce premium pricing for destinations like Andromeda and Venus, as these are likely luxury destinations with higher demand, indicated by their frequent association with luxury travel classes.
#Incorporate Travel Class:
#  
#  Maintain higher price multipliers for luxury and business classes, as the model shows a strong positive relationship between these classes and price. Offer discounts for economy class to attract budget-conscious travelers.
#Promotional Discounts:
#  
#  Use promotional tokens strategically. Increase the maximum tokens for underbooked destinations or off-peak travel dates to encourage bookings and optimize fleet usage.
#Species-Specific Pricing:
#  
#  Base fares can vary by species, considering differences in their average willingness to pay, as shown by MySpeciesAverage. For example, Jovians may value shorter trips and benefit from reduced fares for brief journeys.
#Rationale:
#  This strategy balances revenue optimization with customer satisfaction by leveraging trends from the data. It aligns with the positive and negative relationships identified in the linear model (e.g., duration, destination, and travel class) while remaining competitive and adaptive to intergalactic preferences.
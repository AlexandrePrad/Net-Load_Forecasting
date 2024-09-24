################################################################################
############### Net-Load Forecasting During the "Soberty" Period ###############
############################## Data Visualization ############################## 
################################################################################

rm(list=objects())
graphics.off()

library(tidyverse)
library(lubridate)

#setwd("C:/Users/alici/Documents/Cours/Universite/MIA/M1/Modélisation prédictive")


###########################################################
######## Data Import
#####################

train <- read_csv('Data/train.csv')
test <- read_csv('Data/test.csv')


head(train)
summary(train)
names(train)

str(train)
train$WeekDays = as.factor(train$WeekDays)
test$WeekDays = as.factor(test$WeekDays)

range(train$Date)
range(test$Date)


###########################################################
######## Descriptive Data Analysis
###################################

par(mfrow=c(1,1))

#Demand distribution (our target)
hist(train$Net_demand, breaks=100)
#looks like two Gaussians


#Correlation between demand and temperature
par(mfrow = c(1, 1))
plot(train$Date, train$Net_demand, type = 'l', xlab = 'temps', ylab = "Demande Nette")
par(new = T)
plot(train$Date, train$Temp, type = 'l', col = 'red', axes = F, xlab = '', ylab = '')
axis(side = 4, col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Temperature', col = 'red')
legend("top", c("Net_demand", "Temperature"), col = c("black", "red"), lty = 1, ncol = 1, bty = "n")
#Temperature is probably a significant feature



#Electricity consumption as a function of time
plot(train$Date, train$Load, type='l')



#Consumption and production by the sun and wind
plot(train$Date, train$Load, type = 'l', ylim = range(train$Solar_power, train$Load), col = 1)
lines(train$Date, train$Wind_power, col = 3)
lines(train$Date, train$Solar_power, col = 2)
#Renewable energy production well below consumption


#Consumption, solar and wind generation by month
par(mfrow=c(3,1))
boxplot(Net_demand ~ Month, data = train, col = 2)
boxplot(Solar_power ~ Month, data = train, col = 3)
boxplot(Wind_power ~ Month, data = train, col = 4)
#A period of high consumption (winter) and one of lower consumption (summer) are clearly visible.


#A enlever
summer = subset(train, Month == 4 | Month == 5 | Month == 6 | Month == 7 | Month == 8 | Month == 9)
par(mfrow = c(1, 1))
hist(summer$Net_demand, breaks=100, proba = TRUE)
curve(dnorm(x, mean(summer$Net_demand), sd(summer$Net_demand)), add = TRUE)

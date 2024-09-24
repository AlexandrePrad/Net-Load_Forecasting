################################################################################
############### Net-Load Forecasting During the "Soberty" Period ###############
################################ Random Forest ################################# 
################################################################################

rm(list=objects())
graphics.off()

library(tidyverse)
library(lubridate)
library(randomForest)

#setwd("C:/Users/alici/Documents/Cours/Universite/MIA/M1/Modélisation prédictive")
#source('Scripts prof/score.R')

train <- read_csv('Data/train.csv')
test <- read_csv('Data/test.csv')
train$WeekDays = as.factor(train$WeekDays)
test$WeekDays = as.factor(test$WeekDays)


###########################################################
######## Data Preprocessing
############################

#Some variables are missing from the test dataset : 
#Load, Net_demand, Solar_power, Wind_power

Train = train[ , -2] #Load
Train = Train[ , -5] #Solar_power
Train = Train[ , -5] #Wind_power
#We keep Net_demand for our labels


#We add our variables
Train$Temp_trunc1 <- pmax(Train$Temp-285,0)
Train$Temp_trunc2 <- pmax(Train$Temp-295,0)
test$Temp_trunc1 <- pmax(test$Temp-285,0)
test$Temp_trunc2 <- pmax(test$Temp-295,0)


#We separate our dataset into train and test to be able to compare errors.
n = nrow(Train)
set.seed(12)
ind = sample(1:n, size = 0.2*n)
train_bis = Train[-ind, ]
test_bis = Train[ind, ]

#We'll try to optimize the rmse and pinball_loss


###########################################################
######## Modelisation
######################

#Model with all the variables
rf = randomForest(Net_demand ~ ., data = train_bis)
rf
y_rf = predict(rf, newdata = test_bis, type = "response")
rmse_rf = sqrt(mean((y_rf - test_bis$Net_demand)^2))
rmse_rf #1593.374

res = train_bis$Net_demand - predict(rf, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_rf_quant = y_rf + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_rf_quant, quant = 0.95, output.vect=FALSE)
#240.7518



#Model with selected variables
rf1 = randomForest(Net_demand ~ Date + Load.1 + I(Load.1**2) + Temp + I(Temp^2) + 
                     Temp_s95 + I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + 
                     Temp_s95_max + I(Temp_s95_max^2) + Wind + Wind_weighted + 
                     Nebulosity + Nebulosity_weighted + toy + I(toy^2) + WeekDays + 
                     BH_before + BH + Year + Month + I(Month^2) + DLS + Summer_break + 
                     Christmas_break + BH_Holiday + Wind_power.7 + Net_demand.7 + 
                     Temp_trunc1 + Temp_trunc2, data = train_bis)
rf1
y_rf1 = predict(rf1, newdata = test_bis, type = "response")
rmse_rf1 = sqrt(mean((y_rf1 - test_bis$Net_demand)^2))
rmse_rf1 #1612.577

res = train_bis$Net_demand - predict(rf1, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_rf1_quant = y_rf1 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_rf_quant, quant = 0.95, output.vect=FALSE)
#240.7518



#------------------------------
# Conclusion on Gaussian models
#------------------------------

#RF models are very high pinball loss

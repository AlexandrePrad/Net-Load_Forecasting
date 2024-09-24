################################################################################
############### Net-Load Forecasting During the "Soberty" Period ###############
################################## GAM Models ################################## 
################################################################################

rm(list=objects())
graphics.off()

library(tidyverse)
library(lubridate)
library(mgcv)

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

#For this part, we're going to do the opposite of Gaussian models. 
#We'll start our model with one variable, then gradually add more.
#The variables we'll be adding are those already selected in our model 4 (Gaussian model).
#Here, spline functions will replace truncated variables.


#We start by putting the most significant variables
gam1 = gam(Net_demand ~ s(Load.1, bs = 'cr') + s(Temp, k = 10, bs = 'cr') +  
            s(Wind_weighted) + WeekDays + BH_before + s(Month, k = 5, bs = 'tp') + 
            DLS + Christmas_break + BH_Holiday + + s(Wind_power.1), data = train_bis, family = gaussian)
summary(gam1) 
#plot(gam)
y_g1 = predict(gam1, newdata = test_bis, type = "response")
rmse_g1 = sqrt(mean((y_g1 - test_bis$Net_demand)^2))
rmse_g1 #1730.332

res = train_bis$Net_demand - predict(gam1, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_g1_quant = y_g1 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_g1_quant, quant = 0.95, output.vect=FALSE)
#107.7598



#We add all the variables of model 4
gam2 = gam(Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') + s(Year) + 
             s(Load.1, bs = 'cr') + s(Load.7, bs = 'cr') + s(Temp, k = 10, bs = 'cr') +  
             s(Wind) + s(Wind_weighted) + s(Nebulosity) + s(Nebulosity_weighted)  + 
             WeekDays + BH + BH_before + BH_Holiday + s(Month, k = 5, bs = 'tp') + 
             DLS + Christmas_break + Summer_break + BH_Holiday + s(Wind_power.1) + 
             s(Wind_power.7) + s(Net_demand.7) + s(Temp_s95, k = 10, bs = 'cr') + 
             s(Temp_s95_min, k = 10, bs = 'cr') + s(Temp_s95_max, k = 10, bs = 'cr') + 
             s(toy, k = 30, bs = 'cc'),
            data = train_bis, family = gaussian)
summary(gam2) 
#plot(gam2)
y_g2 = predict(gam2, newdata = test_bis, type = "response")
rmse_g2 = sqrt(mean((y_g2 - test_bis$Net_demand)^2))
rmse_g2 #986.5421

res = train_bis$Net_demand - predict(gam2, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_g2_quant = y_g2 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_g2_quant, quant = 0.95, output.vect=FALSE)
#105.4296
#Public score = 157.46 (q65)



#We do not keep those that do not provide significant information or improvements.
gam3 = gam(Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') + s(Year) + 
             s(Load.1, bs = 'cr') + s(Load.7, bs = 'cr') + s(Wind) + 
             s(Wind_weighted) + s(Nebulosity) + s(Nebulosity_weighted) + BH + 
             BH_before + WeekDays + DLS + Christmas_break + BH_Holiday + 
             s(Wind_power.7) + s(Net_demand.7) + s(Temp, k = 10, bs = 'cr') + 
             s(Temp_s95, k = 10, bs = 'cr') + s(Temp_s95_min, k = 10, bs = 'cr') + 
             s(toy, k = 30, bs = 'cc'), data = train_bis, family = gaussian)
summary(gam3) 
#plot(gam3)
y_g3 = predict(gam3, newdata = test_bis, type = "response")
rmse_g3 = sqrt(mean((y_g3 - test_bis$Net_demand)^2))
rmse_g3 #1013.081

res = train_bis$Net_demand - predict(gam3, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_g3_quant = y_g3 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_g3_quant, quant = 0.95, output.vect=FALSE)
#107.5244
#Public score = 156.27 #q55




#-------------------------
# Conclusion on GAM models
#-------------------------

#The last two models have similar performance levels, 
#so it's hard to tell them apart.


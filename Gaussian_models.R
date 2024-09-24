################################################################################
############### Net-Load Forecasting During the "Soberty" Period ###############
############################### Gaussian Models ################################ 
################################################################################

rm(list=objects())
graphics.off()

library(tidyverse)
library(lubridate)

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


#----------------------------
# Model with all our features
#----------------------------

model = glm(Net_demand ~ . , data = Train, family = 'gaussian')

#We start with a model containing all our variables, then, using a Student test, 
#we select only those which are significant (i.e. which provide information).
#Significant features(Student test): *, ** or ***
#Date, Load.1, Load.7, Temp, Temp_s95, Temp_s95_min, Temp_s95_max, Wind, Wind_weighted,
#Nebulosity_weighted, toy, WeekDays, BH_before, BH, Year, Month, DLS,
#Summer_break, Christmas_break, BH_Holiday, Wind_power.7, Net_demand.7


#Modele with all our features
model1 = glm(Net_demand ~ . , data = train_bis, family = 'gaussian')
summary(model1)
y_m1 = predict(model1, newdata = test_bis, type = "response")
rmse_m1 = sqrt(mean((y_m1 - test_bis$Net_demand)^2))
rmse_m1 #1523.366

res = train_bis$Net_demand - predict(model1, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_m1_quant = y_m1 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_m1_quant, quant = 0.95, output.vect=FALSE)
#175.8852



#--------------------------------------
# Model with only significant variables
#--------------------------------------

#We look at the distribution of our target with each explanatory variable.
#We adjust the linearity

plot(Train$Net_demand ~ Train$Temp) #Example with Temp
#It looks like x²

model2 = glm(Net_demand ~ Date + Load.1 + I(Load.1^2) + Load.7 + I(Load.7^2) + 
               Temp + I(Temp^2) + Temp_s95 + I(Temp_s95^2) + Temp_s95_min + 
               I(Temp_s95_min^2) + Temp_s95_max + I(Temp_s95_max^2) + Wind + 
               Wind_weighted + Nebulosity + Nebulosity_weighted + toy + 
               I(toy^2) + WeekDays + BH_before + BH + Year + Month + I(Month^2) + 
               DLS + Summer_break + Christmas_break + BH_Holiday + Wind_power.7 + 
               Net_demand.7, data = train_bis, family = gaussian)
summary(model2)
y_m2 = predict(model2, newdata = test_bis, type = "response")
rmse_m2 = sqrt(mean((y_m2 - test_bis$Net_demand)^2))
rmse_m2 #1191.419

res = train_bis$Net_demand - predict(model2, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_m2_quant = y_m2 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_m2_quant, quant = 0.95, output.vect=FALSE)
#131.3644

#Nebulosity scores higher, so we keep it, and Load.1² too (it adds information)


#We re-select our significant variables
model3 = glm(Net_demand ~ Date + Load.1 + I(Load.1^2) + Temp + I(Temp^2) + 
               Temp_s95 + I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + 
               Temp_s95_max + I(Temp_s95_max^2) + Wind_weighted + Nebulosity + 
               toy + I(toy^2) + WeekDays + BH_before + BH + Year + Month + 
               I(Month^2) + DLS + Christmas_break + BH_Holiday, 
               data = train_bis, family = 'gaussian')
summary(model3)
y_m3 = predict(model3, newdata = test_bis, type = "response")
rmse_m3 = sqrt(mean((y_m3 - test_bis$Net_demand)^2))
rmse_m3 #1215.662

res = train_bis$Net_demand - predict(model3, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_m3_quant = y_m3 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_m3_quant, quant = 0.95, output.vect=FALSE)
#137.3768

#A little worse than model 2, so we won't be keeping it.



#--------------------------------
# Models with truncated variables
#--------------------------------

#We add truncated variables to better adapt to linear models

plot(Train$Net_demand ~ Train$Temp)
abline(v = 285)
abline(v = 295)
Train$Temp_trunc1 <- pmax(Train$Temp-285,0)
Train$Temp_trunc2 <- pmax(Train$Temp-295,0)
test$Temp_trunc1 <- pmax(test$Temp-285,0)
test$Temp_trunc2 <- pmax(test$Temp-295,0)

plot(Train$Net_demand ~ Train$toy)
abline(v = 0.4)
abline(v = 0.56)
abline(v = 0.615)
abline(v = 0.77)
Train$toy_trunc1 <- pmax(Train$toy - 0.4,0)
Train$toy_trunc2 <- pmax(Train$toy - 0.56,0)
Train$toy_trunc3 <- pmax(Train$toy - 0.615,0)
Train$toy_trunc4 <- pmax(Train$toy - 0.77,0)
test$toy_trunc1 <- pmax(test$toy - 0.4,0)
test$toy_trunc2 <- pmax(test$toy - 0.56,0)
test$toy_trunc3 <- pmax(test$toy - 0.615,0)
test$toy_trunc4 <- pmax(test$toy - 0.777,0)

n = nrow(Train)
set.seed(12)
ind = sample(1:n, size = 0.2*n)
train_bis = Train[-ind, ]
test_bis = Train[ind, ]


# Model with Temp_trunc
model4 = glm(Net_demand ~ Date + Load.1 + I(Load.1^2) + Temp + I(Temp^2) + 
               Temp_s95 + I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + 
               Temp_s95_max + Wind_weighted + Nebulosity + toy + I(toy^2) + 
               WeekDays + BH_before + BH + Year + Month + I(Month^2) + DLS + 
               Summer_break + Christmas_break + BH_Holiday + Wind_power.7 + 
               Net_demand.7 + Temp_trunc1 + Temp_trunc2, 
               data = train_bis, family = gaussian)
summary(model4)
y_m4 = predict(model4, newdata = test_bis, type = "response")
rmse_m4 = sqrt(mean((y_m4 - test_bis$Net_demand)^2))
rmse_m4 #1197.684

res = train_bis$Net_demand - predict(model4, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_m4_quant = y_m4 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_m4_quant, quant = 0.95, output.vect=FALSE)
#136.7281
#Model with only trunc temp has a public score of 148.24 for quantile 75
#Public score = 147.96 for quantile 74

#To find the right quantile each time, we just test a range of different 
#quantiles (55, 65, 75, 85, 95).
#Then, if we wanted to refine, we looked for a quantile between those that gave 
#us the two best scores.
#The same procedure will be followed for each model of interest.



# Model with Temp_trunc and toy_trunc
model5 = glm(Net_demand ~ Date + Load.1 + I(Load.1^2) + Temp + I(Temp^2) + 
               Temp_s95 + I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + 
               Temp_s95_max + Wind_weighted + Nebulosity + toy + I(toy^2) + 
               WeekDays + BH_before + BH + Year + Month + I(Month^2) + DLS + 
               Summer_break + Christmas_break + BH_Holiday + Wind_power.7 + 
               Net_demand.7 + Temp_trunc1 + Temp_trunc2, toy_trunc1 + toy_trunc2 + 
               toy_trunc3 + toy_trunc4, data = train_bis, family = gaussian)
summary(model5)
y_m5 = predict(model5, newdata = test_bis, type = "response")
rmse_m5 = sqrt(mean((y_m5 - test_bis$Net_demand)^2))
rmse_m5 #1440.137

res = train_bis$Net_demand - predict(model5, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_m5_quant = y_m5 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_m5_quant, quant = 0.95, output.vect=FALSE)
#141.2256
#Public score 156.6 for quantile 69



#Model 5 isn't very good, so we're adding cuts to see if it improves
plot(Train$Net_demand ~ Train$toy)
abline(v = 0.05)
abline(v = 0.4)
abline(v = 0.56)
abline(v = 0.615)
abline(v = 0.665)
abline(v = 0.77)
abline(v = 0.925)
Train$toy_trunc1 <- pmax(Train$toy - 0.05,0)
Train$toy_trunc2 <- pmax(Train$toy - 0.4,0)
Train$toy_trunc3 <- pmax(Train$toy - 0.56,0)
Train$toy_trunc4 <- pmax(Train$toy - 0.615,0)
Train$toy_trunc5 <- pmax(Train$toy - 0.665,0)
Train$toy_trunc6 <- pmax(Train$toy - 0.77,0)
Train$toy_trunc7 <- pmax(Train$toy - 0.925,0)
test$toy_trunc1 <- pmax(test$toy - 0.05,0)
test$toy_trunc2 <- pmax(test$toy - 0.4,0)
test$toy_trunc3 <- pmax(test$toy - 0.56,0)
test$toy_trunc4 <- pmax(test$toy - 0.615,0)
test$toy_trunc5 <- pmax(test$toy - 0.665,0)
test$toy_trunc6 <- pmax(test$toy - 0.777,0)
test$toy_trunc7 <- pmax(test$toy - 0.925,0)

n = nrow(Train)
set.seed(12)
ind = sample(1:n, size = 0.2*n)
train_bis = Train[-ind, ]
test_bis = Train[ind, ]

model6 = glm(Net_demand ~ Date + Load.1 + I(Load.1**2) + Temp + I(Temp^2) + 
               Temp_s95 + I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + 
               Temp_s95_max + I(Temp_s95_max^2) + Wind + Wind_weighted + 
               Nebulosity + Nebulosity_weighted + toy + I(toy^2) + WeekDays + 
               BH_before + BH + Year + Month + I(Month^2) + DLS + Summer_break + 
               Christmas_break + BH_Holiday + Wind_power.7 + Net_demand.7 + 
               Temp_trunc1 + Temp_trunc2 + toy_trunc1 + toy_trunc2 + toy_trunc3 + 
               toy_trunc4 + toy_trunc5 + toy_trunc6 + toy_trunc7, 
               data = Train, family = gaussian)
summary(model6)
y_m6 = predict(model6, newdata = test_bis, type = "response")
rmse_m6 = sqrt(mean((y_m6 - test_bis$Net_demand)^2))
rmse_m6 #1127.883

res = train_bis$Net_demand - predict(model6, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_m6_quant = y_m6 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_m6_quant, quant = 0.95, output.vect=FALSE)
#128.9382
#But public score = 158, it's worst than our model with only Temp_trunc



#------------------------------
# Conclusion on Gaussian models
#------------------------------

#We will keep only model 4 for improvement, because he has the highest public score.







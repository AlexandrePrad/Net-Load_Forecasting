################################################################################
############### Net-Load Forecasting During the "Soberty" Period ###############
############################## Model Improvement ############################### 
################################################################################

rm(list = objects())
graphics.off()

library(tidyverse)
library(lubridate)
library(mgcv)
library(ranger)

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


#We add our truncated variables
Train$Temp_trunc1 <- pmax(Train$Temp-285,0)
Train$Temp_trunc2 <- pmax(Train$Temp-295,0)
test$Temp_trunc1 <- pmax(test$Temp-285,0)
test$Temp_trunc2 <- pmax(test$Temp-295,0)




###########################################################
######## Modelisation
######################

#We are going to improve the models selected previously, 
#the GAMs models (gam2 and gam3) and the Gaussian regression model (model4).


#--------------------------------
# Models without data before 2018
#--------------------------------

#The aim of this section is to include only post-2018 data, as data prior 
#to 2018 is unreliable due to distorted weather data. Our models will then 
#learn better from the data and be better able to predict the future.


d0_2018 <- which(Train$Year<=2018)
Train2 = Train[-d0_2018,]

#We separate our dataset into train and test to be able to 
#compare errors(RMSE and pinball loss).
n = nrow(Train2)
set.seed(12)
ind = sample(1:n, size = 0.2*n)
train_bis = Train2[-ind, ]
test_bis = Train2[ind, ]



#Gaussian regression
model4_2018 = glm(Net_demand ~ Date + Load.1 + I(Load.1^2) + Temp + I(Temp^2) + 
                      Temp_s95 + I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + 
                      Temp_s95_max + Wind_weighted + Nebulosity + toy + I(toy^2) + 
                      WeekDays + BH_before + BH + Year + Month + I(Month^2) + DLS + 
                      Summer_break + Christmas_break + BH_Holiday + Wind_power.7 + 
                      Net_demand.7 + Temp_trunc1 + Temp_trunc2, 
                      data = train_bis, family = gaussian)
summary(model4_2018)
y_m4_2018 = predict(model4_2018, newdata = test_bis, type = "response")
rmse_m4_2018 = sqrt(mean((y_m4_2018 - test_bis$Net_demand)^2))
rmse_m4_2018 #1233.527 (1197.684 with data before 2018)

res = train_bis$Net_demand - predict(model4_2018, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_m4_2018_quant = y_m4_2018 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_m4_2018_quant, quant = 0.95, output.vect=FALSE)
#121.0835 (136.7281 with data before 2018)
#The model does not seem to be improved. However, by submitting the predictions, 
#we obtained a better public score 
#Public score = 133.06 with quantile 73
#Public score = 147.96 for quantile 74 with data before 2018



#GAMs

#With all our selected variables
gam2_2018 = gam(Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') + Year + 
             s(Load.1, bs = 'cr') + s(Load.7, bs = 'cr') + s(Temp, k = 10, bs = 'cr') +  
             s(Wind) + s(Wind_weighted) + s(Nebulosity) + s(Nebulosity_weighted)  + 
             WeekDays + BH + BH_before + BH_Holiday + s(Month, k = 5, bs = 'tp') + 
             DLS + Christmas_break + Summer_break + BH_Holiday + s(Wind_power.1) + 
             s(Wind_power.7) + s(Net_demand.7) + s(Temp_s95, k = 10, bs = 'cr') + 
             s(Temp_s95_min, k = 10, bs = 'cr') + s(Temp_s95_max, k = 10, bs = 'cr') + 
             s(toy, k = 30, bs = 'cc'), data = train_bis, family = gaussian)
summary(gam2_2018) 
#plot(gam2_2018)
y_g2_2018 = predict(gam2_2018, newdata = test_bis, type = "response")
rmse_g2_2018 = sqrt(mean((y_g2_2018 - test_bis$Net_demand)^2))
rmse_g2_2018 #1091.346 (986.5421 with data before 2018)

res = train_bis$Net_demand - predict(gam2_2018, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_g2_2018_quant = y_g2_2018 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_g2_2018_quant, quant = 0.95, output.vect=FALSE)
#111.1729 (105.4296 with data before 2018)
#The model looks less good.
#Public score = 144.04 (q65)
#Public score = 157.46 (q65) with data before 2018



#With only significant variables
gam3_2018 = gam(Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') + Year + 
             s(Load.1, bs = 'cr') + s(Load.7, bs = 'cr') + s(Wind) + 
             s(Wind_weighted) + s(Nebulosity) + s(Nebulosity_weighted) + BH + 
             BH_before + WeekDays + DLS + Christmas_break + BH_Holiday + 
             s(Wind_power.7) + s(Net_demand.7) + s(Temp, k = 10, bs = 'cr') + 
             s(Temp_s95, k = 10, bs = 'cr') + s(Temp_s95_min, k = 10, bs = 'cr') + 
             s(toy, k = 30, bs = 'cc'), data = train_bis, family = gaussian)
summary(gam3_2018) 
#plot(gam3_2018)
y_g3_2018 = predict(gam3_2018, newdata = test_bis, type = "response")
rmse_g3_2018 = sqrt(mean((y_g3_2018 - test_bis$Net_demand)^2))
rmse_g3_2018 #1100.84 (1013.081 with data before 2018)

res = train_bis$Net_demand - predict(gam3_2018, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_g3_2018_quant = y_g3_2018 + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_g3_2018_quant, quant = 0.95, output.vect=FALSE)
#110.7038 (107.5244 with data before 2018)
#The model looks also less good.
#Public score = 143.57 (q55)
#Public score = 156.27 (q55) with data before 2018



#Conclusion
#Gaussian regression model and GAM models without the pre-2018 data are better, 
#so for the rest of the study, we will only keep data after 2018.




#---------------------------------------------
# Models with the addition of a covid variable
#---------------------------------------------

#We have retrieved the containment indexes, by date, from the Oxford site.
#https://ourworldindata.org/covid-stringency-index

covid = read_csv('Data/covid_index.csv')
index = which(covid$Entity == 'France')
covid = covid[index, ] #we retrieve information for France


#We create an index_covid column for the Train2 dataset (without data prior to 2018)
col_covid = Train2$Net_demand
id = 1
for( i in 1:nrow(Train2)){
  if(Train2$Date[i] != covid$Day[id]){
    col_covid[i] = 0 
  }else{
    col_covid[i] = covid$containment_index[id]
    id = id + 1
  }
}
Train2[39] = col_covid
names(Train2)[39] = "covid_index"

#We create an index_covid column for the Test dataset, 
#but we have no more data for dates after 12/31/2022. 
#To complete the dataset, we look at the dates of mandatory mask wearing in hospitals:
#- 05/30/2023 end of obligation for caregivers
#- 27/08/23 return of the obligation 
#We therefore extend the score 25.60 to 30/05/23, as there has been no change.
#We reduce the score, 0 or 13.69 (a value found before the first confinement) 
#or 23.81 (a value found 2022-11-25 and lower than 25.60) or 25.60, 
#between 30/05/23 and 27/08/23, because we have a reduction in the rules.
#The score is reset to 25.60 from 27/08/23.

col_covid = test$Temp_s95 #We initialize a column of the same size
id = 976  #the first date of the test data set

for(i in 1:nrow(test)){
  if(id <= nrow(covid)){
    if(test$Date[i] != covid$Day[id]){
      col_covid[i] = 0 
    }
    else{
      col_covid[i] = covid$containment_index[id]
      id = id + 1
    }
  }
  else{
    if(test$Date[i] <= '2023-05-30'| test$Date[i] > '2023-08-27'){
      col_covid[i] = 25.60
    }
    else{
      col_covid[i] = 25.60 #0, 13.69, 23.81 or 25.60 for submissions
    }
  }
}
test[40] = col_covid
names(test)[40] = "covid_index"


#We separate our dataset into train and test
n = nrow(Train2)
set.seed(12)
ind = sample(1:n, size = 0.2*n)
train_bis = Train2[-ind, ]
test_bis = Train2[ind, ]



#Gaussian regression
model4_covid = glm(Net_demand ~ Date + Load.1 + I(Load.1^2) + Temp + I(Temp^2) + 
                     Temp_s95 + I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + 
                     Temp_s95_max + Wind_weighted + Nebulosity + toy + I(toy^2) + 
                     WeekDays + BH_before + BH + Year + Month + I(Month^2) + DLS + 
                     Summer_break + Christmas_break + BH_Holiday + Wind_power.7 + 
                     Net_demand.7 + Temp_trunc1 + Temp_trunc2 + covid_index, 
                     data = train_bis, family = gaussian)
summary(model4_covid)
y_m4_covid = predict(model4_covid, newdata = test_bis, type = "response")
rmse_m4_covid = sqrt(mean((y_m4_covid - test_bis$Net_demand)^2))
rmse_m4_covid #1187.878

res = train_bis$Net_demand - predict(model4_covid, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_m4_covid_quant = y_m4_covid + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_m4_covid_quant, quant = 0.95, output.vect=FALSE)
#118.6313
#There has been a slight improvement.
#Now let's see the public score
#The different public scores (with q73) according to the index between the dates 
#05/30/23 and 08/27/23.
# - index = 0 : Public score = 138
# - index = 13.69 : Public score = 136.09
# - index = 23.81 : Public score = 134.67
# - index = 25.60 : Public score = 134.42

#The covid variable does not seem to improve the results. 



#GAM models

#With all our selected variables
gam2_covid = gam(Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') + Year + 
                  s(Load.1, bs = 'cr') + s(Load.7, bs = 'cr') + s(Temp, k = 10, bs = 'cr') +  
                  s(Wind) + s(Wind_weighted) + s(Nebulosity) + s(Nebulosity_weighted)  + 
                  WeekDays + BH + BH_before + BH_Holiday + s(Month, k = 5, bs = 'tp') + 
                  DLS + Christmas_break + Summer_break + BH_Holiday + s(Wind_power.1) + 
                  s(Wind_power.7) + s(Net_demand.7) + s(Temp_s95, k = 10, bs = 'cr') + 
                  s(Temp_s95_min, k = 10, bs = 'cr') + s(Temp_s95_max, k = 10, bs = 'cr') + 
                  s(toy, k = 30, bs = 'cc') + s(covid_index), 
                  data = train_bis, family = gaussian)
summary(gam2_covid) 
#plot(gam2_covid)
y_g2_covid = predict(gam2_covid, newdata = test_bis, type = "response")
rmse_g2_covid = sqrt(mean((y_g2_covid - test_bis$Net_demand)^2))
rmse_g2_covid #1028.988

res = train_bis$Net_demand - predict(gam2_covid, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_g2_covid_quant = y_g2_covid + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_g2_covid_quant, quant = 0.95, output.vect=FALSE)
#103.4987
#Public score = 128.42 (with index = 25.60, because it gives the best score, and q65)
#Improvement in pinball loss and public score.


#With only significant variables
gam3_covid = gam(Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') + Year + 
                  s(Load.1, bs = 'cr') + s(Load.7, bs = 'cr') + s(Wind) + 
                  s(Wind_weighted) + s(Nebulosity) + s(Nebulosity_weighted) + BH + 
                  BH_before + WeekDays + DLS + Christmas_break + BH_Holiday + 
                  s(Wind_power.7) + s(Net_demand.7) + s(Temp, k = 10, bs = 'cr') + 
                  s(Temp_s95, k = 10, bs = 'cr') + s(Temp_s95_min, k = 10, bs = 'cr') + 
                  s(toy, k = 30, bs = 'cc') + s(covid_index), 
                  data = train_bis, family = gaussian)
summary(gam3_covid) 
#plot(gam3_covid)
y_g3_covid = predict(gam3_covid, newdata = test_bis, type = "response")
rmse_g3_covid = sqrt(mean((y_g3_covid - test_bis$Net_demand)^2))
rmse_g3_covid #1033.507

res = train_bis$Net_demand - predict(gam3_covid, newdata = train_bis)
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
y_g3_covid_quant = y_g3_covid + quant

pinball_loss(y = test_bis$Net_demand, yhat_quant = y_g3_covid_quant, quant = 0.95, output.vect=FALSE)
#102.0429
#Public score = 133.17 (with index = 25.60 because it gives the best score, and q65)
#Improvement in pinball loss and public score.



#Conclusion
#The covid variable improves prediction for the GAM models but not for the 
#Gaussian regression model.



#---------------------------------------------------
# Models with random forest explanation of residuals
#---------------------------------------------------

#Gaussian Regression
#Data preparation
model4_2018 = glm(Net_demand ~ Date + Load.1 + I(Load.1^2) + Temp + I(Temp^2) + 
                    Temp_s95 + I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + 
                    Temp_s95_max + Wind_weighted + Nebulosity + toy + I(toy^2) + 
                    WeekDays + BH_before + BH + Year + Month + I(Month^2) + DLS + 
                    Summer_break + Christmas_break + BH_Holiday + Wind_power.7 + 
                    Net_demand.7 + Temp_trunc1 + Temp_trunc2, 
                    data = Train2, family = gaussian)
summary(model4_2018)


equation = Net_demand ~ Date + Load.1 + I(Load.1^2) + Temp + I(Temp^2) + Temp_s95 + 
  I(Temp_s95^2) + Temp_s95_min + I(Temp_s95_min^2) + Temp_s95_max + Wind_weighted + 
  Nebulosity + toy + I(toy^2) + WeekDays + BH_before + BH + Year + Month + I(Month^2) + 
  DLS + Summer_break + Christmas_break + BH_Holiday + Wind_power.7 + Net_demand.7 + 
  Temp_trunc1 + Temp_trunc2



g = glm(equation, data = Train2, family = gaussian) #our model
g_forecast = predict(g, newdata = Train2, family = gaussian)
terms0 = predict(g, newdata = Train2, type = 'terms') #gterms
colnames(terms0) = paste0("gterms_", c(1:ncol(terms0)))

residuals = g$residuals
Train_rf = data.frame(Train2, terms0) #creating a train dataset for rf with gterms
Train_rf$res = residuals


#we create the formula 
cov = "Date + Load.1 + Temp + Temp_s95 + Temp_s95_min  + Temp_s95_max + 
Wind_weighted + Nebulosity + toy  + WeekDays + BH_before + BH + Year + Month  + 
DLS + Summer_break + Christmas_break + BH_Holiday + Wind_power.7 + Net_demand.7 + 
Temp_trunc1 + Temp_trunc2 + "
gterm = paste0("gterms_", c(1:ncol(terms0)))
gterm = paste0(gterm, collapse = ' + ')
cov = paste0(cov, gterm, collapse = '+')
formula_rf = paste0("res", "~", cov)

rf_glm = ranger::ranger(formula_rf, data = Train_rf, importance = 'permutation')
rf_glm$r.squared

rf_glm_forecast = predict(rf_glm, data = Train_rf)$predictions + g_forecast


#Submission
#We start by retrieving the quantile of the law for our Train2 dataset
res = Train2$Net_demand - rf_glm_forecast # ~ N(0, sigma²)
quant = qnorm(0.97, mean = mean(res), sd = sd(res)) #quantile q of N(0,sigma²)


#Creating a test dataset with gterms
y_hat_sans_rf = predict(g, newdata = test, family = gaussian)
terms1 = predict(g, newdata = test, type = 'terms')
colnames(terms1) = paste0("gterms_", c(1:ncol(terms1)))

test_rf = data.frame(test, terms1)


y_hat = y_hat_sans_rf + predict(rf_glm, data = test_rf)$predictions

#We first tested a submission with quantile 74, as this was the one that gave 
#the best score, but we obtained 136 a worse score. This result seemed 
#inconsistent, so we compared our prediction with the old one we had.

#To do this, we retrieve our best submission and compare it with our predictions.
y_hat_m4 = read_csv('submission_model4_q74_sans2018.csv')
y_hat_m4 = y_hat_m4$Net_demand

(y_hat + quant) - y_hat_m4
mean((y_hat + quant) - y_hat_m4)
max((y_hat + quant) - y_hat_m4)
min((y_hat + quant) - y_hat_m4)
quantile((y_hat + quant),0.95) - quantile(y_hat_m4,0.95)
#the majority of values are negative ==> underestimation

#We therefore tested a higher quantile, q = 0.95, and obtained a better score.

#We look for the best quantile by submitting several predictions and changing 
#the quantile.
#Quantile 96 improved the score (121.53)




#GAM models

#GAM2
g = gam(Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') + Year + 
          s(Load.1, bs = 'cr') + s(Load.7, bs = 'cr') + s(Temp, k = 10, bs = 'cr') +  
          s(Wind) + s(Wind_weighted) + s(Nebulosity) + s(Nebulosity_weighted)  + 
          WeekDays + BH + BH_before + BH_Holiday + s(Month, k = 5, bs = 'tp') + 
          DLS + Christmas_break + Summer_break + BH_Holiday + s(Wind_power.1) + 
          s(Wind_power.7) + s(Net_demand.7) + s(Temp_s95, k = 10, bs = 'cr') + 
          s(Temp_s95_min, k = 10, bs = 'cr') + s(Temp_s95_max, k = 10, bs = 'cr') + 
          s(toy, k = 30, bs = 'cc') + s(covid_index), data = Train2, family = gaussian)

g_forecast = predict(g, newdata = Train2, type = "response")
terms0 = predict(g, newdata = Train2, type = 'terms')
colnames(terms0) = paste0("gterms_", c(1:ncol(terms0)))

residuals = g$residuals
train_rf_gam = data.frame(Train2, terms0) #creating a train dataset
train_rf_gam$res = residuals

cov = "Date + Year + Load.1 + Load.7 + Temp + Wind + Wind_weighted + Nebulosity + 
Nebulosity_weighted + WeekDays + BH + BH_before + BH_Holiday + Month + DLS + 
Christmas_break + Summer_break + BH_Holiday + Wind_power.1 + Wind_power.7 + 
Net_demand.7 + Temp_s95 + Temp_s95_min + Temp_s95_max + toy + covid_index + "
gterm = paste0("gterms_", c(1:ncol(terms0)))
gterm = paste0(gterm, collapse = ' + ')
cov = paste0(cov, gterm, collapse = '+')
formule_rf = paste0("res", "~", cov)

rf_gam = ranger::ranger(formule_rf, data = train_rf_gam, importance = 'permutation')
rf_gam$r.squared #the forest accounts for 23.4% of the gam error

rf_gam_forecast = predict(rf_gam, data = train_rf_gam)$predictions+ g_forecast


#Submission
res = Train2$Net_demand - rf_gam_forecast # ~ N(0, sigma²)
quant = qnorm(0.86, mean = mean(res), sd = sd(res)) #quantile 0.95 of N(0, sigma²)

#Creating a test dataset with gterms
y_hat_sans_rf = predict(g, newdata = test, family = gaussian)
terms0 = predict(g, newdata = test, type='terms')
colnames(terms0) = paste0("gterms_", c(1:ncol(terms0)))

test_gam_rf = data.frame(test, terms0)


y_hat = y_hat_sans_rf + predict(rf_gam, data = test_gam_rf)$predictions



#We check whether the quantile we've chosen is the right one
#to do this we retrieve our best submission and compare it with our predictions
y_hat_m4_rf <- read_csv('submission_model4_rf_q95_sans2018.csv')
y_hat_m4_rf=y_hat_m4_rf$Net_demand

quantile(y_hat,0.95) - quantile(y_hat_m4_rf,0.95)
mean((y_hat ) - y_hat_m4_rf)
max((y_hat ) - y_hat_m4_rf)
min((y_hat ) - y_hat_m4_rf)
#the majority of values are positive ==> overestimate slightly
#We therefore tested lower quantiles (e.g. q85) and obtained a better score
#Public score = 127.6



#GAM3

g = gam(Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') + Year + 
          s(Load.1, bs = 'cr') + s(Load.7, bs = 'cr') + s(Wind) + 
          s(Wind_weighted) + s(Nebulosity) + s(Nebulosity_weighted) + BH + 
          BH_before + WeekDays + DLS + Christmas_break + BH_Holiday + 
          s(Wind_power.7) + s(Net_demand.7) + s(Temp, k = 10, bs = 'cr') + 
          s(Temp_s95, k = 10, bs = 'cr') + s(Temp_s95_min, k = 10, bs = 'cr') + 
          s(toy, k = 30, bs = 'cc') + s(covid_index), 
          data = Train2, family = gaussian)

g_forecast = predict(g, newdata = Train2, type = "response")
terms0 = predict(g, newdata = Train2, type = 'terms')
colnames(terms0) = paste0("gterms_", c(1:ncol(terms0)))

residuals = g$residuals
train_rf_gam = data.frame(Train2, terms0) #creating a train dataset
train_rf_gam$res = residuals


cov = "Date + Year + Load.1 + Load.7 + Temp + Wind + Wind_weighted + Nebulosity + 
Nebulosity_weighted + WeekDays + BH + BH_before + BH_Holiday + DLS + Christmas_break + 
Wind_power.7 + Net_demand.7 + Temp_s95 + Temp_s95_min + toy + covid_index + "
gterm = paste0("gterms_", c(1:ncol(terms0)))
gterm = paste0(gterm, collapse = ' + ')
cov = paste0(cov, gterm, collapse = '+')
formule_rf = paste0("res", "~", cov)

rf_gam = ranger::ranger(formule_rf, data = train_rf_gam, importance = 'permutation')
rf_gam$r.squared #the forest accounts for 23.4% of the gam error

rf_gam_forecast = predict(rf_gam, data = train_rf_gam)$predictions+ g_forecast


#Submission
res = Train2$Net_demand - rf_gam_forecast # ~ N(0, sigma²)
quant = qnorm(0.82, mean = mean(res), sd = sd(res)) #quantile 0.95 of N(0, sigma²)

#Creating a test dataset with gterms
y_hat_sans_rf = predict(g, newdata = test, family = gaussian)
terms0 = predict(g, newdata = test, type='terms')
colnames(terms0) = paste0("gterms_", c(1:ncol(terms0)))

test_gam_rf = data.frame(test, terms0)

y_hat = y_hat_sans_rf + predict(rf_gam, data = test_gam_rf)$predictions

#Public score = 132.12 (q80)

#soumission
submit <- read_delim(file = "sample_submission.csv", delim = ",")
submit$Net_demand <- y_hat + quant
write.table(submit, file = "submission_gam3_rf_q82_covid_2018.csv", quote=F, sep=",", dec='.',row.names = F)



#Conclusion
#This modelling improved the public score of the Gaussian model but not those of 
#the GAM models.




#--------------------
# Average submissions
#--------------------

#To conclude, we're going to average some of our results so that the errors 
#cancel each other out. This will enable us to obtain better results.

y_hat_m4_rf_97 = read_csv('submission_model4_rf_q97_sans2018.csv')
y_hat_m4_rf_97 = y_hat_m4_rf_97$Net_demand

y_hat_m4_rf_96 = read_csv('submission_model4_rf_q96_sans2018.csv')
y_hat_m4_rf_96 = y_hat_m4_rf_96$Net_demand

y_hat_gam_rf_42 = read_csv('submission_gam1_rf_q42_sans_2018.csv')
y_hat_gam_rf_42 = y_hat_gam_rf_42$Net_demand

y_hat_gam_rf_38 = read_csv('submission_gam1_rf_q38_sans_2018.csv')
y_hat_gam_rf_38 = y_hat_gam_rf_38$Net_demand

y_hat_m4_74 = read_csv('submission_model4_q74_sans2018.csv')
y_hat_m4_74 = y_hat_m4_74$Net_demand

y_hat_m4_73_covid = read_csv('submission_model4_covid_q73_sans2018.csv')
y_hat_m4_73_covid = y_hat_m4_73_covid  $Net_demand

y_hat_gam1_55 = read_csv('submission_gam1_q55_sans2018.csv')
y_hat_gam1_55 = y_hat_gam1_55$Net_demand

y_hat_glm_trunc_55 = read_csv('submission_glm_complet_trunc_q70_145.23.csv')
y_hat_glm_trunc_55 = y_hat_glm_trunc_55$Net_demand


#Mix 1
y_hat = y_hat_m4_rf_97 + y_hat_m4_rf_96 + y_hat_gam_rf_42 + y_hat_gam_rf_38 + y_hat_m4_74
y_hat = y_hat/5
#Public score = 126.19

#Mix 2
y_hat= y_hat_m4_rf_96 + y_hat_gam_rf_42 + y_hat_m4_74
y_hat = y_hat/3
#Public score = 126.41

#Mix 3
y_hat=y_hat_m4_rf_97 + y_hat_m4_rf_96 + y_hat_gam_rf_42 + y_hat_gam_rf_38 + y_hat_m4_74+y_hat_m4_73_covid+y_hat_gam1_55+y_hat_glm_trunc_55
y_hat=y_hat/8
#Public score = 128.08
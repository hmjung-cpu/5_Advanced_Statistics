rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)

load("wage2.RData")
names(data)
KWW <- data$KWW
educ <- data$educ
lwage <- data$lwage

#1(a)regress KWW on educ
lm(KWW~educ) #b0=17.57, b1=1.35

#1(b)regress educ on lwage
model <- lm(lwage~educ) #b0=5.973063, b1=0.059839
summary(model)
#Coefficient
model$coefficients #0.0598392 
#Residual Standard error (Like Standard Deviation) #0.4003195
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
n=length(model$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error

#1(c)regress educ, KWW on lwage
model2 <- lm(lwage~educ+KWW)
summary(model2) #b0=5.761978, b1=0.043620, b2=0.012017, Residual standard error=0.3915
#Coefficient
model2$coefficients #educ=0.04361977,KWW=0.01201686 
#Residual Standard error (Like Standard Deviation) #0.3914786
k=length(model2$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model2$residuals**2)
n=length(model2$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error

#1(d)run a regression of educ on KWW and save the residuals
model3 <- lm(educ~KWW)
model3.residuals <- model3$residuals
#run a regression of lwage on the residuals
model4 <- lm(lwage ~ model3.residuals)
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("sjlabelled")
library(sjPlot)
library(sjmisc)
library(sjlabelled)
summary(model4)
tab_model(model4)

#2(h)
install.packages("readxl")
library(readxl)
verify.data <- read_excel("AS_HW2_check.xlsx")

verify.model <- lm(verify.data$Y ~ verify.data$X)
summary(verify.model) #b0=816.63, b1=-8.44, Multiple R-squared:  0.1516
#t-statistics, p-value
#verify.data$X -1.196, 0.266113   

-8.44-1.96*7.06
-8.44+1.96*7.06

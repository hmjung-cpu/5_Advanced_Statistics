rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)
library(estimatr) #standard error
library(stargazer) #nice output table result
library(sandwich) #robust estimator is sometimes called sandwich estimator
library(lmtest)
library(margins) 
library(haven) #because our data is stata data
load("gpa1.RData")
names(data)

#a
reg <- lm(colGPA ~ hsGPA + ACT + skipped + PC, data=data)
summary(reg)

#b
resid.sq <- (reg$residuals)^2
y.hat <- reg$fitted.values
y.hat.sq <- (reg$fitted.values)^2

white.test <- lm(resid.sq ~ y.hat+y.hat.sq)
summary(white.test)
#c
h.hat <- white.test$fitted.values
weights <- 1/h.hat

WLS <- lm(colGPA ~ hsGPA + ACT + skipped + PC, data=data, weights = weights)
summary(WLS)

lpm <- lm(fraud ~ election_viol + election_viol2 + num_closed_polls + dist2kabul + elevation, data=df)
summary(lpm) #standard error formula used(but we have heteroskedastic error so it would not be correct)

#getting robuts standard errors
vcv <- vcovHC(WLS, type='HC2') #technically different standard error formula. we will keep use HC2 but it does not matter what you use actually
#this is from sandwich

coeftest(WLS, vcov=vcv) #we will have same coefficient estimate but have new robust error



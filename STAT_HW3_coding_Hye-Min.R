rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)

data <- read.csv("cpsmar12_chi.csv")
##################################################################
#a.1.reg wages on age
reg.dollar <- lm(data$wages~data$age)
reg.dollar
  #(Intercept)     data$age  
  #6884.8        732.6 
  #for every additional year of age, expected wage rises by $732.6
data <- data %>%
  mutate(wages_cents=data$wages*100) %>%
  mutate(age_months=data$age*12) %>%
  mutate(sq_age=(data$age)^2) %>%
  mutate(thirty = ifelse(data$age==30,1,0)) %>%
  mutate(forty = ifelse(data$age==40,1,0)) %>%
  mutate(fifty = ifelse(data$age==50,1,0))

#a.2.reg wages_cents on age
reg.cents <- lm(data$wages_cents~data$age)
reg.cents
  #  (Intercept)     data$age  
  #688476        73262 

73262/12

#a.3.reg wages_cents on age_months
reg.centsmonth <- lm(data$wages_cents~data$age_months)
reg.centsmonth
  #(Intercept)  data$age_months  
  #688476           6105 
6105*0.01*12
##################################################################
#b.1.marginal effect of wage~age
reg.i <- lm(data$wages~data$age) #6884.8        732.6  
beta1.i=6884.8 ; beta2.i= 732.6  
(beta1.i+31*beta2.i)-(beta1.i+30*beta2.i)
(beta1.i+41*beta2.i)-(beta1.i+40*beta2.i)
(beta1.i+51*beta2.i)-(beta1.i+50*beta2.i)

#b.2.margianl effect of wage~age+age^2
reg.ii <- lm(data$wages~data$age+data$sq_age)
reg.ii
#-85218.56      5789.51       -61.98  
#wage=5789.51*age-61.98*age^2
beta1.ii=5789.51 ; beta2.ii=-61.98  
31*beta1.ii + 31^2*beta2.ii - (30*beta1.ii+30^2*beta2.ii) 
41*beta1.ii + 41^2*beta2.ii - (40*beta1.ii+40^2*beta2.ii) 
51*beta1.ii + 51^2*beta2.ii - (50*beta1.ii+50^2*beta2.ii) 

#c.1. reg wages ~ male, collgrad, and an interaction term between male and collgrad
reg.c.1<- lm(wages~male+collgrad+I(male*collgrad), data=data)
reg.c.1
#(Intercept)           male       collgrad  male:collgrad  
#16265          11369          30309          27170  

#c.2. reg wages ~ just collgrad and the interaction 
#between male and collgrad (where the male dummy by itself is omitted).
reg.c.2 <- lm(wages~collgrad+I(male*collgrad), data=data)
reg.c.2
#(Intercept)            collgrad  I(male * collgrad)  
#21676               24898               38539  

(1.83-1.50)/2.13
307+198-3-1

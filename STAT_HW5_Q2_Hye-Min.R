rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)

load("mroz.RData")
#indicator
#woman was in the labor for dummy
#standard measures of educational attainment
#work experience
#square of actual work experience

#a
reg_lpm <- lm(inlf ~ educ + exper + expersq, data=data)
summary(reg_lpm)
sum(data$exper>25.9)
#b
inlfhat_lpm <- reg_lpm$fitted.values
summary(inlfhat_lpm)
#c
reg_probit <- glm(inlf ~ educ + exper + expersq, family = binomial(link = "probit"), 
                data = data)
summary(reg_probit)
inlfhat_probit <- reg_probit$fitted.values
range(inlfhat_probit)
summary(inlfhat_probit)

#e
library(estimatr) #standard error
library(stargazer) #nice output table result
library(sandwich) #robust estimator is sometimes called sandwich estimator
library(lmtest)
library(margins) 
library(haven) #because our data is stata data
library(tidyverse)
#average partial effect
probit_marg_APE <- margins(reg_probit)
summary(probit_marg_APE)
#partial effect at average

probit_marg <- margins(probit)
summary(probit_marg)

reg_probit <- glm(inlf ~ educ + exper + expersq, family = binomial(link = "probit"), 
                  data = data)
summary(reg_probit)
margins(reg_probit, data=find_data(reg_probit), variables=NULL, 
        at=tibble(educ=mean(data$educ), 
                  exper=mean(data$exper),
                  expersq=mean(data$expersq)), type='response') 

#mroz <- glm(inlf ~ educ + exper + expersq, family = binomial(link = "probit"), 
                  data = data)
#mroz<- mroz %>%
  mutate(ape=dnorm(probit$coefficients[[1]]
                   +probit$coefficients[[2]]*educ
                   +probit$coefficients[[3]]*exper
                   +probit$coefficients[[4]]*expersq)*probit$coefficients[[2]])

#sum(mroz$ape)/nrow(mroz)

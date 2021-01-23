rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)

load("vote1.RData")

##2.a
reg <- lm(voteA ~ prtystrA + democA + log(expendA) + log(expendB), data=data)
u.i <- reg$residuals
rega <- lm(u.i ~ data$prtystrA + data$democA + log(data$expendA) + log(data$expendB))

##2.b
u.i.sq <- (u.i)^2
regb <- lm(u.i.sq ~ data$prtystrA + data$democA + log(data$expendA) + log(data$expendB))
summary(regb)
names(regb)

##2.c
y.hat <- regb$fitted.values
y.hat.sq <- (regb$fitted.values)^2

regc <- lm(u.i.sq ~ y.hat + y.hat.sq)
summary(regc)


rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)

load("gpa1.RData")
names(data)


reg <- lm(colGPA ~ hsGPA + ACT + skipped + PC, data=data)
resid.sq <- (reg$residuals)^2
y.hat <- reg$fitted.values
y.hat.sq <- (reg$fitted.values)^2

white.test <- lm(resid.sq ~ y.hat+y.hat.sq)
summary(white.test)

h.hat <- white.test$fitted.values
inverse.h.hat <- 1/h.hat

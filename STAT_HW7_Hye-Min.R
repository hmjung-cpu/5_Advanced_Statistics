rm(list=ls())
setwd("~/Documents/coding")

install.packages("plm")
install.packages("wooldridge")
library(plm)
library(tidyverse)
library(wooldridge)



load("card.RData")
load("crime4.RData")
names(data)
summary(data)

##(a)Run a regular OLS of the crime rate on (prbarr), (prbconv), (prbpris), (avgsen), (polpc)
summary(lm(crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc, data=data))

##(b)first difference example
fd <- plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc, 
          index = c("county", "year"), model = "fd", data = data)
summary(fd)

##(c)fixed effects example
fe <- plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc, 
          index = c("county", "year"), model = "within",
          effect = "individual", data = data)
summary(fe)
# notice that d88 and d89 are year dummies, actually we are also using time fixed effects
# which is equivalent to
fe.twoways <- plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc, 
                  index = c("county", "year"), model = "within",
                  effect = "twoways", data = data)
summary(fe.twoways)

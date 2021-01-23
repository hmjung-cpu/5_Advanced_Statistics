rm(list=ls())
setwd("~/Documents/coding")

load("card.RData")
names(data)
View(data)

summary(lm(IQ ~ nearc4, data=data))

summary(lm(IQ ~ nearc4 + smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, data=data))

summary(lm(log(wage) ~ educ + exper + exper^2, data=data))
summary(lm(log(wage) ~ educ + exper + exper^2 + smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, data=data))

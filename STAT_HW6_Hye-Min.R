
rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)

load("kielmc.RData")
names(data)
summary(data)

reg.1 <- lm(lrprice[year==1981] ~ nearinc[year==1981], data=data)
summary(reg.1)

reg.2 <- lm(lrprice[year==1981] ~ nearinc[year==1981] + age[year==1981] + agesq[year==1981] 
            + rooms[year==1981] + baths[year==1981] + lintst[year==1981] + larea[year==1981] 
            + lland[year==1981], data=data)
summary(reg.2)

reg.3 <- lm(lrprice[year==1978] ~ nearinc[year==1978], data=data)
summary(reg.3)

reg.4 <- lm(lrprice ~ nearinc + y81 + I(nearinc*y81), data=data)
summary(reg.4)

reg.5 <- lm(lrprice ~ nearinc + y81 + I(nearinc*y81) + age + agesq + rooms
            + baths + lintst + larea + lland, data=data)
summary(reg.5)

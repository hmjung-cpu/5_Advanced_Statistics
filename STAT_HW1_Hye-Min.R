setwd("~/Documents/coding")
# Draw a sample of 1000 observations, 
# construct a variable treated that takes a value of 1 for the first 300 observations
# value of 0 for last 700 observations

# simulate two random variables X1 and X2
# both are uniformly distributed on[0,2]
# Be sure to seed for random number generator
set.seed(1000)
X.1 <- runif(1000, min = 0, max = 1)
X.2 <- runif(1000, min = 0, max = 1)

# let ui=X1i, let Ti=2*X2i+2
u.i = X.1
T.i = 2*X.2 + 2
# Then construct variables Yci=15+ui, Yti=15+Ti+ui
library(tidyverse)
simulation <- tibble(
  Y.t.i = 15 + T.i + u.i,
  Y.c.i = 15 + u.i,
  u.i = X.1, 
  T.i = 2*X.2 + 2
)
#summary(Y.t.i)
#boxplot(Y.c.i)

dummy1 <- data.frame(treated=rep(1, length(1:300)))
dummy0 <- data.frame(treated=rep(0, length(1:700)))
dummy <- rbind(dummy1,dummy0)


simulation <- cbind(simulation,dummy)

View(dummy)
# potential midterm score (out of 20 points)
# simulate: 30% treated - students assigned to attend TA session (dummy variable coded 1)
#         : 70% controlled - assigend NOT to attend TA session (dummy variable coded 0)

# variable: u meant to represent the error term in a regression(u~N(0,1))
#         : Ti treatment effect for individual i
#         : Yt,i potential outcomes for individual i under treated
#         : Yc,i potential outcomes for individual i under controlled

# We want to find out what would happen 
# if we could only observe one outcome for each person 
# based on their treatment status

# 4.a.Define new variable Yobs=midterm test score that would be observed for each student, 
# given their treatment status
# Yobs is the data that a researcher would normally observe instead of both potential outcomes

simulation<-simulation %>%
  mutate(Y.obs=ifelse(treated %in% 0, 15+u.i, 15+T.i+u.i))

# 4.b Use Yobs to compute the average treatment effect on midterm scores of attending the TA sessions by 
# comparing the final test scores for the treated and control groups
mean(simulation$Y.obs[1:300])-mean(simulation$Y.obs[301:1000]) #18.52331-15.49326=3.030051

# 4.c What is population mean of Ti? is you answer to part(b) exactly equal to this population mean? why or why not?
# Ti=2*X2i+2, E(Ti)=E`(2*X2i+2)
2*(1/2)+2 #3

# T.i (causal effect : the difference between potential outcomes for a given unit)
# population mean of T.i is not exactly equal to average treatment effect in 4.b.
# population mean of T.i contains difference between potential outcomes for all 1000 objects.
# however average treatment effect has outcomes for 300 treated, 
# and does not contain 700 controlled.

# 4.d. Calclate the mean of Ti in the sample(using all 1000 observations). Is the mean of Ti exactly the same as your estimate
# from part (b)? why or why not?
mean(T.i) #2.978224

3.030051-3; 3-2.978224

mean(simulation$u.i[1:300]);mean(simulation$u.i[301:1000])
mean(simulation$T.i[1:300]);mean(simulation$T.i[301:1000])
mean(u.i)

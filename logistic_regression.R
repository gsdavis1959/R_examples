mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
View(mydata)
attach(mydata)
names(mydata)
mylogit<- glm(admit~gre+gpa+as.factor(rank), family=binomial(link="logit"), na.action=na.pass)
summary(mylogit)
# confidence intervals for the coefficient estimates
confint(mylogit)
# wald test - to test the overall effect of rank
library(aod)
wald.test(b=coef(mylogit), Sigma=vcov(mylogit), Terms=4:6)

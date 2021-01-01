library(ISLR)
library(tidyverse)
library(Rfast)
library(MASS)
# create the wage_cat variable which takes two values
# such as Above if the wage is above median and Below if
# the wage is below median
Wage$wage_cat<-as.factor(ifelse(Wage$wage>median(Wage$wage),"Above","Below"))

# Examine the Wage vs Job Class
# you could use also the command xtabs(~jobclass+wage_cat, data=Wage)
con1<-table(Wage$jobclass,Wage$wage_cat)
con1

mosaicplot(con1)

# overall
prop.table(con1)
# by row
prop.table(con1, margin = 1)
# by column
prop.table(con1, margin = 2)

addmargins(con1)
addmargins(prop.table(con1))

chisq.test(con1)
fisher.test(con1)

loglm( ~ 1 + 2, data = con1)


con2<-table(Wage$education,Wage$wage_cat)
con2
mosaicplot(con2)
chisq.test(con2)
con3<-table(Wage$race,Wage$wage_cat)
con3
mosaicplot(con3)
chisq.test(con3)


con4<-xtabs(~jobclass+wage_cat+race, data=Wage) 
ftable(con4)

con4%>%ftable(row.vars=c("race", "jobclass"))

con4%>%ftable(row.vars=c("race", "jobclass"))%>%prop.table(margin = 1)%>%round(2)

#get the 4 odds ratio for race
for (i in 1:4) {
  
  print(odds.ratio(con4[,,i])$res[1])
}

mantelhaen.test(con4)

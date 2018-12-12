## DATA

dat = data.frame(
  name=c("Tony","James","Sara","Alice","David","Angie","Don","Faith","Becky","Jenny"),
  state=c("KS","IA","CA","FL","MI","CO","KA","CO","KS","CA"),
  gender=c("M","M","F","F","F","M","F","M","F","F"),
  marital_status=c("M","S","S","S","M","M","S","M","S","M"),
  credit=c("good","good","poor","fair","poor","fair","fair","fair","good","fair"),
  owns_home=c(0,1,0,0,1,0,1,1,1,1),
  cost=c(500,200,300,150,200,300,400,450,250,150))

dat  

## DDPLY FUNCTION IN THE PLYR PACKAGE 
## Use 'nrow' to find the count of a particular variable  
library(plyr)
ddply(dat, .(credit), "nrow")
ddply(dat, .(gender), "nrow")
ddply(dat, .(marital_status, credit), "nrow")
## use 'summarise' to summarize numeric variables
ddply(dat, .(gender), summarise, mean_cost = mean(cost))
ddply(dat, .(state), summarise, mean_cost = mean(cost))
ddply(dat, .(gender), summarise, min_cost = min(cost), 
      max_cost = max(cost), mean_cost = mean(cost))
ddply(dat, .(gender, credit), summarise, credit=length(credit),
      min_cost = min(cost), max_cost = max(cost), mean_cost = mean(cost))

## AGGREGATE FUNCTION FROM BASE R
aggregate(cost ~ marital_status + gender, data=dat, FUN=mean)
aggregate(cost ~ credit + gender, data=dat, FUN=mean)


library(Information)
library(gridExtra)

options(scipen=10)

### Loading the data
data(train, package="Information")
data(valid, package="Information")

### Exclude the control group
train <- subset(train, TREATMENT==1)
valid <- subset(valid, TREATMENT==1)

### Ranking variables using penalized IV  
IV <- create_infotables(data=train,
                   valid=valid,
                   y="PURCHASE")

grid.table(head(IV$Summary), rows=NULL)

grid.table(IV$Tables$N_OPEN_REV_ACTS, rows=NULL)



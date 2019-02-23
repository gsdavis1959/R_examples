require(RCurl)

setwd("~/Data/Datasets/dataset_diabetes")

# open diabetes file

diabetes <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE)
str(diabetes)

length(unique(diabetes$diag_1))
## [1] 717
length(unique(diabetes$diag_2))
## [1] 749
length(unique(diabetes$diag_3))
## [1] 790

# drop useless variables
diabetes <- subset(diabetes,select=-c(encounter_id, patient_nbr))
# transform all "?" to 0s
diabetes[diabetes == "?"] <- NA

# remove zero variance - ty James http://stackoverflow.com/questions/8805298/quickly-remove-zero-variance-variables-from-a-data-frame
diabetes <- diabetes[sapply(diabetes, function(x) length(levels(factor(x,exclude=NULL)))>1)]

# prep outcome variable to those readmitted under 30 days
diabetes$readmitted <- ifelse(diabetes$readmitted == "<30",1,0)

outcomeName <- 'readmitted'

diabetes_dummy <- diabetes
# warning will need 2GB at least free memory
require(caret)
dmy <- dummyVars(" ~ .", data = diabetes_dummy)
diabetes_dummy <- data.frame(predict(dmy, newdata = diabetes_dummy))

dim(diabetes_dummy)

# change all NAs to 0
diabetes_dummy[is.na(diabetes_dummy)] <- 0
# split the data into training and testing data sets
set.seed(1234)
split <- sample(nrow(diabetes_dummy), floor(0.5*nrow(diabetes_dummy)))
objTrain <-diabetes_dummy[split,]
objTest <- diabetes_dummy[-split,]

predictorNames <- setdiff(names(diabetes_dummy),outcomeName)

# cv.glmnet expects a matrix 
library(glmnet)
# straight matrix model not recommended - works but very slow, go with a sparse matrix
# glmnetModel <- cv.glmnet(model.matrix(~., data=objTrain[,predictorNames]), objTrain[,outcomeName], 
#             family = "binomial", type.measure = "auc")

glmnetModel <- cv.glmnet(sparse.model.matrix(~., data=objTrain[,predictorNames]), objTrain[,outcomeName], 
                         family = "binomial", type.measure = "auc")
glmnetPredict <- predict(glmnetModel,sparse.model.matrix(~., data=objTest[,predictorNames]), s="lambda.min")

auc(objTest[,outcomeName], glmnetPredict)

# hash features

diabetes_hash <- diabetes
predictorNames <- setdiff(names(diabetes_hash),outcomeName)

# change all NAs to 0
diabetes_hash[is.na(diabetes_hash)] <- 0

set.seed(1234)
split <- sample(nrow(diabetes_hash), floor(0.5*nrow(diabetes_hash)))
objTrain <-diabetes_hash[split,]
objTest <- diabetes_hash[-split,]

library(FeatureHashing)
objTrain_hashed = hashed.model.matrix(~., data=objTrain[,predictorNames], hash.size=2^12, transpose=FALSE)
objTrain_hashed = as(objTrain_hashed, "dgCMatrix")
objTest_hashed = hashed.model.matrix(~., data=objTest[,predictorNames], hash.size=2^12, transpose=FALSE)
objTest_hashed = as(objTest_hashed, "dgCMatrix")


library(glmnet)
glmnetModel <- cv.glmnet(objTrain_hashed, objTrain[,outcomeName], 
                         family = "binomial", type.measure = "auc")

glmnetPredict <- predict(glmnetModel, objTest_hashed, s="lambda.min")
auc(objTest[,outcomeName], glmnetPredict)

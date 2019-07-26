

#load train and test file
train <- read.csv("train_Big.txt")
test <- read.csv("test_Big.txt")

#add a column
test$Item_Outlet_Sales <- 1

#combine the data set
combi <- rbind(train, test)

#impute missing values with median
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute 0 with median
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),                                   combi$Item_Visibility)

#find mode and impute
table(combi$Outlet_Size, combi$Outlet_Type)
levels(combi$Outlet_Size)[1] <- "Other"
#remove the dependent and identifier variables
my_data <- subset(combi, select = -c(Item_Outlet_Sales, Item_Identifier))
#check available variables
colnames(my_data)
#check variable class
str(my_data)                  
#load library
library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                                                     "Outlet_Establishment_Year","Outlet_Size",
                                                     "Outlet_Location_Type","Outlet_Type"))
#check the data set
str(new_my_data)
#divide the new data
pca.train <- new_my_data[1:nrow(train),]
pca.train <- subset(pca.train, select = -c(Outlet_Identifier))
pca.test <- new_my_data[-(1:nrow(train)),]
pca.test <- subset(pca.test, select = -c(Outlet_Identifier))
str(pca.test) 
head(pca.test)

#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center
summary(prin_comp)
#outputs the standard deviation of variables
prin_comp$scale
prin_comp$rotation
prin_comp$rotation[1:5,1:4]

biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#add a training set with principal components
train.data <- data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)

#we are interested in first 30 PCAs
train.data <- train.data[,1:31]

#run a decision tree

library(rpart)
rpart.model <- rpart(Item_Outlet_Sales ~ .,data = train.data, method = "anova")
rpart.model
lm.model <- glm(Item_Outlet_Sales ~ .,data = train.data)
summary(lm.model)

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- as.data.frame(test.data[,1:30])

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)
summary(rpart.prediction)

#For fun, finally check your score of leaderboard
sample <- read.csv("SampleSubmission_TmnO39y.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
write.csv(final.sub, "pca.csv",row.names = F)

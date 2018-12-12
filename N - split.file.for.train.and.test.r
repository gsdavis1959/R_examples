setwd("~/Data/Datasets")

file <- read.csv("top-tv-earners.csv", header = T,
                 stringsAsFactors = F)

#spliting the dataset in training and testing 
splitdf <- function(file, seed=NULL) { 
  if (!is.null(seed)) set.seed(seed) 
  index <- 1:nrow(file) 
  trainindex <- sample(index, trunc(length(index)/5)) 
  trainset <- file[-trainindex, ] 
  testset <- file[trainindex, ] 
  list(trainset=trainset,testset=testset) 
} 


#apply the function 
splits <- splitdf(file, seed=808) 


# save the training and testing sets as data frames 
training <- splits$trainset 
testing <- splits$testset 
View(file)
View(training)
View(testing)



#Model based on the training dataset 

Model_fit <- glm(Actor~Pay, family=binomial(link="logit"), data=training, na.action=na.pass)

plot(testing$pclass~testing$fare)
correlation <- cor(testing$pclass, testing$fare)
correlation
#Prediction on test dataset 
predicted<-as.data.frame(predict(Model_fit, newdata=testing[,testing$pclass])) 
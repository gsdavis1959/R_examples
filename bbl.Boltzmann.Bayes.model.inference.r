titanic <- as.data.frame(Titanic)

freq <- titanic$Freq
titanic <- titanic[,1:4]

library(bbl)
titanic_raw <- freq2raw(data=titanic, freq=freq)
head(titanic_raw)

summary(titanic_raw)

# logistic regression model
gfit <- glm(Survived ~ Class + Sex + Age, family=binomial(), data=titanic, weights=freq)
gfit
summary(gfit)
# include interactions
gfit1 <- glm(Survived ~ (Class + Sex + Age)^2, family=binomial(), data=titanic, weights=freq)
summary(gfit1)

set.seed(159)
nsample <- NROW(titanic_raw)

flag <- rep(TRUE, nsample)
flag[sample(nsample, nsample/2)] <- FALSE
dtrain <- titanic_raw[flag,]
dtest <- titanic_raw[!flag,]

gfit2 <- glm(Survived ~ Class*Sex + Sex*Age, family=binomial(), data=dtrain)
prl <- predict(gfit2, newdata=dtest)
yhat <- ifelse(prl>0, 'Yes','No')
mean(yhat==dtest$Survived)
# area under the curve
gauc <- pROC::roc(response=dtest$Survived, predictor=prl, direction='<')$auc
gauc

bfit0 <- bbl(Survived ~ Class + Sex + Age, data=titanic, freq=freq)
bfit0
summary(bfit0)

# with interactions
bfit <- bbl(Survived ~ Class*Sex + Sex*Age, data=titanic, freq=freq)
bfit
summary(bfit)
plot(bfit)

bfit2 <- bbl(Survived ~ Class*Sex + Sex*Age, data=dtrain)
pr <- predict(bfit2, newdata=dtest, logit=FALSE)
head(pr)
pROC::roc(response=dtest$Survived, predictor=pr[,2], direction='<')$auc

cv <- crossVal(Survived ~ Class*Sex + Sex*Age, data=dtrain, method='pseudo', lambda=10^seq(-5,-2,0.2), verbose=0)
cv
plot(cv, mar=c(4,4,3,3), tck=-0.04, las=1, ylab='AUC', bty='n')

model <- bbl(Survived ~ Class*Sex + Sex*Age, data=dtrain, lambda=cv$regstar)
pr2 <- predict(model, newdata=dtest)
bscore <- mean(dtest$Survived==pr2$yhat)
bscore

bauc <- pROC::roc(response=dtest$Survived, predictor=pr2[,2], direction='<')$auc
bauc

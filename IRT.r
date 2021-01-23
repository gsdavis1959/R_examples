library(ltm)
library(psych)
irtdat<-read.table("C:/Users/gsdav/Documents/Data/RStatistics/WindowsRSource/IRT tutorial/ouirt.dat",header=F)
head(irtdat)
describe(irtdat)

# We will use (simulated) results from N=500 individual taking a 10-item test (V1-V10). 
# Items are coded `1` for correct and `0` for incorrect responses.

# estimate the difficulty of each item
PL1.rasch<-rasch(irtdat)
summary(PL1.rasch)

##Step 2: Plot the item characteristic curves of all 10 items
plot(PL1.rasch,type=c("ICC"))

##Step 3: Plot the item information curves for all 10 items, then the whole test
plot(PL1.rasch,type=c("IIC"))
# item 10 provides the most information about high ability levels
# item 5 provides the most information about lower ability levels

# plot the information curve for the whole test
plot(PL1.rasch,type=c("IIC"),items=c(0))
# test provides the most information about slightly-higher-than-average ability levels

##Step 4: Test the fit of the 1PL model
item.fit(PL1.rasch,simulate.p.value=T)
# We see from this that items 8, 9, and 10 perhaps do not fit the 1PL model so well (small p-values).

##Step 5: Estimate ability scores & plot
theta.rasch<-ltm::factor.scores(PL1.rasch)
summary(theta.rasch$score.dat$z1)
plot(theta.rasch)

##Step 6: Test for unidimensionality

unidimTest(PL1.rasch,irtdat)

##Step 1: Fit the 2PL model
PL2.rasch<-ltm(irtdat~z1)
summary(PL2.rasch)

# Higher difficulty values indicate that the item is harder (i.e., higher latent ability to answer correctly); 
# higher discriminability estimates indicate that the item has better ability to tell the difference between different levels of latent ability.
plot(PL2.rasch,type=c("ICC"))

##Step 3: Plot the item information curves for all 10 items, then the whole test
plot(PL2.rasch,type=c("IIC"))

# item information curve for the whole test. This is the sum of all the item IICs above.
plot(PL2.rasch,type=c("IIC"),items=c(0))

##Step 4: Test the fit of the 2PL model
theta.rasch <-ltm::factor.scores(PL2.rasch)
summary(theta.rasch$score.dat$z1)
plot(theta.rasch)

##Step 6: Test for unidimensionality
unidimTest(PL2.rasch,irtdat)

##Step 1: Fit the 3PL model includes guessing
PL3.rasch<-tpm(irtdat)
summary(PL3.rasch)

# Items 2 and 4 have significant guessing parameters (z>1.65)
# probabilities of getting the item correcty by guessing are pretty low
##Step 2: Plot the item characteristic curves of all 10 items
plot(PL3.rasch,type=c("ICC"))

##Step 3: Plot the item information curves for all 10 items, then the whole test
plot(PL3.rasch,type=c("IIC"))

# plot the IIC for the entire test, the sum of the item IICS:
plot(PL3.rasch,type=c("IIC"),items=c(0))

##Step 4: Estimate ability scores & plot
theta.rasch<-ltm::factor.scores(PL3.rasch)
summary(theta.rasch$score.dat$z1)
plot(theta.rasch)

##Step 5: Test for unidimensionality
unidimTest(PL3.rasch,irtdat)

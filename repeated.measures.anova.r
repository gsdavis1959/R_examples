#  R for repeated measures
#  One within subject measure and no between subject measures
#  Be sure to run this from the beginning because 
#  otherwise vectors become longer and longer.
library(car)
library(multcompView)
rm(list = ls())
##  You want to clear out old variables --with "rm(list = ls())" --
##  before building new ones.
data <- read.csv("~/Data/Datasets/Psychology/NolenHoeksema.csv", header = TRUE)
datLong <- reshape(data = data, varying = 2:6, v.names = "outcome", timevar
                   = "time", idvar = "Subject", ids = 1:9, direction = "long")
datLong$time <- factor(datLong$time)
datLong$Subject <- factor(datLong$Subject)
orderedTime <- datLong[order(datLong$time),]
options(contrasts=c("contr.sum","contr.poly"))
# Using "data = dataLong" I can use the simple names for the variables
modelAOV <- aov(outcome~factor(time)+Error(factor(Subject)), data = datLong)
print(summary(modelAOV))
obtF <- summary(modelAOV)$"Error: Within"[[1]][[4]][1]

par( mfrow = c(2,2))
plot(datLong$time, datLong$outcome, pch = c(2,4,6), col = c(3,4,6))
legend(1, 20, c("same", "different", "control"), col = c(4,6,3),
       text.col = "green4",  pch = c(4, 6, 2),
       bg = 'gray90')


model=lm(outcome ~ factor(time), data=datLong)
ANOVA=aov(model)

TUKEY <- TukeyHSD(ANOVA)
TUKEY


# This code was originally written by Joshua Wiley, in the Psychology Department at UCLA.
#  Modified for one between and one within for King.dat by dch

### Howell Table 14.4 ###
## Repeated Measures ANOVA with 2 variables
## Read in data, convert to 'long' format, and factor()
dat <- read.csv("~/Data/Datasets/Psychology/Tab14-4.csv", header = TRUE)
head(dat)

dat$subject <- factor(1:24)
datLong <- reshape(data = dat, varying = 2:7, v.names = "outcome", timevar
                   = "time", idvar = "subject", ids = 1:24, direction = "long")
datLong$Interval <- factor(rep(x = 1:6, each = 24), levels = 1:6, labels = 1:6)
datLong$Group <- factor(datLong$Group, levels = 1:3, labels = c("Control",
                                                                "Same", "Different"))
cat("Group Means","\n")
cat(tapply(datLong$outcome, datLong$Group, mean),"\n")
cat("\nInterval Means","\n")
cat(tapply(datLong$outcome, datLong$Interval, mean),"\n")
# Actual formula and calculation
King.aov <- aov(outcome ~ (Group*Interval) + Error(subject/(Interval)), data = datLong)
# Present the summary table (ANOVA source table)
print(summary(King.aov))
interaction.plot(datLong$Interval, factor(datLong$Group),
                 datLong$outcome, fun = mean, type="b", pch = c(2,4,6),
                 legend = "F", 
                 col = c(3,4,6), ylab = "Mean of Outcome", 
                 legend(4, 300, c("Same", "Different", "Control"), col = c(4,6,3),
                        text.col = "green4", lty = c(2, 1, 3), pch = c(4, 6, 2),
                        merge = TRUE, bg = 'gray90'))

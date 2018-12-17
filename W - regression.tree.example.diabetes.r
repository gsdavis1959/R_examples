library(party) # For ctree.plot
library(partykit) # For conversion
library(rpart)

setwd("~/Data/Datasets")
file <- read.csv('diabetes.csv',
                  stringsAsFactors = FALSE)

View(file)
fit.r <- rpart(class ~ no_preg + plasma + BP + skin + insu + BMI + pedi + age, data=file)

plot(fit.r) # Draw rtree plot
text(fit.r, use.n=TRUE, cex=.75) # Add text

# Next we will convert the display to ctree style.
# Please note that this only changes the way it is stored, not the actual tree


plot(as.party(fit.r))

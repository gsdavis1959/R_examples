rm(list = ls(all = TRUE))
library(quantmod)
require(fpp)
library(tseries)
library(lubridate)
library(ggplot2)
library(reshape2)
library(corrplot)




getSymbols("SP500", src="FRED")
chartSeries(SP500, theme="white")
# addBBands()
addSMA(200)
addSMA(65)
addSMA(35)

sp500 <- na.omit(SP500)
head(sp500)
sp500l <- Lag(sp500)

sma.100 <-   SMA(sp500[,"SP500"], 100)
sma.200 <-  SMA(sp500[,"SP500"], 200)
sma.65 <-   SMA(sp500[,"SP500"], 65)
sma.5 <- SMA(sp500[, "SP500"], 5)
sma.2 <- SMA(sp500[, "SP500"], 2)

combined <- na.omit(merge(sp500, sma.2, sma.5, sma.65, sma.200, sma.100, sp500l))

combined <- as.data.frame(combined)
View(combined)
combined$distance=combined$SMA.1 - combined$SP500
combined$avgdist= combined$SMA.4 - combined$SMA.2
combined$change=combined$SP500-combined$SMA
correlation <- as.matrix(cor(combined))
corrplot(correlation)
print(correlation)
model <- lm(change~distance, data=combined)
summary(model)
plot(model)

View(sma.65)
View(sma.200)
View(combined)
qplot(change, distance, data=combined)
qplot(avgdist, change, data=combined)
qplot(change, data=combined)
qplot(date, Lag.1, aes("line"), data=combined)
combined.lc <- subset(combined, change > 100)
summary(combined)

combined.2 <- combined
combined.2$change[combined.2$change < 0] <- 0
combined.2$change[combined.2$change > 0] <- 1
View(combined.2)
qplot(distance, change, data=combined.2)
qplot(avgdist, change, data=combined.2)
qplot(change, data=combined.2)
setwd("~/Data/RStatistics/Datasets")
write.table(combined.2, "SP500.csv", sep=",", row.names=F)

# graph in ggplot

combined <- as.data.frame(combined)
combined$date=as.Date(rownames(combined))
combined <- subset(combined, date > '2010-01-01')

m=melt(combined,id="date")
m$variable=gsub('\\.',' ',m$variable)
View(m)
ggplot(m,aes(date,0,colour=variable)) + geom_line(aes(x=date,y=value))+ ylab("value")

# forecast

f.sma.65 <- ts(sma.65)
head(f.sma.65)

plot(forecast(sp500))
plot(forecast(sma.65))
plot(forecast(sma.200))


fcast1 <- forecast(sma.65, h=200)
fcast2 <- forecast(sma.200, h=200)
fcast3 <- forecast(sp500, h=200)

plot(fcast1)
plot(fcast2)
plot(fcast3)

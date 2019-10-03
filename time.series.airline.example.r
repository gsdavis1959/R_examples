# loading packages
library(forecast)
library(Metrics)
setwd('~/Data/Datasets/Business')
# reading data
data = read.csv("example_air_passengers.csv")

# splitting data into train and valid sets
train = data[1:100,]
valid = data[101:nrow(data),]

# removing "Month" column
train$ds = NULL

# training model
model = auto.arima(train)

# model summary
summary(model)

# forecasting
forecast = predict(model,44)


# evaluation
rmse(valid$y, forecast$pred)

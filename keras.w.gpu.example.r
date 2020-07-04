library(tidyverse)
library(keras)
install_keras()
install_keras(tensorflow = "gpu")

# check
to_categorical(0:3)

rm(list=ls())
data(iris)
plot(iris$Petal.Length,
     iris$Petal.Width, col = iris$Species)

onehot.species = to_categorical(as.numeric(iris$Species) - 1)
iris = as.matrix(iris[, 1:4])
iris = cbind(iris, onehot.species)
iris

set.seed(17)
ind <- sample(2, nrow(iris),
              replace = TRUE, prob = c(0.7, 0.3))
iris.training <- iris[ind == 1, 1:4]
iris.test <- iris[ind == 2, 1:4]
iris.trainingtarget <- iris[ind == 1, -seq(4)]
iris.testtarget <- iris[ind == 2, -(1:4)]


model <- keras_model_sequential()

model %>%
  layer_dense(units = ncol(iris.trainingtarget), activation = 'softmax',
              input_shape = ncol(iris.training))
summary(model)

model$inputs
model$outputs

library(igraph)
g = graph_from_literal(Sepal.Length:Sepal.Width:Petal.Length:Petal.Width---Species,simplify = TRUE)
layout <- layout_in_circle(g, order = order(degree(g)))
plot(g,layout = layout,vertex.color = c(2,2,2,2,3))

# blue are the inputs, green is the output

# learning rate
sgd <- optimizer_sgd(lr = 0.01)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = sgd,
  metrics = 'accuracy'
)

history <- model %>% fit(
  x = iris.training,
  y = iris.trainingtarget,
  epochs = 100,
  batch_size = 5,
  validation_split = 0.2,
  verbose = 0
)

plot(history)

classes <- model %>% predict_classes(iris.test)
table(iris.testtarget%*%0:2, classes)

(score <- model %>% evaluate(iris.test, iris.testtarget))

# breastcancer example
library(mlbench)
data(BreastCancer)
dim(BreastCancer)

levels(BreastCancer$Class)
head(BreastCancer)
str(BreastCancer)

# put date into martix
tt = BreastCancer[complete.cases(BreastCancer),2:11]
x = NULL
for(i in seq(9)) x = cbind(x,to_categorical(as.numeric(tt[,i])-1))
y = to_categorical(as.numeric(tt[,10])-1)
head(y)

# training and test
set.seed(17)
ind <- sample(2, nrow(x),
              replace = TRUE, prob = c(0.7, 0.3))

x.train = x[ind == 1,]
y.train = y[ind == 1,]
x.test = x[ind == 2,]
y.test = y[ind == 2,]

# Initialize a sequential model
model <- keras_model_sequential()
# Add layers to model
model %>%
  layer_dense(units = 8, activation = 'relu', input_shape = ncol(x.train)) %>%
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dense(units = ncol(y.train), activation = 'softmax')

print(ncol(x.train))
print(ncol(y.train))

summary(model)

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = "adam",
  metrics = 'accuracy'
)

history <- model %>% fit(
  x = x.train,
  y = y.train,
  epochs = 50,
  batch_size = 50,
  validation_split = 0.2,
  verbose = 2
)
plot(history)

classes <- model %>% predict_classes(x.test)
table(y.test%*%0:1, classes)

(score <- model %>% evaluate(x.test, y.test))

# LSTM

library(BatchGetSymbols)
tickers <- c('^GSPC')
first.date <- Sys.Date() - 360*15
last.date <- Sys.Date()

myts <- BatchGetSymbols(tickers = tickers,
                        first.date = first.date,
                        last.date = last.date,
                        cache.folder = file.path(tempdir(),
                                                 'BGS_Cache') ) # cache in tempdir()
print(myts$df.control)


y = myts$df.tickers$price.close
myts = data.frame(index = myts$df.tickers$ref.date, price = y, vol = myts$df.tickers$volume)
myts = myts[complete.cases(myts), ]
myts = myts[-seq(nrow(myts) - 3000), ]
myts$index = seq(nrow(myts))

library(plotly)

plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol)

acf(myts$price, lag.max = 3000)

msd.price = c(mean(myts$price), sd(myts$price))
msd.vol = c(mean(myts$vol), sd(myts$vol))
myts$price = (myts$price - msd.price[1])/msd.price[2]
myts$vol = (myts$vol - msd.vol[1])/msd.vol[2]
summary(myts)

datalags = 10
train = myts[seq(2000 + datalags), ]
test = myts[2000 + datalags + seq(1000 + datalags), ]
batch.size = 50

x.train = array(data = lag(cbind(train$price, train$vol), datalags)[-(1:datalags), ], dim = c(nrow(train) - datalags, datalags, 2))
y.train = array(data = train$price[-(1:datalags)], dim = c(nrow(train)-datalags, 1))

x.test = array(data = lag(cbind(test$vol, test$price), datalags)[-(1:datalags), ], dim = c(nrow(test) - datalags, datalags, 2))
y.test = array(data = test$price[-(1:datalags)], dim = c(nrow(test) - datalags, 1))

model <- keras_model_sequential()

model %>%
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mae', optimizer = 'adam')

model

for(i in 1:2000){
  model %>% fit(x = x.train,
                y = y.train,
                batch_size = batch.size,
                epochs = 1,
                verbose = 2,
                shuffle = FALSE)
  model %>% reset_states()
}

pred_out <- model %>% predict(x.test, batch_size = batch.size) %>% .[,1]

plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol) %>%
  add_trace(y = c(rep(NA, 2000), pred_out), x = myts$index, name = "LSTM prediction", mode = "lines")
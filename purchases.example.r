library(tidyverse)
library(methods)
library(recommenderlab)
library(data.table)
library(ggplot2)
library(knitr)

df_data <- fread('small_invoices.csv')
head(df_data)
df_data[ ,InvoiceDate := as.Date(InvoiceDate)]
head(df_data)

df_data[Quantity<=0,Quantity:=NA]
df_data[UnitPrice<=0,UnitPrice:=NA]
df_data <- na.omit(df_data)

setkeyv(df_data, c('StockCode', 'Description'))
itemCode <- unique(df_data[, c('StockCode', 'Description')])
setkeyv(df_data, NULL)
df_data
itemCode

df_train_ori <- dcast(df_data, CustomerID ~ StockCode, value.var = 'Quantity',fun.aggregate = sum, fill=0)

CustomerId <- df_train_ori[,1] #!

df_train_ori <- df_train_ori[,-c(1,3504:3508)]

#Fill NA with 0
for (i in names(df_train_ori))
  df_train_ori[is.na(get(i)), (i):=0]

df_train_ori

df_train <- as.matrix(df_train_ori)
df_train <- df_train[rowSums(df_train) > 5,colSums(df_train) > 5] 
df_train <- binarize(as(df_train, "realRatingMatrix"), minRatin = 1)

df_train

# split dataset
which_train <- sample(x = c(TRUE, FALSE), size = nrow(df_train),replace = TRUE, prob = c(0.8, 0.2))
y <- df_train[!which_train]
x <- df_train[which_train]

method <- 'IBCF'
parameter <- list(method = 'Jaccard')
n_recommended <- 5
n_training <- 1000

recc_model <- Recommender(data = x, method = method, parameter = parameter)
model_details <- getModel(recc_model)

recc_predicted <-predict(object = recc_model, newdata=y,n = n_recommended, type="topNList")
as(recc_predicted,"list")[1:5]

user_1 <- CustomerId[as.integer(names(recc_predicted@items[1]))]
user_1
vvv <- recc_predicted@items[[1]]
vvv <- rownames(model_details$sim)[vvv]
itemCode[vvv]

# compare to actual purchases
user_1_buy <- df_data[CustomerID=='12356', sum(Quantity), by=StockCode]
merge(itemCode,user_1_buy, by='StockCode')

library(magrittr)
library(dplyr)
library(caret)
library(tibble)
library(ggplot2)
library(forcats)
library(NHSRdatasets)

#Load in stranded dataset from NHSRDatasets
strand <- NHSRdatasets::stranded_data %>%
na.omit() %>%
  select(-c('frailty_index', 'admit_date')) %>%
  mutate(stranded_class = make.names(as.factor(stranded.label))) %>%
  select(-stranded.label)

dataset <- as.data.frame(strand)

train_split_idx <- caret::createDataPartition(dataset$stranded_class, p = 0.75, list = FALSE)
data_TRAIN <- dataset[train_split_idx, ]
data_TEST <- dataset[-train_split_idx, ]
dim(data_TRAIN)
dim(data_TEST)
# Set the model metrics to accuracy and train a random forest model
eval_metric <- "Accuracy"
set.seed(123) # Random seed to make the results reproducible
rf_mod <- caret::train(stranded_class ~ .,
                       data = data_TRAIN,
                       method = "rf",
                       metric = eval_metric)

# install.packages("remotes") # if not already installed
remotes::install_github("https://github.com/StatsGary/ConfusionTableR")
library(ConfusionTableR)

# Use the function

ConfusionTableR::var_impeR(rf_mod)

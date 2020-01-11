library(keras)
library(tidyverse)

df <- read.csv('player_game_history_for_training.csv')
head(df)

# Create train test split
sample_rows <- sample(nrow(df), nrow(df) * 0.8)
# Create the training dataset
train <- df[sample_rows, ]
head(train)
# Create the test dataset
test <- df[-sample_rows, ]



train_labels <- train$accuracy_group
head(train_labels)
str(train_labels)
dim(train_labels)
train_data <- train %>%
  select(-accuracy_group, -X)
head(train_data)
str(train_data)
dim(train_data)
train_labels <- as.matrix(train_labels)
train_data <- as.matrix(train_data)


test_labels <- test$accuracy_group
test_data <- test %>%
  select(-accuracy_group, -X)
test_labels <- as.matrix(test_labels)
test_data <- as.matrix(test_data)

train_labels=to_categorical(train_labels, num_classes = 4)
test_labels=to_categorical(test_labels, num_classes = 4)



build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = ncol(train_data)) %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}



model <- build_model()
model %>% summary()

model %>% save_model_hdf5("player_history_model.h5")
# new_model <- load_model_hdf5("layer_history_model.h5")

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 600

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history)


c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae))


test_predictions <- model %>% predict(test_data)

r <- lm(accuracy_group ~ ., data = train)
p <- predict(r, newdata = test)


p_acc <- round(test_predictions)
c <- cut(test_predictions,breaks=4, labels=c("0", "1", "2", "3"))
c
comb <- cbind(test_labels, c)
colnames(comb) <- c('actual', 'predicted')
conf_mat <- table(test_labels, p_acc)
conf_mat
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
accuracy

c2 <- cut(p,breaks=4, labels=c("0", "1", "2", "3"))
c
comb <- cbind(test$accuracy_group, c2)
colnames(comb) <- c('actual', 'predicted')
conf_mat <- table(test$accuracy_group, c2)
conf_mat
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
accuracy



comb <- as.data.frame(comb)
comb

ggplot(comb, aes(actual, predicted)) +
  geom_point() +
  labs(x = "Actual", y = "Predicted", 
       title = "Keras Model") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5))

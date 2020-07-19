library(data.table)
time <- rep(1:10, 10)
replicate <- sort(time)
value <- rnorm(100)
df <- data.frame(replicate, time, value)
head(df)
dt <- data.table(df)
dt[, mean(value), by="replicate"]

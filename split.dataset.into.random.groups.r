N <- 3
df <- mtcars
summary(df)
f <- split(df, sample(1:N, nrow(df), replace=T))

grp1 <- f[[1]]
grp2 <- f[[2]]
grp3 <- f[[3]]

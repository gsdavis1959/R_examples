set.seed(23)
data <- data.frame(Group = rep(c("control", "Test1", "Test2"), each = 10),
                   value = c(rnorm(10), rnorm(10),rnorm(10)))
data$Group<-as.factor(data$Group)
data

boxplot(value~ Group,
        data = data,
        main = "Product Values",
        xlab = "Groups",
        ylab = "Value",
        col = "red",
        border = "black")

model <- aov(value ~ Group, data = data)
summary(model)

library(DescTools)
DunnettTest(x=data$value, g=data$Group)

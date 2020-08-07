library(tidyverse)

car_pref <- read.csv("https://raw.githubusercontent.com/PandulaP/personal-projects/master/Using%20PCA%20to%20make%20predictions/carpreference.r", sep="")
attach(car_pref)

head(car_pref)

str(car_pref)

library(reshape2)
car_pref.m <- melt(car_pref)
head(car_pref.m)

library(ggplot2)

plot1 <- ggplot(car_pref.m, aes(x = variable, y = value, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("plum", "gold2","ivory4")) +
  ggtitle("Characteristics by Price-Groups") +
  xlab("Characteristic") + ylab("Value")


print(plot1)

library(scatterplot3d)

colors <- c("blue", "red", "dark green")
colors = colors[as.numeric(Group)]
sample_3d_plot <- with(car_pref, scatterplot3d(Environment,Innovation,Prestige, color = colors, pch = 19, box = FALSE))
legend(sample_3d_plot$xyz.convert(4.5, 5.05, 14), legend = levels(car_pref$Group), col =  c("blue", "red", "dark green"), pch = 16)

library(corrplot)
cor_vals <- round(cor(car_pref[,2:5]),4)

corrplot(cor_vals, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

library(dplyr)
car_pref_reduced <- car_pref %>% select(-Group)

pca_model <- prcomp(car_pref_reduced, scale. = TRUE, center = TRUE)

summary(pca_model)

summary_pca_model = summary(pca_model)

plot(pca_model,type = "l", main ="Scree plot for PCA")

plot(summary_pca_model$importance[3,],type="l")

#For PC 1
loading_Scores_PC_1 <- pca_model$rotation[,1]
fac_scores_PC_1 <- abs(loading_Scores_PC_1)
fac_scores_PC_1_ranked <- names(sort(fac_scores_PC_1,decreasing = T))

#For PC 2
loading_Scores_PC_2 <- pca_model$rotation[,2]
fac_scores_PC_2 <- abs(loading_Scores_PC_2)
fac_scores_PC_2_ranked <- names(sort(fac_scores_PC_2,decreasing = T))

print("for PC 1")

pca_model$rotation[fac_scores_PC_1_ranked,1]
pca_model$rotation[fac_scores_PC_2_ranked,2]



scores <- data.frame(car_pref, pca_model$x[,1:2])

#PC_1n2 <- qplot(x=PC1, y=PC2, data=scores, colour=factor(car_pref$Group)) + theme(legend.position="none")
#print(PC_1n2)

plot_2 <-ggplot(scores,aes(x=PC1,y=PC2,color=Group )) + geom_point(size =2) + labs(title="Plotting Customer Data against PC1 and PC2")

print(plot_2)

new_customer <- c(9,8,5,4)

car_pref_reduced_w_nc <- rbind(car_pref_reduced,new_customer)

new_cus_group <- predict(pca_model, newdata = car_pref_reduced_w_nc[nrow(car_pref_reduced_w_nc),])
new_cus_group_PC1_PC2 <- new_cus_group[, 1:2]



plot_3 <- plot_2 + geom_point(aes(x=new_cus_group_PC1_PC2[1], y=new_cus_group_PC1_PC2[2]), colour="blue", size =4)

plot_3 <- plot_3 + labs(title="Plotting new observation against PC1 and PC2")

print(plot_3)

#http://jeromyanglim.blogspot.com.au/2012/05/how-to-plot-three-categorical-variables.html
library(ggplot2)

# Create some data
set.seed(4444)
Data <- expand.grid(group=c("Apples", "Bananas", "Carrots", "Durians", "Eggplants"),
                    year=c("2000", "2001", "2002"),
                    quality=c("Grade A", "Grade B", "Grade C", "Grade D", "Grade E"))
Group.Weight <- data.frame(
  group=c("Apples", "Bananas", "Carrots", "Durians", "Eggplants"),
  group.weight=c(1,1,-1,0.5, 0))
Quality.Weight <- data.frame(
  quality=c("Grade A", "Grade B", "Grade C", "Grade D", "Grade E"),
  quality.weight = c(1,0.5,0,-0.5,-1))
Data <- merge(Data, Group.Weight)
Data <- merge(Data, Quality.Weight)
Data$score <- Data$group.weight + Data$quality.weight + rnorm(nrow(Data), 0, 0.2)
Data$proportion.tasty <- exp(Data$score)/(1 + exp(Data$score))


# Plot Data with ggplot2
ggplot(data=Data, 
       aes(x=factor(year), y=proportion.tasty, 
           group=group,
           shape=group,
           color=group)) + 
             geom_line() + 
             geom_point() 
             title("Proportion Tasty by Year, Quality, and Group") +
             scale_x_discrete("Year") +
             scale_y_continuous("Proportion Tasty") + facet_grid(.~quality )

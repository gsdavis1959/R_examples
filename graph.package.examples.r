data(airquality)
head(airquality)
summary(airquality)
library(graphics)

plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)
plot(airquality)
# points and lines 
plot(airquality$Ozone, type= "b")
# high density vertical lines.
plot(airquality$Ozone, type= "h")
plot(airquality$Ozone, xlab = 'ozone Concentration', ylab = 'No of Instances', main = 'Ozone levels in NY city', col = 'green')
# Horizontal bar plot
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',xlab = 'ozone levels', col= 'green',horiz = TRUE)
# Vertical bar plot
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',xlab = 'ozone levels', col='red',horiz = FALSE)
hist(airquality$Solar.R)
hist(airquality$Solar.R, main = 'Solar Radiation values in air',xlab = 'Solar rad.', col='red')
#Single box plot
boxplot(airquality$Solar.R)
# Multiple box plots
boxplot(airquality[,0:4], main='Multiple Box plots')
par(mfrow=c(3,3), mar=c(2,5,2,1), las=1, bty="n")
plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)
plot(airquality$Ozone, type= "c")
plot(airquality$Ozone, type= "s")
plot(airquality$Ozone, type= "h")
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',xlab = 'ozone levels', col='green',horiz = TRUE)
hist(airquality$Solar.R)
boxplot(airquality$Solar.R)
boxplot(airquality[,0:4], main='Multiple Box plots')



library(lattice)  
#Loading the dataset
attach(mtcars)

gear_factor<-factor(gear,levels=c(3,4,5),
                    labels=c("3gears","4gears","5gears")) 

cyl_factor <-factor(cyl,levels=c(4,6,8),
                    labels=c("4cyl","6cyl","8cyl"))
densityplot(~mpg, main="Density Plot",  xlab="Miles per Gallon")
splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")

#Installing & Loading the package 


library(ggplot2)

#Loading the dataset
attach(mtcars)

# create factors with value labels 

mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),  
                      labels=c("3gears", "4gears", "5gears"))  
mtcars$am <- factor(mtcars$am,levels=c(0,1),  
                    labels=c("Automatic","Manual"))  
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),  
                     labels=c("4cyl","6cyl","8cyl"))
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + geom_point()
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = as.factor(cyl))) + geom_point()
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, size = qsec)) + geom_point()
p  <-  ggplot(mtcars,aes(mpg, wt, shape  =  factor(cyl)))
p + geom_point(aes(colour  =  factor(cyl)), size  =  4) + geom_point(colour  =  "grey90", size  =  1.5)

#Installing & Loading the package 

  
library(plotly)
p <- plot_ly(data = mtcars, x = ~hp, y = ~wt)
p

p <- plot_ly(data = mtcars, x = ~hp, y = ~wt, marker = list(size = 10, color = 'rgba(255, 182, 193, .9)', line = list(color = 'rgba(152, 0, 0, .8)', width = 2)))
p

data1 <- rnorm(100, mean = 10)   
data2 <- rnorm(100, mean = 0)   
data3 <- rnorm(100, mean = -10)   
x <- c(1:100)
data <- data.frame(x, data1, data2, data3)
p <- plot_ly(data, x = ~x)%>%   
  
  add_trace(y = ~data1, name = 'data1',mode = 'lines')%>%             
  add_trace(y = ~data2, name = 'data2', mode = 'lines+markers')%>% 
  add_trace(y = ~data3, name = 'data3', mode = 'markers')
p

p <- plot_ly(data = mtcars, x =~hp, y = ~wt,color = ~hp, size = ~hp )
p


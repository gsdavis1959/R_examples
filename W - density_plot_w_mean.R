x <- as.vector(rnorm(10000))
d <- as.data.frame(x=x)
library(ggplot2)
p <- ggplot(data = d) + theme_bw() + 
  geom_density(aes(x=x, y = ..density..), color = 'black')
# new code is below
q5 <- quantile(x,.05)
q95 <- quantile(x,.95)
medx <- median(x)
x.dens <- density(x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
p + geom_area(data = subset(df.dens, x >= q5 & x <= q95), 
              aes(x=x,y=y), fill = 'blue') +
                geom_vline(xintercept = medx)

ggplot(data = d) + theme_bw() + 
  geom_density(aes(x=x, y = ..density..), color = 'black') + 
  geom_line(aes(x=mean(x), y=c(0,.4) ) )

library(tidyverse)

# create a data frame
variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)

cars <- mtcars %>%
  select(cyl, mpg, hp, qsec) %>%
  group_by(cyl) %>%
  summarise(mpg = mean(mpg), hp = mean(hp), qsec = mean(qsec))

data2 <- data %>%
  group_by(variety, treatment) %>%
  select(variety, treatment, note) %>%
  summarise(avg_note = mean(note))
head(data2)

# grouped boxplot
ggplot(data2, aes(x=variety, y=avg_note, fill=treatment)) + 
  geom_boxplot()

# grouped boxplot
ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
    geom_boxplot()

# One box per treatment
ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment)
# one box per variety
ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~variety, scale="free")


library(tidyverse)
df <- data.frame(month=rep(1:3,2),
                 student=rep(c("Amy", "Bob"), each=3),
                 A=c(9, 7, 6, 8, 6, 9),
                 B=c(6, 7, 8, 5, 6, 7))

df

df %>% 
  gather(variable, value, -(month:student)) %>%
  unite(temp, student, variable) %>%
  spread(temp, value)

library(data.table) ## v >= 1.9.6
dcast(setDT(df), month ~ student, value.var = c("A", "B")) 

# try adding an id

df2 <- df %>%
  mutate(id = row.names(df))

dcast(setDT(df2), id ~ student, value.var = c("A", "B"))

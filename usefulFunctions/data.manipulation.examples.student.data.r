students <- read.csv("https://www.casaonofri.it/_datasets/students.csv", header=T)
head(students)

# subset
subData <- subset(students, Mark >= 28)
head(subData)

subData <- subset(students, Mark <= 28 & Mark >=26)
head(subData)

# range
subData <- subset(students, Mark <= 28 & Mark >=26)
head(subData)

# or
subData <- subset(students, Mark <= 28 & Mark >=26 & 
                    Subject == "MATHS" | 
                    Subject == "CHEMISTRY")
head(subData)

# select columns
subData <- subset(students, Mark <= 28 & Mark >=26 & 
                    Subject == "MATHS" | 
                    Subject == "CHEMISTRY",
                  select = c(Subject, Mark, HighSchool))
head(subData)

# drop columns
subData <- subset(students, Mark <= 28 & Mark >=26 & 
                    Subject == "MATHS" | 
                    Subject == "CHEMISTRY",
                  select = c(-Date,
                             -Year))
head(subData)

subData <- students[(students$Mark <= 29 & students$Mark >=25),]
head(subData)

# add NA
subData <- students
subData[(subData$Mark <= 29 & subData$Mark >=25), "Mark"] <- NA
head(subData)

# replace NA with 0
subData[is.na(subData$Mark) == T, "Mark"] <- 0 
head(subData)

# sorting
sortedData <- students[order(students$Mark), ]
head(sortedData)

# decreasing
sortedData <- students[order(-students$Mark), ]
head(sortedData)

# multiple keys
sortedData <- students[order(-students$Mark, students$Subject), ]
head(sortedData)

sortedData <- students[order(-students$Mark, -xtfrm(students$Subject)), ]
head(sortedData)

# casting
rimsulfuron <- read.csv("https://www.casaonofri.it/_datasets/rimsulfuron.csv", header=T)
head(rimsulfuron)
library(tidyverse)
rimsulfuron <- rimsulfuron %>%
  rename(Herbicide = ï..Herbicide)
library(reshape)
castData <- cast(Herbicide ~ Block, data = rimsulfuron,
                 value = "Yield")
head(castData)

library(reshape2)
castData <- as.data.frame(castData)
mdati <- melt(castData,
              variable.name = "Block",
              value.name = "Yield",
              id=c("Herbicide"))

head(mdati)

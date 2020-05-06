install.packages("explor")
library(explor)
library(FactoMineR)
library(tidyverse)
library(remotes)
library(RCurl) 


data(decathlon)
head(decathlon)
names(decathlon)
pca <- PCA(decathlon[,1:12], quanti.sup = 11:12, graph = FALSE)
explor(pca)

data(hobbies)
mca <- MCA(hobbies[1:1000,c(1:8,21:23)],quali.sup = 9:10, quanti.sup = 11, ind.sup = 1:100)
names(hobbies)
explor(mca)

url <- 'sftp://raspberrypi/media/pi/GSD_02_2020/Data/Datasets/Education/StudentsPerformance.csv'
data <- getURL(url, userpwd = "pi:clayclaire", connecttimeout = 60)
df <- read.csv(text = data, stringsAsFactors = FALSE)
head(df)
names(df)
df <- na.omit(df)
pca <- PCA(df[,c(6:8)], quanti.sup = 3, graph = FALSE)
explor(pca)

#Association Rule
library(arules)
myurl <- "https://docs.google.com/spreadsheets/d/18KBtFWkMq1Q9mOSVo9Q55GJ9IeC3NRYRn7yV5Id3z6A/pub?gid=0&single=true&output=csv"
data.raw <- read.transactions(url(myurl), sep=",") #Please use read.transactions! It's not read.csv!
inspect(data.raw)
rules<-apriori(data.raw)
inspect(rules)

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

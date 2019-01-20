library(reshape2)
data("mtcars")
View(mtcars)

# A sample vector
v <- c(1,4,4,3,2,2,3)

subset(v, v<3)
v[v<3]

# Another vector
t <- c("small", "small", "large", "medium")

# Remove "small" entries
subset(t, t!="small")
t[t!="small"]

#' 
#' One important difference between the two methods is that you can assign values to elements with square bracket indexing, but you cannot with `subset()`.
#' 
## ------------------------------------------------------------------------
v[v<3] <- 9
# 9 4 4 3 9 9 3

subset(v, v<3) <- 9
# Error in subset(v, v < 3) <- 9 : could not find function "subset<-"

#' 
#' With data frames:
#' 
## ------------------------------------------------------------------------
# A sample data frame
data <- read.table(header=T, text='
 subject sex size
       1   M    7
       2   F    6
       3   F    9
       4   M   11
 ')

subset(data, subject < 3)
data[data$subject < 3, ]

# Subset of particular rows and columns
subset(data, subject < 3, select = -subject)
subset(data, subject < 3, select = c(sex,size))
subset(data, subject < 3, select = sex:size)
data[data$subject < 3, c("sex","size")]

# Logical AND of two conditions
subset(data, subject < 3  &  sex=="M")
data[data$subject < 3  &  data$sex=="M", ]

# Logical OR of two conditions
subset(data, subject < 3  |  sex=="M")
data[data$subject < 3  |  data$sex=="M", ]

# Condition based on transformed data
subset(data, log2(size)>3 )
data[log2(data$size) > 50, ]

# Subset if elements are in another vector
subset(data, subject %in% c(1,3))
data[data$subject %in% c(1,3), ]

mtcars$id <- 1:nrow(mtcars)
mtcars$id

mtcars.lng <- melt(mtcars, id=c("id", "mpg"))
head(mtcars.lng)


mtcars.wide <- dcast(mtcars.lng, id + mpg ~ variable)
head(mtcars.wide)

# build a table and average by a variable
library(data.table)
dt <- data.table(mtcars)
dt
dt[, mean(disp), by="cyl"]

# filter a dataframe
library(tibble)
library(dplyr)
data(iris)
head(iris)
iris %>% filter(Species == "setosa") %>% group_by(Species, .drop=TRUE)




# create a table with values and then cast the table
# create observations
genes = paste('MMP', sprintf("%04d",1:10), sep="")
genes
# create another column with conditions
data = expand.grid(gene=genes, condition=c('copper', 'cheetos', 'beer', 'pizza'))
data
# add values
data$value = rnorm(40)
data
# cast into table
dcast(data, gene ~ condition)

# change rownames to a named column
subject<-c(4,4,2,2,3,3)
correct<-c(0,1,1,1,0,0)
test<-data.frame(subject,correct)
freq_test<-head(table(test$subject,test$correct))
View(freq_test)

# deal with rownames
df<-data.frame(as.numeric(rownames(freq_test)),freq_test)
df
colnames(df)[1]="subject"
df

# pivot tables
library(pivottabler)
library(dplyr)


# arguments: qhpvt(dataFrame, rows, columns, calculations, ...)
data(bhmtrains)
names(bhmtrains)
head(bhmtrains)
qhpvt(bhmtrains, "TOC", "TrainCategory", "n()") # TOC = Train Operating Company 

pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()
pt

# additional grouped rows
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addColumnDataGroups("PowerType")    #    << **** CODE CHANGE **** <<
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()
pt

# expand colunn totals
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addColumnDataGroups("PowerType", expandExistingTotals=TRUE) # << ** CODE CHANGE ** <<
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()
pt

# Instead of adding "PowerType" as columns, it can also be added as rows:


pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$addRowDataGroups("PowerType")    #    << **** CODE CHANGE **** <<
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()

# It is possible to selectively show/hide totals using the addTotal argument.
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC", totalCaption="Grand Total")    #    << **** CODE CHANGE **** <<
pt$addRowDataGroups("PowerType")
pt$addRowDataGroups("SchedSpeedMPH", addTotal=FALSE)      #    << **** CODE CHANGE **** <<
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()

# quick pivot example
qhpvt(bhmtrains, c("=", "TOC"), c("TrainCategory", "PowerType"),
      c("Number of Trains"="n()", "Maximum Speed"="max(SchedSpeedMPH, na.rm=TRUE)"))
# A quick pivot table with a format specified:
qhpvt(bhmtrains, "TOC", "TrainCategory", "mean(SchedSpeedMPH, na.rm=TRUE)", format="%.0f")
# A quick pivot table with two calculations that are formatted differently:
qhpvt(bhmtrains, "TOC", "TrainCategory", 
      c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)", "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"), 
      formats=list("%.0f", "%.1f"))
# the totals can now be renamed to "All ." using:
qhpvt(bhmtrains, "TOC", "TrainCategory", 
      c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)", "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"), 
      formats=list("%.0f", "%.1f"), totals=list("TOC"="All TOCs", "TrainCategory"="All Categories"))

 

## DDPLY FUNCTION IN THE PLYR PACKAGE 
## Use 'nrow' to find the count of a particular variable  
library(plyr)
ddply(mtcars, .(mpg), "nrow")
ddply(mtcars, .(cyl), "nrow")
ddply(mtcars, .(hp, mpg), "nrow")
## use 'summarise' to summarize numeric variables
ddply(mtcars, .(cyl), summarise, mean_mpg = mean(mpg))
ddply(mtcars, .(hp), summarise, mean_mpg = mean(mpg))
ddply(mtcars, .(wt), summarise, min_disp = min(disp), 
      max_disp = max(disp), mean_disp = mean(disp))
ddply(mtcars, .(hp), summarise, mpg=length(mpg),
      min_mpg = min(mpg), max_mpg = max(mpg), mean_mpg = mean(mpg))

## AGGREGATE FUNCTION FROM BASE R
aggregate(mpg ~ cyl, data=mtcars, FUN=mean)
aggregate(hp ~ mpg + wt, data=mtcars, FUN=mean)

# another way of doing pivot tables
library(rpivotTable)
data(HairEyeColor)
HairEyeColor
rpivotTable(data = HairEyeColor, rows = "Hair",cols="Eye", vals = "Freq", aggregatorName = "Sum", rendererName = "Table", width="100%", height="400px")
rpivotTable(mtcars,rows="gear", cols=c("cyl","carb"),subtotals=TRUE, width="100%", height="400px")
iris %>%
        tbl_df %>%
        filter( Sepal.Width > 3 & Sepal.Length > 5 ) %>%
        rpivotTable(rows="Sepal.Width",  rendererName="Treemap")

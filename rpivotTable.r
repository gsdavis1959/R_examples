# Read packages into R library
library(data.table)
library(rpivotTable)
library(htmlwidgets)
## Install packages
# library(devtools)
#install_github("ramnathv/htmlwidgets") 
#install_github("smartinsightsfromdata/rpivotTable")
## Load rpivotTable

data(mtcars)
## One line to create pivot table
rpivotTable(mtcars, rows="gear", col="cyl", aggregatorName="Average", 
            vals="mpg", rendererName="Treemap")

library(pivottabler)
arguments:  qhpvt(dataFrame, rows, columns, calculations, ...)
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

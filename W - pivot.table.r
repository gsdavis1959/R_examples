library(pivottabler)
# arguments:  qhpvt(dataFrame, rows, columns, calculations, ...)
names(bhmtrains)
qhpvt(bhmtrains, "TOC", "TrainCategory", "n()") # TOC = Train Operating Company 

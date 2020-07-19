library(sjPlot)
library(sjmisc)
data(efc)
head(efc)
# find all variables from COPE-Index, which all 
# have a "cop" in their variable name, and then
# plot the items as likert-plot
mydf <- find_var(efc, pattern = "cop", out = "df")
head(mydf)
plot_likert(mydf)

plot_likert(
  mydf,
  grid.range = c(1.2, 1.4),
  expand.grid = FALSE,
  values = "sum.outside",
  show.prc.sign = TRUE
)
# creates a HTML-table of the results of an PCA.
sjt.pca(mydf)

groups <- sjt.pca(mydf)$factor.index
plot_likert(mydf, groups = groups, values = "sum.outside")

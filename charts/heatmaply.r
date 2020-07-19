library(heatmaply)

mtcars2 <- percentize(mtcars)

heatmaply(mtcars2, k_row = 4, k_col = 2)

heatmaply_cor(
  cor(mtcars),
  xlab = "Features",
  ylab = "Features",
  k_col = 2,
  k_row = 2
)

r <- cor(mtcars)
## We use this function to calculate a matrix of p-values from correlation tests
## https://stackoverflow.com/a/13112337/4747043
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}
p <- cor.test.p(mtcars)

heatmaply_cor(
  r,
  node_type = "scatter",
  point_size_mat = -log10(p), 
  point_size_name = "-log10(p-value)",
  label_names = c("x", "y", "Correlation")
)

# save in html
dir.create("folder")
heatmaply(mtcars, file = "folder/heatmaply_plot.html")
browseURL("folder/heatmaply_plot.html")

# save as pdf
dir.create("folder")
# Before the first time using this code you may need to first run:
# webshot::install_phantomjs() or to install 
# [plotly's orca](https://github.com/plotly/orca) program.
heatmaply(mtcars, file = "folder/heatmaply_plot.png")
browseURL("folder/heatmaply_plot.png")
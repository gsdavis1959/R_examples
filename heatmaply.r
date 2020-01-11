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

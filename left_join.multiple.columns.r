library(dplyr)

d1 <- data_frame(
  x = letters[1:3],
  y = LETTERS[1:3],
  a = rnorm(3)
)

d1

d2 <- data_frame(
  x2 = letters[3:1],
  y2 = LETTERS[3:1],
  b = rnorm(3)
)

d2

left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))

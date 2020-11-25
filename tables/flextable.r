library(flextable)
library(officer)

myft <- flextable(
  head(mtcars), 
  col_keys = c("am", "carb", "gear", "mpg", "drat" ))
myft

myft <- theme_vanilla(myft)
myft <- color(myft, color = "black", part = "all")
myft <- bg(myft, bg = "transparent", part = "all")
myft <- italic(myft, j = 1)
myft <- bg(myft, bg = "#C90000", part = "header")
myft <- color(myft, color = "white", part = "header")
myft <- color(myft, ~ drat > 3.5, ~ drat, color = "green")
myft <- bold(myft, ~ drat > 3.5, ~ drat, bold = TRUE)
myft <- autofit(myft)
myft

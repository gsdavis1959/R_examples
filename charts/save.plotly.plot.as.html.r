library(plotly)
library(tidyverse)

p <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(p)
#> File is: 3.495 MB
widget_file_size(partial_bundle(p))
#> File is: 1.068 MB

library(htmlwidgets)
p <- plot_ly(x = rnorm(100))
saveWidget(p, "p1.html", selfcontained = F, libdir = "lib")
saveWidget(p, "p2.html", selfcontained = F, libdir = "lib")


zip("p1.zip", c("p1.html", "lib"))
zip("p2.zip", c("p2.html", "lib"))




    

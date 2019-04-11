library(rmarkdown)
library(lattice)
xyplot(mpg ~ disp, mtcars)
library(xtable)
html <- print(xtable(head(mtcars[1:3])), type="html", print.results=FALSE)
html

library(layoutEnginePhantomJS)
grid.html(html,
          x=unit(1, "npc") - unit(2, "mm"),
          y=unit(1, "npc") - unit(2, "mm"),
          just=c("right", "top"))

xyplot(mpg ~ disp, mtcars,
       panel=function(...) {
         panel.xyplot(...)
         grid.html(html,
                   x=unit(1, "npc") - unit(2, "mm"),
                   y=unit(1, "npc") - unit(2, "mm"),
                   just=c("right", "top"))
       })











render("example.Rmd", "html_document")
options(layoutEngine.backend=phantomjsEngine)
html <- readLines("example.html")
htmlDoc <- htmlDocument(html)
xyplot(mpg ~ disp, mtcars,
       panel=function(...) {
         panel.xyplot(...)
         grid.html(htmlDoc,
                   x=unit(1, "npc") - unit(1, "mm"),
                   y=unit(1, "npc") - unit(1, "mm"),
                   width=unit(3, "in"),
                   just=c("right", "top"))
       })
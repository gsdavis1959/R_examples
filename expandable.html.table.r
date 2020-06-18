library(reactable)
library(dplyr)
library(rio)

reactable(mtcars)
reactable(mtcars, searchable = TRUE, showSortable = TRUE, showSortIcon = TRUE)

reactable(mtcars, searchable = TRUE, showSortable = TRUE, showSortIcon = TRUE,
          columns = list(
            mpg = colDef(html = TRUE, resizable = TRUE),
            disp = colDef(show = FALSE)
          )
) 

# Function needed according to Greg Lin, creator of reactable
html <- function(x, inline = FALSE) {
  container <- if (inline) htmltools::span else htmltools::div
  container(dangerouslySetInnerHTML = list("__html" = x))
}
reactable(mtcars, searchable = TRUE, showSortable = TRUE,
          columns = list(
            mpg = colDef(html = TRUE, resizable = TRUE),
            hp = colDef(show = FALSE)
          ),
          # if there exists a comment, make row expandable
          details = function(index) {
            if(mtcars$cyl[index] != "") {
              htmltools::tagList(
                html(mtcars$cyl[index])
              )
            } 
          }
)





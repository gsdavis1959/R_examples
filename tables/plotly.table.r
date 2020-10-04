library(plotly)
library(tidyverse)

fig <- plot_ly(
  type = 'table',
  header = list(
    values = c("<b>Cars</b>", names(mtcars)),
    align = c('left', rep('center', ncol(mtcars))),
    line = list(width = 1, color = 'black'),
    fill = list(color = 'rgb(235, 100, 230)'),
    font = list(family = "Arial", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(
      rownames(mtcars), 
      t(as.matrix(unname(mtcars)))
    ),
    align = c('left', rep('center', ncol(mtcars))),
    line = list(color = "black", width = 1),
    fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
    font = list(family = "Arial", size = 12, color = c("black"))
  ))

fig


# change size of rows
values <- rbind(c('Salaries', 'Office', 'Merchandise', 'Legal', '<b>TOTAL<br>EXPENSES</b>'), c("Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad",                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad"))

fig <- plot_ly(
  type = 'table',
  columnorder = c(1,2),
  columnwidth = c(80,400),
  header = list(
    values = c('<b>EXPENSES</b><br>as of July 2017', '<b>DESCRIPTION</b>'),
    line = list(color = '#506784'),
    fill = list(color = '#119DFF'),
    align = c('left','center'),
    font = list(color = 'white', size = 12),
    height = 40
  ),
  cells = list(
    values = values,
    line = list(color = '#506784'),
    fill = list(color = c('#25FEFD', 'white')),
    align = c('left', 'center'),
    font = list(color = c('#506784'), size = 12),
    height = 30
  ))

fig



p <- plot_ly(
  type = 'table',
  header = list(
    values = c("<b>Cars</b>", names(mtcars)),
    align = c('left', rep('center', ncol(mtcars))),
    line = list(width = 1, color = 'black'),
    fill = list(color = 'rgb(235, 100, 230)'),
    font = list(family = "Arial", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(
      rownames(mtcars), 
      t(as.matrix(unname(mtcars)))
    ),
    align = c('left', rep('center', ncol(mtcars))),
    line = list(color = "black", width = 1),
    fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
    font = list(family = "Arial", size = 12, color = c("black"))
  ))

p


      
p <- p %>% layout(
      updatemenus = list(
        list(
          type = 'dropdown',
          active = 0,
          buttons = list(
            list(method = "restyle",
                 args = list("transforms[0].value", unique(mtcars$gear)[1]),
                 label = unique(mtcars$gear)[1]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(mtcars$gears)[2])),
                 label = unique(mtcars$gears)[2]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(mtcars$gears)[3]),
                 label = unique(mtcars$gears)[3]),
            list(method = "restyle",
               args = list("transforms[0].value", unique(mtcars$gears)[4]),
               label = unique(mtcars$gears)[4])
          )
        )
      )
)    
p




p <- plot_ly(
  type = 'table',
  transforms = list(
    list(
      type = 'filter',
      target = ~mtcars$gear,
      operation = '=',
      value = mtcars$gear[1])),
  header = list(
    values = c("<b>Cars</b>", names(mtcars)),
    align = c('left', rep('center', ncol(mtcars))),
    line = list(width = 1, color = 'black'),
    fill = list(color = 'rgb(235, 100, 230)'),
    font = list(family = "Arial", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(
      rownames(mtcars), 
      t(as.matrix(unname(mtcars)))
    ),
    align = c('left', rep('center', ncol(mtcars))),
    line = list(color = "black", width = 1),
    fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
    font = list(family = "Arial", size = 12, color = c("black"))
  ))

p




transforms = list(
  list(
    type = 'filter',
    target = ~mtcars$gear,
    operation = '=',
    value = mtcars$gear))

print(transforms)

recap_option<-list() %>% print()


recap_option[[1]]<-list(method = "restyle",
                        args = list("transforms[0].value", unique(mtcars$gear)[1]),
                        label = unique(mtcars$gear)[1]) %>% print()
recap_option[[2]]<-list(method = "restyle",
                        args = list("transforms[0].value", unique(mtcars$gears)[2]),
                        label = unique(mtcars$gears)[2]) %>% print()   
recap_option[[3]]<-list(method = "restyle",
                        args = list("transforms[0].value", unique(mtcars$gears)[3]),
                        label = unique(mtcars$gears)[3]) %>% print()


a <- unique(mtcars$gear)[3] %>% print()

p%>%layout(title = 'Cars',
           updatemenus=list(
             list(
               buttons=recap_option
             )
           ))



plot_ly(type="table",header=list(values=names(mtcars)), cells=list(values=unname(mtcars),
              transforms = list(
                            list(
                                type = 'filter',
                                target = ~mtcars$gear,
                                operation = '=',
                                value = 3)))) 



install.packages("rpivotTable")
library(rpivotTable)

data("Titanic")

rpivotTable(Titanic)


# pre-populate

rpivotTable(Titanic, rows = c("Class","Sex"), 
            cols = c("Age","Survived"), aggregatorName = "Sum", vals = "Freq")


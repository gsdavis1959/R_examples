# with html widgets
library(dplyr)
library(sparkline)
library(htmlwidgets)
library(reactable)

data <- chickwts %>%
  group_by(feed) %>%
  summarise(weight = list(weight)) %>%
  mutate(boxplot = NA, sparkline = NA)

reactable(data, columns = list(
  weight = colDef(cell = function(values) {
    sparkline(values, type = "bar")
  }),
  boxplot = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]], type = "box")
  }),
  sparkline = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]])
  })
))

# with borders
reactable(iris[1:5, ], bordered = TRUE)

# bordered, stipped, highlighted
reactable(iris[1:5, ], bordered = TRUE, striped = TRUE, highlight = TRUE)

# with sticky head and footer
reactable(
  iris[1:20, ],
  height = 270,
  striped = TRUE,
  borderless = TRUE,
  defaultColDef = colDef(
    footer = function(values, name) htmltools::div(name, style = list(fontWeight = 600))
  )
)

# nested tables
data <- unique(CO2[, c("Plant", "Type")])

reactable(data, details = function(index) {
  plant_data <- CO2[CO2$Plant == data$Plant[index], ]
  htmltools::div(style = "padding: 16px",
                 reactable(plant_data, outlined = TRUE)
  )
})

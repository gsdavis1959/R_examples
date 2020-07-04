library(upsetjs)
listInput <- list(one = c("a", "b", "c", "e", "g", "h", "k", "l", "m"), two = c("a", 
                                                                                "b", "d", "e", "j"), three = c("a", "e", "f", "g", "h", "i", "j", "l", "m"))

w <- upsetjs() %>% fromList(listInput) %>% interactiveChart()
w

# example of expression input
expressionInput <- list(one = 9, two = 5, three = 9, `one&two` = 3, `one&three` = 6, 
                        `two&three` = 3, `one&two&three` = 2)


upsetjs() %>% fromExpression(expressionInput)

# boolean table with rows = elements, columns = sets, cell = is row part of this
# set
dataFrame <- as.data.frame(list(one = c(1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1), 
                                two = c(1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0),
                                three = c(1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1)),
                                row.names = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", 
                                         "l", "m"))
                                

print(dataFrame)                                                                          


upsetjs() %>% fromDataFrame(dataFrame)

upsetjs() %>% fromList(listInput) %>% generateDistinctIntersections()

upsetjs() %>% fromList(listInput) %>% generateIntersections(min = 2, max = NULL, 
                                                            empty = T, order.by = "cardinality", limit = NULL)

upsetjs() %>% fromList(listInput) %>% generateUnions(min = 0, max = 2, empty = T, 
                                                     order.by = "degree", limit = NULL)

upsetjs() %>% fromList(listInput) %>% interactiveChart()

# choose selection
upsetjs() %>% fromList(listInput) %>% setSelection("one")
upsetjs() %>% fromList(listInput) %>% setSelection(c("one", "two"))

# highlight sets with queries
upsetjs() %>% fromList(listInput) %>% addQuery("Q1", color = "red", elems = c("a", 
                                                                              "b", "c")) %>% addQuery("Q2", color = "blue", set = "two")
# add box plots
upsetjs() %>% fromDataFrame(dataFrame, attributes = list(attr = runif(nrow(dataFrame))))

# styling
upsetjs() %>% fromList(listInput) %>% chartTheme("dark")

upsetjs() %>% fromList(listInput) %>% chartLabels(title = "Chart Title", description = "this is a long chart description")

upsetjs() %>% fromList(listInput) %>% chartLabels(combination.name = "Combination Label", 
                                                  set.name = "Set Label")

upsetjs() %>% fromList(listInput) %>% chartLayout(numerical.scale = "log")

# venn diagrams
w <- upsetjsVennDiagram() %>% fromList(listInput) %>% interactiveChart()
w

we <- upsetjsEulerDiagram() %>% fromList(listInput) %>% interactiveChart()
we

upsetjsVennDiagram() %>% fromList(listInput) %>% chartVennLabels(title = "Chart Title", 
                                                                 description = "this is a long chart description") %>% interactiveChart()

# colors
listInput <- list(s1 = c('a', 'b', 'c', 'e', 'g', 'h', 'k', 'l', 'm'), s2 = c('a', 'b', 'd', 'e', 'j'), s3 = c('a', 'e', 'f', 'g', 'h', 'i', 'j', 'l', 'm'))
colors <- list(s1 = '#1f77b4', s2 = '#2ca02c', s3 = '#d62728', `s1&s2` = '#9467bd', `s1&s3` = '#8c564b', `s2&s3` = '#e377c2', `s1&s2&s3` = '#bcbd22')

render <- function(upsetjs) {
  upsetjs %>% fromList(listInput, colors=colors) %>% chartTheme(selection.color='colors', has.selection.opacity=0.3) %>% interactiveChart()
}

v <- upsetjs() %>% render()
v

v <- upsetjsVennDiagram() %>% render()
v

v <- upsetjsEulerDiagram() %>% render()
v

library(tibble)

t <- tribble(
  ~set1, ~set2, ~set3,
  1,   1,   0,
  0,   0,   1,
  0,   1,   1,
  0,   0,   1,
  0,   0,   1,
  0,   1,   1,
  1,   0,   1,
  0,   1,   1,
  0,   0,   1,
  0,   0,   1,
  1,   1,   1,
  1,   0,   0,
  0,   0,   1,
  0,   1,   0,
  1,   1,   1,
  0,   1,   0,
  0,   1,   1,
  0,   1,   0,
  0,   0,   1,
  0,   0,   1
)

upsetjs() %>% fromDataFrame(t) %>% generateIntersections() %>% interactiveChart()

upsetjs() %>% fromDataFrame(t) %>% generateUnions() %>% interactiveChart()

upsetjs() %>% fromDataFrame(t) %>% generateDistinctIntersections()

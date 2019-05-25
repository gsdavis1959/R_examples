library(glue)
library(data.table, warn.conflicts = FALSE)

irisDT <- as.data.table(iris)

irisDT[, description_glue := glue_data(.SD, "This {Species} has a petal length of {Petal.Length}")]
irisDT[, description_glue2 := glue("This {Species} has a petal length of {Petal.Length}", 
                                   Species = Species, Petal.Length = Petal.Length)]
irisDT[, description_glue3 := glue("This {Species} has a petal length of {Petal.Length}", 
                                   .envir = .SD)]
irisDT

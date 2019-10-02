# get the french fries data
library(reshape2)
library(glue)
knitr::kable(head(french_fries))

fries_names <- names(french_fries)
fries_inline <- glue::glue_collapse(fries_names, 
                                    sep = ", ")



fries_inline <- glue::glue_collapse(fries_names, 
                                    sep = ", ",
                                    last = ", and ")
fries_inline
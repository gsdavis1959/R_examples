library(datawizard)

# select and filter
matching_rows <- data_match(mtcars, data.frame(vs = 0, am = 1))
mtcars[matching_rows, ]

# add prefix

head(data_addprefix(iris, "NEW_"))

# standarize
# before
summary(swiss)

# after
summary(standardize(swiss))

wide_data <- data.frame(replicate(5, rnorm(10)))

data_to_long(wide_data)

long_data <- data_to_long(wide_data, rows_to = "Row_ID") # Save row number

data_to_wide(long_data,
             colnames_from = "Name",
             values_from = "Value",
             rows_from = "Row_ID"
)

data(iris)
describe_distribution(iris)

describe_distribution(mtcars$wt)

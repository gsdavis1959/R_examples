library(DataEditR)

data_edit(mtcars)

# Save output to R object & csv file
mtcars_new <- data_edit(mtcars,
                        save_as = "mtcars_new.csv")
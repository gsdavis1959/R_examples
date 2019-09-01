devtools::install_github("laresbernardo/lares")
library(lares)
library(dplyr)
data("starwars")
# Let's get rid of the lists inside the dataframe
df <- select(starwars, -starships, -vehicles, -films)
head(df)
corr_cross(df)

data(dft)
# Let's get rid of some noisy columns first

dft <- select(dft, -Cabin, -Ticket)
dft <- na.omit(dft)
corr_cross(dft, top = 15)
corr_cross(df, type = 2)
corr_cross(df, type = 2, contains = "species")

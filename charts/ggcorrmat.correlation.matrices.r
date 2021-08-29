library(gapminder)
library(dplyr)

dplyr::glimpse(gapminder)

# setup
set.seed(123)
library(ggstatsplot)

# select data only from the year 2007
gapminder_2007 <- dplyr::filter(gapminder::gapminder, year == 2007)

# producing the correlation matrix
ggcorrmat(
  data = gapminder_2007, # data from which variable is to be taken
  cor.vars = lifeExp:gdpPercap # specifying correlation matrix variables
)

# for reproducibility
set.seed(123)

# let's obtain correlation coefficients with their CIs
grouped_ggcorrmat(
  data = ggplot2::msleep,
  cor.vars = sleep_total:awake,
  grouping.var = vore,
  output = "dataframe"
)

# for reproducibility
set.seed(123)

# plot faceted
grouped_ggcorrmat(
  # arguments relevant for `ggcorrmat`
  data = ggplot2::diamonds,
  type = "bayes", # Bayesian test
  grouping.var = cut,
  # arguments relevant for `combine_plots`
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    tag_levels = "a",
    title = "Relationship between diamond attributes and price across cut",
    caption = "Dataset: Diamonds from ggplot2 package"
  )
)

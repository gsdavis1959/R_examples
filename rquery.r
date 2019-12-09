library(rquery)

d <- data.frame(
  x = c(1, 1, 2),
  y = c(5, 4, 3),
  z = c(6, 7, 8)
)

knitr::kable(d)
drop_columns(d, c('y', 'z'))

d %.>%
  drop_columns(., c('y', 'z')) %.>%
  knitr::kable(.)

d %.>%
  rename_columns(., 
                 c('x_new_name' = 'x', 
                   'y_new_name' = 'y')
  ) %.>%
  knitr::kable(.)

d %.>%
  select_rows(., x == 1) %.>%
  knitr::kable(.)

d %.>%
  order_rows(., 
             c('x', 'y'),
             reverse = 'x') %.>%
  knitr::kable(.)

d %.>%
  extend(., zzz := y / x) %.>%
  knitr::kable(.)

shift <- data.table::shift

d %.>%
  extend(.,
         max_y := max(y),
         shift_z := shift(z),
         row_number := row_number(),
         cumsum_z := cumsum(z),
         partitionby = 'x',
         orderby = c('y', 'z')) %.>%
  knitr::kable(.)

d %.>%
  project(.,
          max_y := max(y),
          count := n(),
          groupby = 'x') %.>%
  knitr::kable(.)


d %.>%
  project(.,
          max_y := max(y),
          count := n()) %.>%
  knitr::kable(.)

d_left <- data.frame(
  k = c('a', 'a', 'b'),
  x = c(1, NA, 3),
  y = c(1, NA, NA),
  stringsAsFactors = FALSE
)

knitr::kable(d_left)

d_right <- data.frame(
  k = c('a', 'b', 'q'),
  y = c(10, 20, 30),
  stringsAsFactors = FALSE
)

knitr::kable(d_right)

natural_join(d_left, d_right,
             by = 'k',
             jointype = 'LEFT') %.>%
  knitr::kable(.)


natural_join(d_left, d_right,
             by = 'k',
             jointype = 'RIGHT') %.>%
  knitr::kable(.)

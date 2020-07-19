library(gtsummary)
library(kableExtra)
library(gt)
# make dataset with a few variables to summarize
trial2 <- trial %>% dplyr::select(trt, age, grade, response)

# summarize the data with our package
table1 <- tbl_summary(trial2)

table2 <- tbl_summary(
  trial2,
  by = trt, # split table by group
  missing = "no" # don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test if there's difference between groups
  bold_labels()

mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t1 <- tbl_regression(mod1, exponentiate = TRUE)

tbl_summary(trial2) %>%
  as_kable_extra()

tbl_summary(trial2) %>%
  as_gt()



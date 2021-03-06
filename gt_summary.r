install.packages("gtsummary")
remotes::install_github("rstudio/gt")

library(gtsummary)
library(dplyr)

sm_trial <- trial %>% select(trt, age, response, grade)

head(sm_trial)

tbl_summary_1 <- tbl_summary(sm_trial)

tbl_summary_2 <- sm_trial %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  add_overall() %>% 
  bold_labels()

tbl_summary_1
tbl_summary_2

tbl_summary_3 <- sm_trial %>%
  tbl_summary(
    by = trt,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"), 
    label = age ~ "Patient Age") %>%
  add_p(test = all_continuous() ~ "t.test",
        pvalue_fun = function(x) style_pvalue(x, digits = 2))


tbl_summary_3

# regression
m1 <- glm(response ~ trt + grade + age, 
          data = trial,
          family = binomial) 

tbl_reg_1 <- tbl_regression(m1, exponentiate = TRUE)
tbl_reg_1

library(survival)

tbl_reg_3 <- 
  coxph(Surv(ttdeath, death) ~ trt + grade + age, 
        data = trial) %>%
  tbl_regression(exponentiate = TRUE)

tbl_reg_4 <-
  tbl_merge(
    tbls = list(tbl_reg_1, tbl_reg_3), 
    tab_spanner = c("**Tumor Response**", "**Time to Death**") 
  ) 

tbl_reg_3
tbl_reg_4



# printing trial data
head(trial) %>% knitr::kable()

trial2 =
  trial %>%
  select(trt, marker, stage)

tbl_summary(trial2)

tbl_summary(trial2, by = trt) %>% add_p()

trial2 %>%
  # build base summary table
  tbl_summary(
    by = trt,
    # change variable labels
    label = list(vars(marker) ~ "Marker, ng/mL",
                 vars(stage) ~ "Clinical T Stage"),
    # change statistics printed in table
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = list("marker" ~ c(1, 2))
  ) %>%
  # add p-values, report t-test, round large pvalues to two decimal place
  add_p(test = list(vars(marker) ~ "t.test"),
        pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  # add statistic labels
  add_stat_label() %>%
  # bold variable labels, italicize levels
  bold_labels() %>%
  italicize_levels() %>%
  # bold p-values under a given threshold (default is 0.05)
  bold_p(t = 0.2) %>%
  # include percent in headers
  modify_header(stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})")

trial %>%
  dplyr::select(trt, response, age, stage, marker, grade) %>%
  tbl_summary(
    by = trt,
    type = list(c("response", "grade") ~ "categorical"), # select by variables in a vector
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{p}%") # select by summary type/attribute
  ) %>%
  add_p(test = list(contains("response") ~ "fisher.test", # select using functions in tidyselect
                    all_continuous() ~ "t.test"))

tab1 = tbl_summary(trial2, by = trt)
tab1

tbl_summary(trial2, by = trt) %>%
  as_gt(exclude = "tab_footnote") %>%
  gtsummary::tab_spanner(label = gt::md("**Treatment Group**"),
                  columns = gt::starts_with("stat_"))

# save a table

# make model and tbl_regression object
m_linear <- lm(mpg ~ cyl, data = mtcars)
t1 <- tbl_regression(m_linear)

# Use function from gt package to save table, after converting to 
# gt object using as_gt()
gt::gtsave(as_gt(t1), file = file.path(tempdir(), "temp.png"))

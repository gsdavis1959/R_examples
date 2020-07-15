suppressPackageStartupMessages({
  library(tidyverse) # data workhorse
  library(readxl) # importing xlsx files
  library(correlationfunnel) # rapid exploratory data analysis
  library(cluster) # calculating gower distance and PAM
  library(Rtsne) # dimensionality reduction and visualization
  library(plotly) # interactive graphing
})

set.seed(175) # reproducibility

hr_data_tbl <- read_csv("https://raw.githubusercontent.com/gsdavis1959/Data/master/HR-Employee-Attrition.csv")

# examine the strength of relationship between attrition and the other variables in the data set

hr_corr_tbl <- hr_data_tbl %>%
  select(-EmployeeNumber) %>%
  binarize(n_bins = 5, thresh_infreq = 0.01, name_infreq = "OTHER", one_hot = TRUE) %>%
  correlate(Attrition__Yes)

hr_corr_tbl %>%
  plot_correlation_funnel() %>%
  ggplotly()

# select the variables we wish to analyze
var_selection <- c("EmployeeNumber", "Attrition", "OverTime", "JobLevel", "MonthlyIncome", "YearsAtCompany", "StockOptionLevel", "YearsWithCurrManager", "TotalWorkingYears", "MaritalStatus", "Age", "YearsInCurrentRole", "JobRole", "EnvironmentSatisfaction", "JobInvolvement", "BusinessTravel")

# several variables are character and need to be converted to factors
hr_subset_tbl <- hr_data_tbl %>%
  select(one_of(var_selection)) %>%
  mutate_if(is.character, as_factor) %>%
  select(EmployeeNumber, Attrition, everything())

# Compute Gower distance and covert to a matrix
gower_dist <- daisy(hr_subset_tbl[, 2:16], metric = "gower")
gower_mat <- as.matrix(gower_dist)

# Print most similar employees
hr_subset_tbl[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

#determine the optimal number of clusters for the data
sil_width <- map_dbl(2:10, function(k){
  model <- pam(gower_dist, k = k)
  model$silinfo$avg.width
})

sil_tbl <- tibble(
  k = 2:10,
  sil_width = sil_width
)
print(sil_tbl)

fig2 <- ggplot(sil_tbl, aes(x = k, y = sil_width)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)
ggplotly(fig2)

k <- 6

pam_fit <- pam(gower_dist, diss = TRUE, k)

hr_subset_tbl <- hr_subset_tbl %>%
  mutate(cluster = pam_fit$clustering)

#have a look at the centroids to understand the clusters
hr_subset_tbl[pam_fit$medoids, ]

attrition_rate_tbl <- hr_subset_tbl %>%
  select(cluster, Attrition) %>%
  mutate(attrition_num = (as.numeric(Attrition) *-1) +2) %>%
  group_by(cluster) %>%
  summarise(
    Cluster_Turnover_Rate  = (sum(attrition_num) /
                                length(attrition_num)) %>% scales::percent(accuracy = 0.1),
    Turnover_Count = sum(attrition_num),
    Cluster_Size = length(attrition_num)
  ) %>%
  ungroup() %>%
  mutate(Population_Turnover_Rate = (Turnover_Count /
                                       sum(Turnover_Count)) %>% scales::percent(accuracy = 0.1))

## `summarise()` ungrouping output (override with `.groups` argument)

attrition_rate_tbl

data_formatted_tbl <- hr_subset_tbl %>%
  left_join(attrition_rate_tbl) %>%
  rename(Cluster = cluster) %>%
  mutate(MonthlyIncome = MonthlyIncome %>% scales::dollar()) %>%
  mutate(description = str_glue("Turnover = {Attrition}

                                MaritalDesc = {MaritalStatus}
                                Age = {Age}
                                Job Role = {JobRole}
                                Job Level {JobLevel}
                                Overtime = {OverTime}
                                Current Role Tenure = {YearsInCurrentRole}
                                Professional Tenure = {TotalWorkingYears}
                                Monthly Income = {MonthlyIncome}
                             
                                Cluster: {Cluster}
                                Cluster Size: {Cluster_Size}
                                Cluster Turnover Rate: {Cluster_Turnover_Rate}
                                Cluster Turnover Count: {Turnover_Count}
                             
                                "))

## Joining, by = "cluster"

# map the clusters in 2 dimensional space using t-SNE
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_tbl <- tsne_obj$Y %>%
  as_tibble() %>%
  setNames(c("X", "Y")) %>%
  bind_cols(data_formatted_tbl) %>%
  mutate(Cluster = Cluster %>% as_factor())

g <- tsne_tbl %>%
  ggplot(aes(x = X, y = Y, colour = Cluster)) +
  geom_point(aes(text = description))

## Warning: Ignoring unknown aesthetics: text

ggplotly(g)

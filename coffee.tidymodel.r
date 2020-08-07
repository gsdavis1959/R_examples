library(tidyverse)
library(visdat)
coffee <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

visdat::vis_dat(coffee)

coffee %>% 
  filter(!is.na(country_of_origin)) %>%
  inner_join(
    coffee %>%
      group_by(country_of_origin) %>% 
      summarise(n = n(), average_cupper_points = mean(cupper_points)) %>%
      filter(n / sum(n) > 0.01),
    by = "country_of_origin"
  ) %>% 
  ggplot(aes(
    x = cupper_points,
    y = fct_reorder(country_of_origin, average_cupper_points),
    fill = average_cupper_points
  )) + 
  ggridges::geom_density_ridges() +
  xlim(5, 10) +
  scale_fill_gradient(low = "#A8805C", high = "#5F3622") +
  ggtitle("Coffee quality by country of origin") +
  xlab("cupper points") +
  ylab(NULL) +
  theme_minimal(base_size = 16, base_family = "Montserrat") +
  theme(legend.position = "none")

# modeling
library(tidymodels)
set.seed(123)
coffee_split <- initial_split(coffee, prop = 0.8)
coffee_train <- training(coffee_split)
coffee_test <- testing(coffee_split)

coffee_recipe <- recipe(coffee_train) %>%
  update_role(everything(), new_role = "support") %>% 
  update_role(cupper_points, new_role = "outcome") %>%
  update_role(
    variety, processing_method, country_of_origin,
    aroma, flavor, aftertaste, acidity, sweetness, altitude_mean_meters,
    new_role = "predictor"
  ) %>%
  step_string2factor(all_nominal(), -all_outcomes()) %>%
  step_knnimpute(country_of_origin,
                 impute_with = imp_vars(
                   in_country_partner, company, region, farm_name, certification_body
                 )
  ) %>%
  step_knnimpute(altitude_mean_meters,
                 impute_with = imp_vars(
                   in_country_partner, company, region, farm_name, certification_body,
                   country_of_origin
                 )
  ) %>%
  step_unknown(variety, processing_method, new_level = "unknown") %>%
  step_other(country_of_origin, threshold = 0.01) %>%
  step_other(processing_method, variety, threshold = 0.10) %>% 
  step_normalize(all_numeric(), -all_outcomes())
coffee_recipe

coffee_recipe %>% 
  prep(coffee_train) %>%
  bake(coffee_test) %>%
  count(processing_method)

coffee_model <- rand_forest(
  trees = tune(),
  mtry = tune()
) %>%
  set_engine("ranger") %>% 
  set_mode("regression")
coffee_model

coffee_workflow <- workflow() %>% 
  add_recipe(coffee_recipe) %>% 
  add_model(coffee_model)
coffee_workflow

coffee_grid <- expand_grid(mtry = 3:5, trees = seq(500, 1500, by = 200))

set.seed(123)
coffee_folds <- vfold_cv(coffee_train, v = 5)
coffee_folds

coffee_grid_results <- coffee_workflow %>% 
  tune_grid(
    resamples = coffee_folds,
    grid = coffee_grid
  )

collect_metrics(coffee_grid_results) %>%
  filter(.metric == "rmse") %>% 
  arrange(mean) %>%
  head() %>% 
  knitr::kable()

autoplot(coffee_grid_results, metric = "rmse")

show_best(coffee_grid_results, metric = "rmse") %>% knitr::kable()

select_by_pct_loss(coffee_grid_results, metric = "rmse", limit = 5, trees) %>%
  knitr::kable()

fitted_coffee_model <- coffee_workflow %>% 
  finalize_workflow(
    select_by_pct_loss(coffee_grid_results, metric = "rmse", limit = 5, trees)
  ) %>% 
  fit(coffee_train)

fitted_coffee_model %>%
  predict(coffee_test) %>%
  metric_set(rmse, mae, rsq)(coffee_test$cupper_points, .pred)

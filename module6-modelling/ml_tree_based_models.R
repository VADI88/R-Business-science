# LIBRARY ----

library(tidymodels)
library(tidyverse)
library(tidyquant)
library(readxl)

library(rpart.plot)

tidymodels_prefer()

source('scripts/utils.R')

# READING DATA ----

bike_orderlines_tbl <- read_rds(
  'data/bike_sales/data_wrangled/bike_order_line.rds'
)

glimpse(bike_orderlines_tbl)


####  Function to calcualte metrics ----

calculate_metrics <- function(model, new_data, y_column) {
  y_column <- enquo(y_column)
  model |>
    predict(new_data = new_data) |>
    bind_cols(new_data |> select(!!y_column)) |>
    metrics(truth = !!y_column, estimate = .pred)
}


# 2.0 TRAINING & TEST SETS ----

bike_feature_tbl <- bike_orderlines_tbl |>
  select(price, model, category_2, frame_material) |>
  distinct() |>
  mutate(id = row_number()) |>
  select(id, everything()) |>
  separate_bike_model(append = TRUE, keep_model_column = TRUE)

set.seed(1113)
split_obj <- initial_split(bike_feature_tbl, prop = 0.80, strata = 'base_model') # 80% of data => Training ; 20% of data ==> Testing
# strata : Random sampling to be conducted within strata column
#

training_tbl <- training(split_obj)
testing_tbl <- testing(split_obj)

# 3.0 DECISION TREE

model_04_decsion_tree_rpart <- decision_tree(
  mode = 'regression',
  cost_complexity = 0.001,
  tree_depth = 5,
  min_n = 10
) |>
  set_engine('rpart') |>
  fit(
    formula = price ~ .,
    data = training_tbl |> select(-id, -model, -model_tier)
  )


model_04_decsion_tree_rpart |>
  calculate_metrics(new_data = testing_tbl, y_column = price)


# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#     1 rmse    standard    1592.
# 2 rsq     standard       0.846
# 3 mae     standard    1152.
#

model_04_decsion_tree_rpart$fit |>
  rpart.plot(
    roundint = FALSE,
    type = 1,
    extra = 101,
    fallen.leaves = FALSE,
    main = 'Model 04: Decision Tree Model'
  )


# 4.0 RANDOM FOREST  ----
# WITH RANGER MODEL

model_05_random_forest_ranger <- rand_forest(
  mode = 'regression',
  mtry = 8,
  trees = 5000,
  min_n = 10
) |>
  set_engine('ranger', importance = 'impurity', replace = TRUE) |>
  fit(
    formula = price ~ .,
    data = training_tbl |> select(-id, -model, -model_tier)
  )


model_05_random_forest_ranger |>
  calculate_metrics(new_data = testing_tbl, y_column = price)


# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#     1 rmse    standard    1659.
# 2 rsq     standard       0.866
# 3 mae     standard    1062.

model_05_random_forest_ranger$fit |>
  ranger::importance() |>
  enframe() |>
  arrange(desc(value)) |>
  mutate(name = name |> as_factor() |> fct_rev()) |>
  ggplot(aes(y = name, x = value)) +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = number(value, accuracy = 2)),
    size = 3
  ) +
  scale_x_continuous(labels = number_format()) +
  labs(
    title = "Random Forest Regression: Feature Importance",
    subtitle = "Model 05: Ranger Model"
  )


# 4.1 RANDOM FOREST  ----
# WITH RANGER MODEL

model_06_random_forest_randomForest <- rand_forest(
  mode = 'regression'
) |>
  set_engine('randomForest') |>
  fit(
    formula = price ~ .,
    data = training_tbl |>
      select(-id, -model, -model_tier) |>
      mutate_if(is_character, as_factor)
  )


model_06_random_forest_randomForest |>
  calculate_metrics(new_data = testing_tbl, y_column = price)


# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#     1 rmse    standard    1659.
# 2 rsq     standard       0.866
# 3 mae     standard    1062.

model_06_random_forest_randomForest$fit |>
  randomForest::importance() |>
  as_tibble(rownames = 'names') |>
  arrange(desc(IncNodePurity)) |>
  mutate(names = names |> as_factor() |> fct_rev()) |>
  ggplot(aes(y = names, x = IncNodePurity)) +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = number(IncNodePurity, accuracy = 2)),
    size = 3
  ) +
  scale_x_continuous(labels = number_format()) +
  labs(
    title = "Random Forest Regression: Feature Importance",
    subtitle = "Model 06: RandomForest Model"
  )


#### XGBOOST ####

model_07_xgboost <- boost_tree(
  mode = 'regression'
) |>
  set_engine('xgboost') |>
  fit(
    formula = price ~ .,
    data = training_tbl |>
      select(-id, -model, -model_tier) |>
      mutate_if(is_character, as_factor)
  )

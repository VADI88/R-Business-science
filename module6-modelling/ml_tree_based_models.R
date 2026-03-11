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


model_07_xgboost |>
  calculate_metrics(new_data = testing_tbl, y_column = price)


# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# rmse    standard    1453.
# rsq     standard       0.874
# mae     standard     903.

model_07_xgboost$fit |>
  xgboost::xgb.importance() |>
  as_tibble() |>
  janitor::clean_names() |>
  arrange(desc(gain)) |>
  mutate(feature = feature |> as_factor() |> fct_rev()) |>
  ggplot(aes(y = feature, x = gain)) +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = number(gain, accuracy = 0.001)),
    size = 3
  ) +
  labs(
    title = "XGBOOST: Feature Importance",
    subtitle = "Model 07: XGBoost Model"
  )


##### New predictions tbl ####
#####
#####
bike_feature_tbl |>
  colnames()

new_over_mountain_jekyll <- tibble(
  model = 'Jekyll A1 1',
  frame_material = "Aluminum",
  category_2 = "Over Mountain",
  base_model = "Jekyll",
  model_tier = "Alumium 1",
  black = 0,
  hi_mod = 0,
  team = 0,
  red = 0,
  ultegra = 0,
  dura_ace = 0,
  disc = 0
)

model_07_xgboost |>
  predict_numeric(new_data = new_over_mountain_jekyll)

# [1] 1814.645



# 6.0 BONUS - PREPROCESSING & SVM-Regression ----

recipe_obj <- recipe(price ~ ., data = training_tbl) |> 
  update_role(id, new_role = "id") |> 
  update_role_requirements(role = "id", bake = FALSE) |> 
  step_rm(id,model,model_tier) |>  # REMOVE THE PROVIDED COLUMN 
  step_dummy(all_nominal(),one_hot = TRUE) |>  # CONVERTED NOMINAL COLUMNS TO ONE HOT 
  step_log(price,skip = TRUE) |>  # CONVERT TO LOG PRICE
  step_center(price,skip = TRUE) |> # NORMALIZE THE COLUMN WITH mean of the columns 
  step_scale(price,skip = TRUE) |> # SCALE THE COLUMN TO APPEAR WITHIN [-1 , 1 ]
  prep()



recipe_obj |> 
  bake(new_data =  training_tbl) |> 
  glimpse()

training_transformed_tbl <- recipe_obj |> 
  bake(new_data =  training_tbl)


testing_transformed_tbl <- recipe_obj |> 
  bake(new_data =  testing_tbl)


recipe_scale <- recipe_obj |> 
  tidy(number = 5)

recipe_center <- recipe_obj |> 
  tidy(number = 4)


model_08_svm_rbf <- svm_rbf(mode = "regression",cost = 30, rbf_sigma = 0.05,margin = 0.4) |> 
  set_engine("kernlab",scaled = FALSE) |> 
  fit(price ~ . , data= training_transformed_tbl)



model_08_svm_rbf |> 
  predict(new_data = testing_transformed_tbl) |> 
  mutate(.pred = .pred * recipe_scale$value,
         .pred = .pred + recipe_center$value,
         .pred = exp(.pred)
         
  ) |> 
  bind_cols(testing_tbl |> select(price)) |> 
  metrics(truth = price, estimate = .pred)

bake(recipe_obj, new_data = new_over_mountain_jekyll) |> 
  predict(object = model_08_svm_rbf) |> 
  mutate(
    .pred = .pred * recipe_scale$value,
    .pred = .pred + recipe_center$value,
    .pred = exp(.pred)
  )


# 8.0 SAVING & LOADING MODELS ----

fs::dir_create("models")

models_tbl <- list(
  "MODEL_01__LM_SIMPLE"  = model01_linear_regression_lm,
  "MODEL_02__LM_COMPLEX" = model02_linear_regression_lm,
  "MODEL_03__GLMNET"     = model03_linear_regression_glm,
  "MODEL_04__DECISION_TREE"   = model_04_decsion_tree_rpart,
  "MODEL_05__RF_RANGER"       = model_05_random_forest_ranger,
  "MODEL_06__RF_RANDOMFOREST" = model_06_random_forest_randomForest,
  "MODEL_07__XGBOOST" = model_07_xgboost,
  "MODEL_08__SVM"     = model_08_svm_rbf
) |> 
  enframe(name = "model_id", value = "model") |> 
  mutate(model = purrr::map(model, bundle::bundle)) 

models_tbl

models_tbl|> write_rds("models/parsnip_models_tbl.rds")

recipes_tbl <- list(
  "RECIPE_01" = recipe_obj
) |> 
  enframe(name = "recipe_id", value = "recipe")

recipes_tbl |>  write_rds("models/recipes_tbl.rds")

calculate_metrics |> write_rds("scripts/calc_metrics.rds")

# Reading

models_tbl <- read_rds("models/parsnip_models_tbl.rds")

recipes_tbl <- read_rds("models/recipes_tbl.rds")

calc_metrics <- read_rds("scripts/calc_metrics.rds")







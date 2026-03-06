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


# 1.0 PROBLEM DEFINITION ----
# - Which Bike Categories are in high demand?
# - Which Bike Categories are under represented?
# - GOAL: Use a pricing algorithm to determine a new product price in a category gap

model_sales_tbl <- bike_orderlines_tbl |>
  select(total_price, model, category_2, frame_material) |>

  group_by(model, category_2, frame_material) |>
  summarise(total_sales = sum(total_price)) |>
  ungroup() |>
  arrange(desc(total_sales))

model_sales_tbl |>
  mutate(category_2 = as_factor(category_2) %>% 
           fct_reorder(total_sales, .fun = max) %>% 
           fct_rev()) |>
  ggplot(aes(frame_material, total_sales)) +
  geom_violin() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
  #coord_flip() +
  facet_wrap(~category_2) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
  theme_tq() +
  labs(
    title = "Total Sales for Each Model",
    x = "Frame Material",
    y = "Revenue"
  )

# 2.0 TRAINING & TEST SETS ----

bike_feature_tbl <- bike_orderlines_tbl |>
  select(price, model, category_2, frame_material) |>
  distinct() |>
  mutate(id = row_number()) |>
  select(id, everything()) |>
  separate_bike_model(append = TRUE, keep_model_column = TRUE)

g1 <- bike_feature_tbl %>%
  mutate(category_2 = as_factor(category_2) %>% 
           fct_reorder(price)) %>%
  
  ggplot(aes(category_2, y = price)) +
  geom_violin() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
  coord_flip() +
  facet_wrap(~ frame_material) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_tq() +
  labs(
    title = "Unit Price for Each Model",
    y = "", x = "Category 2"
  )



set.seed(1113)
split_obj <- initial_split(bike_feature_tbl, prop = 0.80, strata = 'base_model') # 80% of data => Training ; 20% of data ==> Testing
# strata : Random sampling to be conducted within strata column
#

training_tbl <- training(split_obj)
testing_tbl <- testing(split_obj)

# 3.0 LINEAR METHODS ----
#

model01_linear_regression_lm <- linear_reg(mode = 'regression') |>
  set_engine('lm') |>
  fit(formula = price ~ category_2 + frame_material, data = training_tbl)


### 3.0.1  prediction with manual metrics calculation ----
model01_linear_regression_lm |>
  predict(new_data = testing_tbl) |>
  bind_cols(
    testing_tbl |> select(price)
  ) |>
  mutate(residuals = price - .pred) |>
  summarise(
    mae = abs(residuals) |> mean(),
    rmse = mean(residuals^2)^0.5
  )

### 3.0.2 prediction with yardstick packages ----

model01_linear_regression_lm |>
  predict(new_data = testing_tbl) |>
  bind_cols(
    testing_tbl |> select(price)
  ) |>
  metrics(truth = price, estimate = .pred)


### 3.0.3 Feature Importance -----

model01_linear_regression_lm$fit |>
  broom::tidy() |>
  arrange(p.value) |>
  mutate(term = as_factor(term) |> fct_rev()) |>
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = scales::dollar(estimate, accuracy = 1)),
    size = 3
  ) +

  scale_x_continuous(labels = scales::dollar_format()) +
  theme_tq() +
  labs(
    title = 'Linear regression: Feature Importance',
    subtitle = 'Model 01: LM Model'
  )


#### 3.1 Function to calcualte metrics ----

calculate_metrics <- function(model, new_data, y_column) {
  y_column <- enquo(y_column)
  model |>
    predict(new_data = new_data) |>
    bind_cols(new_data |> select(!!y_column)) |>
    metrics(truth = !!y_column, estimate = .pred)
}


model01_linear_regression_lm |>
  calculate_metrics(new_data = testing_tbl, y_column = price)

# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 rmse    standard    1829.
# 2 rsq     standard       0.226
# 3 mae     standard    1423.

#### 3.2. Adding the more feature ----

model02_linear_regression_lm <- linear_reg(mode = 'regression') |>
  set_engine('lm') |>
  fit(
    formula = price ~ .,
    data = training_tbl |> select(-id, -model, -model_tier)
  )


model02_linear_regression_lm |>
  calculate_metrics(new_data = testing_tbl, y_column = price)

# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 rmse    standard    1371.
# 2 rsq     standard       0.620
# 3 mae     standard     989.

model02_linear_regression_lm$fit |>
  broom::tidy() |>
  arrange(p.value) |>
  head(n = 25) |>
  mutate(term = as_factor(term) |> fct_rev()) |>
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = scales::dollar(estimate, accuracy = 1)),
    size = 3
  ) +

  scale_x_continuous(labels = scales::dollar_format()) +
  theme_tq() +
  labs(
    title = 'Linear regression: Top 25 Feature Importance',
    subtitle = 'Model 02: LM Model'
  )


#### 3.3 General Linear models ----

model03_linear_regression_glm <- linear_reg(
  mode = 'regression',
  penalty = 500, ## WE CAN USE HYPERPARAMETER TUNING
  mixture = 0 ## WE CAN USE HYPERPARAMETER TUNING
) |>
  set_engine('glmnet') |>
  fit(
    formula = price ~ .,
    data = training_tbl |> select(-id, -model, -model_tier)
  )


model03_linear_regression_glm |>
  calculate_metrics(new_data = testing_tbl, y_column = price)


# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 rmse    standard    1389.
# 2 rsq     standard       0.612
# 3 mae     standard    1011.

model03_linear_regression_glm$fit |>
  broom::tidy() |>
  filter(dev.ratio == max(dev.ratio)) |>
  arrange(desc(abs(estimate))) |>
  mutate(term = as_factor(term) |> fct_rev()) |>
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = scales::dollar(estimate, accuracy = 1)),
    size = 3
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Linear Regression: Feature Importance",
    subtitle = "Model 03: GLMNET Model"
  )


##### New predictions tbl ####

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

model01_linear_regression_lm |>
  predict_numeric(new_data = new_over_mountain_jekyll)
#[1] 975.6231

model03_linear_regression_glm |>
  predict_numeric(new_data = new_over_mountain_jekyll)

#[1] 2734.413
#
#
#

model_tbl <- tibble(
   model_id = str_c("model 0",1:7),
  model = list(
   model01_linear_regression_lm,
   model02_linear_regression_lm,
   model03_linear_regression_glm,
   model_04_decsion_tree_rpart,
   model_05_random_forest_ranger,
  model_06_random_forest_randomForest,
    model_07_xgboost
)
 )

predictions_new_over_mountain_jekyll <- model_tbl |> 
  mutate(predictions = map(model ,predict,new_data = new_over_mountain_jekyll)) |> 
  unnest(predictions) |> 
  mutate(category_2 = "Over Mountain",
         frame_material = "Aluminum"
         
         )



g1 + 
geom_point(aes(category_2, .pred, color = model_id), alpha = 0.5,
           data = predictions_new_over_mountain_jekyll) +
  ggrepel::geom_text_repel(aes(label = model_id, y = .pred),
                           size = 3,
                           data = predictions_new_over_mountain_jekyll)


##### New Triathalon_slice_ tbl ####

new_triathalon_slice_tble <- tibble(
  model = 'Slice A1 1',
  frame_material = "Aluminum",
  category_2 = "Triathalon",
  base_model = "Slice",
  model_tier = "Ultegra",
  black = 0,
  hi_mod = 0,
  team = 0,
  red = 0,
  ultegra = 0,
  dura_ace = 0,
  disc = 0
)


predictions_new_triathalon_slice <- model_tbl |> 
  mutate(predictions = map(model ,predict,new_data = new_triathalon_slice_tble)) |> 
  unnest(predictions) |> 
  mutate(category_2 = "Triathalon",
         frame_material = "Aluminum"
         
  )


g1 + 
  geom_point(aes(category_2, .pred, color = model_id), alpha = 0.5,
             data = predictions_new_over_mountain_jekyll) +
  ggrepel::geom_text_repel(aes(label = model_id, y = .pred),
                           size = 3,
                           data = predictions_new_over_mountain_jekyll)+
  geom_point(aes(category_2, .pred, color = model_id), alpha = 0.5,
             data = predictions_new_triathalon_slice) + 
  ggrepel::geom_text_repel(aes(label = model_id, y = .pred),
                           size = 3,
                           data = predictions_new_triathalon_slice)




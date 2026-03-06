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

# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)
library(fs)

library(logger)

log_threshold(DEBUG)

bike_orderlines_tbl <- read_rds(
  'data/bike_sales/data_wrangled/bike_order_line.rds'
)

glimpse(bike_orderlines_tbl)


# 1.0 PRIMER ON PURRR ----
# Programmatically getting Excel files into R
excel_paths <- dir_info('data/bike_sales/data_raw/') |>
  pull(path)


# What Not To Do: Don't use for loops
#
#
excel_list <- list()

for (path in excel_paths) {
  excel_list[[path]] <- read_excel(path)
}

excel_list

# What to Do: Use map()
#

# ?map ==> Loops through atomic variables and executes functions
# There are three ways to use map
# 1. direct function without any additional args
# 2. anonymous function
# 3. function defined by function()
#

# Method 1
excel_paths |>
  map(.f = read_excel) |>
  set_names(excel_paths)

# Method 2
excel_paths |>
  map(.f = ~ read_excel(.)) |>
  set_names(excel_paths)


# Method 3
excel_paths |>
  map(.f = function(x) read_excel(x)) |>
  set_names(excel_paths)

#
#

# Reading Excel Sheets

# 2.0 MAPPING DATA FRAMES ----

# 2.1 Column-wise Map ----

# Character
bike_orderlines_tbl |>
  map_chr(~ class(.)[1])


# Dataframe
bike_orderlines_tbl |>
  map_df(~ class(.)[1]) |>
  gather()


bike_orderlines_tbl |>
  map_df(~ sum(is.na(.)) / length(.)) |>
  gather()


# 2.3 Row-wise Map ----
#
# excel_paths
excel_tables_tbl <- dir_info('data/bike_sales/data_raw/') |>
  select(path) |>
  mutate(data = path |> map(read_excel))


excel_tables_tbl


# 3.0 NESTED DATA ----

# Unnest

excel_tables_tbl$data[[1]]

excel_tables_tbl$data[[2]]

excel_tables_tbl$data[[3]]


excel_unnested_tbl <- excel_tables_tbl |>
  unnest(data)


# 15,771 × 15 ==> [97 × 4] + [30 × 3]  + [15,644 × 7]

# Nest

excel_nested_tbl <- excel_unnested_tbl |>
  group_by(path) |>
  nest()

excel_nested_tbl$data[[1]]
## Contains NA COLUMNS

# Mapping Nested List Columns
#

excel_nested_tbl$data[[1]] |>
  select_if(~ !is.na(.) |> all()) # REMOVE THE COLUMN THAT ARE NA


# TO APPLY TO ALL NESTED DATA

select_non_na_columns <- function(data) {
  data |>
    select_if(~ !is.na(.) |> all())
}

excel_nested_tbl$data[[2]] |>
  select_non_na_columns()

# APPLY FUNCTION TO ALL NESTED DATA

excel_nested_fixed_tbl <- excel_nested_tbl |>
  mutate(data_fixed = data |> map(select_non_na_columns))


# 4.0 MODELING WITH PURRR ----

# 4.1 Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
#
rolling_avg_3_tbl <- bike_orderlines_tbl |>
  select(order_date, category_1, category_2, total_price) |>

  mutate(order_date = ymd(order_date)) |>
  mutate(
    month_end = ceiling_date(order_date, unit = "month") -
      period(1, unit = "days")
  ) |>

  group_by(category_1, category_2, month_end) |>
  summarise(
    total_price = sum(total_price)
  ) |>
  mutate(
    rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")
  ) |>
  ungroup() |>

  mutate(
    category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)
  )

rolling_avg_3_tbl |>
  ggplot(aes(month_end, total_price, color = category_2)) +

  # Geometries
  geom_point() +
  geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
  facet_wrap(~category_2, scales = "free_y") +

  # Add Loess Smoother
  geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +

  # Formatting
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))


# 4.2 Modeling Primer ----

# Data Preparation
#

cross_country_race_tbl <- rolling_avg_3_tbl |>
  filter(category_2 == 'Cross Country Race') |>

  select(month_end, total_price) |>
  mutate(month_end_num = as.numeric(month_end))


cross_country_race_tbl |>
  ggplot(aes(x = month_end_num, y = total_price)) +
  geom_point() +
  geom_smooth(method = 'loess', span = .2, se = FALSE)

# Making a loess model
fit_loess_cross_country_model <- cross_country_race_tbl |>
  loess(formula = total_price ~ month_end_num, data = _, span = 0.1)

fit_loess_cross_country_model |> View()

# Working With Broom

fit_loess_cross_country_model |>
  broom::augment()


# Visualizing results

fit_loess_cross_country_model |>
  broom::augment() |>
  ggplot(aes(x = month_end_num, y = total_price)) +
  geom_point() +
  geom_line(aes(y = .fitted), color = 'blue')


# 4.3 Function To Return Fitted Results ----
#

rolling_avg_3_nested_tbl <- rolling_avg_3_tbl |>
  group_by(category_1, category_2) |>
  nest()

tidy_loess <- function(data, span = 0.2, degree = 1) {
  data_formatted <- data |>
    select(month_end, total_price) |>
    mutate(month_end_num = as.numeric(month_end))

  log_info(str_glue(
    'Creating the loess model for {count(data_formatted)} data '
  ))

  fit_loess <- loess(
    formula = total_price ~ month_end_num,
    data = data_formatted,
    span = span,
    degree = degree
  )

  log_info('Fitted the Loess Model')

  output_tbl <- fit_loess |>
    broom::augment() |>
    select(.fitted)

  return(output_tbl)
}


# 4.4 Test Function on Single Element ----
# 
rolling_avg_3_nested_tbl$data[[2]] |>
  tidy_loess()


# 4.5 Map Function to All Categories ----


# Map Functions


loess_nested_tbl <- rolling_avg_3_nested_tbl |> 
    mutate(fitted = data |> map(~tidy_loess(.,span = 0.1)))


loess_nested_tbl |> 
    unnest()

# Visualize Results

loess_nested_tbl |> 
    unnest(cols = c(data,fitted)) |> 
    ggplot(aes(x = month_end, y = total_price,color = category_2)) +
    geom_point() +
    geom_line(aes(y = .fitted), color = 'blue') + 
    facet_wrap(~category_2, ncol = 3 ,scales = 'free_y') + 
    theme_tq()+
    scale_color_tq()

# 0.0 Load libraries ----

library(tidyverse)
library(readxl)

raw_folder_path <- fs::fs_path('data/bike_sales/data_raw/')
wrangled_folder_path <- fs::fs_path('data/bike_sales/data_wrangled/')

# 0.1 Reading the data ----
bikes_tbl <- read_xlsx(raw_folder_path / 'bikes.xlsx')
order_lines_tbl <- read_xlsx(raw_folder_path / 'orderlines.xlsx')
bike_order_line_tbl <- read_rds(wrangled_folder_path / 'bike_order_line.rds')

bike_order_line_tbl

# 1.0 Selecting the column ----

bike_order_line_tbl |>
  select(order_date, order_id, order_line)

bike_order_line_tbl |>
  select(starts_with('order'))

## Rearranging columns

bike_order_line_tbl |>
  select(bikeshop_name:state, everything())


bike_order_line_tbl |>
  select(ends_with('price'))


# Pull
# Pull - to get the vector values from data.frame values

bike_order_line_tbl |>
  pull(total_price) |>
  mean()


bike_order_line_tbl |>
  distinct(model) |>
  pull()

# Select_if - select the certain data types
#
#

bike_order_line_tbl |>
  select_if(is.character)


bike_order_line_tbl |>
  select_if(is.numeric)


bike_order_line_tbl |>
  select_if(~ !is.numeric(.))


# 2.0 Arrange ----
#  Arrange the data frame in ascending or descending

bikes_tbl |>
  select(model, price) |>
  arrange(desc(price)) |>
  View()


# 3.0 Filtering ----
## 3.1 filter() ----
#

bikes_tbl |>
  select(model, price) |>
  filter(price > mean(price))


bikes_tbl |>
  select(model, price) |>
  filter((price > 5000) | (price < 1000)) |>
  arrange(desc(price))


bikes_tbl |>
  select(model, price) |>
  filter(price > 6000, model |> str_detect('Supersix'))


bike_order_line_tbl |>
  filter(category_2 %in% c('Over Mountain', 'Trail', 'Endurance Road'))


bike_order_line_tbl |>
  filter(category_2 == "Over Mountain")


bike_order_line_tbl |>
  filter(category_2 != "Over Mountain")


bike_order_line_tbl |>
  filter(!category_2 %in% c('Over Mountain', 'Trail', 'Endurance Road'))

## 3.2 Slice ----
##

bikes_tbl |>
  arrange(desc(price)) |>
  slice((n() - 9):n())

## 3.3 Distinct ----

bike_order_line_tbl |>
  distinct(category_1, category_2)


bike_order_line_tbl |>
  distinct(city, state)

# 4.0 Mutate ----

# Adding the new column
bike_order_price_tbl <- bike_order_line_tbl |>
  select(order_date, model, quantity, price) |>
  mutate(total_price = quantity * price)


# modify the column with transformation
bike_order_price_tbl |>
  mutate(
    total_price_log = total_price |> log(),
    total_price_sqrt = total_price^0.5
  )


# creating the flag
#

bike_order_price_tbl |>
  mutate(
    is_super_six_model = model |> str_to_lower() |> str_detect('supersix')
  ) |>
  filter(is_super_six_model)


# bnning with ntile
# ntile() is a sort of very rough rank, which breaks the input vector into n buckets.

bike_order_price_tbl |>
  mutate(total_price_binned = ntile(total_price, 3)) |>
  mutate(
    total_price_cat = case_when(
      total_price > quantile(total_price, 0.75) ~ "High",
      total_price > quantile(total_price, 0.25) ~ "Medium",
      TRUE ~ "low"
    )
  )


bike_order_price_tbl |>
  mutate(
    bike_type = case_when(
      model |> str_to_lower() |> str_detect('supersix') ~ 'SuperSix',
      model |> str_to_lower() |> str_detect('jekyll') ~ 'Jekyll',
      TRUE ~ 'Neither SuperSix nor Jekyll'
    )
  )

# 5.0 Aggregate Data ----

bike_order_line_tbl |>
  summarize(revenue = sum(total_price))


bike_order_line_tbl |>
  group_by(category_1, category_2, frame_material) |>
  summarize(revenue = sum(total_price), .groups = 'keep') |>
  ungroup() |>
  arrange(desc(revenue))


# More summary Functions

bike_order_line_tbl |>
  group_by(category_1, category_2) |>
  summarize(
    count = n(),
    avg_total_price = total_price |> mean(na.rm = TRUE),
    median_total_price = total_price |> median(na.rm = TRUE), ## 50th percentile
    sd_total_price = total_price |> sd(),
    min_total_price = total_price |> min(),
    max_total_price = total_price |> max()
  ) |>
  ungroup() |>
  arrange(desc(count))


# Summarize all  - detect missing values

bike_order_missing_tbl <- bike_order_line_tbl |>
  mutate(total_price = c(rep(NA, 4), total_price[5:15644]))


bike_order_missing_tbl |>
  summarize_all(~ sum(is.na(.)) / length(.)) |>
  glimpse()


bike_order_missing_tbl |>
  filter(!is.na(total_price))

# 6.0 Renaming data ----
##  6.1 rename one time at a time  ----
##

bike_shop_revenue_tbl <- bike_order_line_tbl |>
  select(bikeshop_name, category_1, total_price) |>
  group_by(bikeshop_name, category_1) |>
  summarize(sales = sum(total_price)) |>
  ungroup()


bike_shop_revenue_tbl |>
  rename(
    `Bike Shop Name` = bikeshop_name,
    `Primary Category` = category_1,
    Sales = sales
  )

##  6.2 set_names ----
##
##
bike_shop_revenue_tbl |>
  set_names(c("Bike Shop Name", "Primary Category", 'Sales'))


bike_shop_revenue_tbl |>
  rename_with(~ str_to_sentence(gsub("_", " ", .x, fixed = TRUE)))


# 7.0 Reshaping ----
##  7.1 Spread ----
##  long to wider
##
bike_shop_revenue_formatted_tbl <- bike_shop_revenue_tbl |>
  spread(key = category_1, value = sales) |>
  arrange(desc(Mountain)) |>
  rename_with(~ str_to_sentence(gsub("_", " ", .x, fixed = TRUE))) |>
  mutate(Mountain = scales::dollar(Mountain), Road = scales::dollar(Road))


## 7.2 Gather ----
## wider to long
##

bike_shop_revenue_formatted_tbl |>
  gather(key = 'category_1', value = 'sales', Mountain, Road) |>
  ## gather(key = 'category_1',value = 'sales', - `Bikes hop name`) ##
  mutate(sales = sales |> str_remove_all("\\$|,") |> as.double())

# 8.0 Joining / Combining ----
##  8.1 Joining ----
#
#

order_lines_tbl <- order_lines_tbl |>
  janitor::clean_names() |>
  select(-x1)


bikes_tbl <- bikes_tbl |>
  janitor::clean_names()


order_lines_tbl |>
  left_join(bikes_tbl, by = c("product_id" = "bike_id"))


## 8.2 Combine ----
### 8.2.1 Column ----
bike_order_line_tbl |>
  select(-contains('order')) |>
  bind_cols(bike_order_line_tbl |> select(order_id)) |>
  select(order_id, everything())


### 8.2.2 Row ----
###
train_tbl <- bike_order_line_tbl |>
  slice(1:7500)

test_tbl <- bike_order_line_tbl |>
  slice(7501:15644)


train_tbl |>
  bind_rows(test_tbl)

# 9.0 Text wrangling ----
## 9.1 Separate ----

bike_order_line_tbl |>
  select(order_date) |>
  mutate(order_date = order_date |> as.character()) |>
  # Separate
  separate(
    col = order_date,
    into = c('year', 'month', 'date'),
    sep = '-',
    remove = FALSE
  ) |>
  mutate(
    year = year |> as.numeric(),
    month = month |> as.numeric(),
    date = month |> as.numeric()
  ) |>

  # unite
  #
  unite(order_date_united, year, month, date, sep = '-', remove = FALSE) |>
  mutate(order_date_united = order_date_united |> as_date())

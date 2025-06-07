# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)
library(fs)
library(janitor)
library(skimr)
library(scales)
# theme_tq()
library(tidyquant)
library(ggrepel)

# Excel Files
library(readxl)
library(writexl)

tidyquant_conflicts()

# 2.0 Importing Files ----

raw_data_path <- fs_path("data/bike_sales/data_raw/")


# Reading the xlsx each file
# removing any spaces using janitor clean names

bikes_tbl <- read_excel(path = raw_data_path + "/bikes.xlsx") |>
  clean_names()

bikeshop_tbl <- read_excel(path = raw_data_path + "/bikeshops.xlsx") |>
  clean_names()

orderlines_tbl <- read_excel(path = raw_data_path + "/orderlines.xlsx") |>
  clean_names() |> #
  rename(index = x1)

# 3.0 Examining Data ----

bikes_tbl |>
  glimpse()

# bikes_tbl |>
#   skim()
#
# orderlines_tbl |>
#   skim()

# 4.0 Joining Data ----

bikes_tbl <- bikes_tbl |>
  mutate(bike_id = as_factor(bike_id))

orderlines_tbl <- orderlines_tbl |>
  mutate(
    product_id = as_factor(product_id),
    customer_id = as_factor(customer_id),
    order_id = as_factor(order_id)
  )


bikeshop_tbl <- bikeshop_tbl |>
  mutate(bikeshop_id = as_factor(bikeshop_id))


bike_orderlines_joined_tbl <- orderlines_tbl |>
  left_join(bikes_tbl, by = c("product_id" = "bike_id")) |>
  left_join(bikeshop_tbl, by = c("customer_id" = "bikeshop_id"))


bike_orderlines_joined_tbl |> glimpse()

# 5.0 Wrangling Data ----

bike_ordelines_wrangled_tbl <- bike_orderlines_joined_tbl |>

  # Separate the description into category1, category2, and frame material
  # and removing the description columns

  separate(
    description,
    into = c("category_1", "category_2", "frame_material"),
    sep = " - ",
    remove = TRUE
  ) |>

  # Separate the location into city and state
  # keeping the locations

  separate(location, into = c("city", "state"), sep = ", ", remove = FALSE) |>

  # creating the total prices = quality * price

  mutate(total_price = quantity * price) |>

  # Removing unnecessary columns
  select(-index, -ends_with('id')) |>
  select(-location) |>
  bind_cols(orderlines_tbl |> select(order_id)) |>

  # Reorder
  select(
    contains('date'),
    contains('id'),
    contains("order"),
    quantity,
    price,
    total_price,
    everything()
  )

# 6.0 Data visualisation ----
#
## 6.1 Sales by year -----

sales_by_year_tbl <- bike_ordelines_wrangled_tbl |>
  # Selecting the columns to focus on

  select(order_date, total_price) |>
  # Mutate - Get year from order year

  mutate(order_year = order_date |> year()) |>

  group_by(order_year) |>
  summarize(
    sales = sum(total_price)
  ) |>
  ungroup() |>
  mutate(sales_text = dollar(sales))


sales_by_year_tbl |>
  # Adding the ggplot
  ggplot() +
  # Mapping the aesthetics to x-axis and y-axis
  aes(x = order_year, y = sales) +

  # Geometrics
  geom_col(fill = '#2C3E50') +
  geom_label_repel(aes(label = sales_text)) +
  geom_smooth(method = 'lm', se = FALSE) +

  # Formatting
  theme_tq() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = 'Revenue by year',
    subtitle = 'There is upward trend',
    x = '',
    y = 'Revenue'
  )


## 6.2 Sales by year , category_2 -----
##

sales_by_year_cat2_tbl <- bike_ordelines_wrangled_tbl |>
  # Selecting the columns to focus on

  select(order_date, category_2, total_price) |>
  # Mutate - Get year from order year

  mutate(order_year = order_date |> year()) |>

  group_by(order_year, category_2) |>
  summarize(
    sales = sum(total_price),
    .groups = "drop"
  ) |>
  mutate(sales_text = dollar(sales))


sales_by_year_cat2_tbl |>
  ggplot() +
  aes(x = order_year, y = sales, fill = category_2) +
  geom_col() +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~category_2) +

  # Formatting
  theme_tq() +
  scale_fill_tq() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = 'Revenue by year and Category 2',
    x = '',
    y = 'Revenue',
    fill = 'Product Secondary Category'
  )


# 7.0 Writing the files ----
data_wrangled_path = 'data/bike_sales/data_wrangled'
dir_create(data_wrangled_path)

##  7.1 Excel ----
#
bike_ordelines_wrangled_tbl |>
  write_xlsx(str_c(data_wrangled_path, "/bike_order_line.xlsx"))

## CSV ----
##
bike_ordelines_wrangled_tbl |>
  write_csv(str_c(data_wrangled_path, "/bike_order_line.csv"))


## RDS ----
bike_ordelines_wrangled_tbl |>
  write_rds(str_c(data_wrangled_path, "/bike_order_line.rds"))

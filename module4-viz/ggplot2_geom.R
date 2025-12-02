# Types of Graphs: ggplot2 Geometries ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds(
  "data/bike_sales/data_wrangled/bike_order_line.rds"
)

glimpse(bike_orderlines_tbl)

# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation

order_value_tbl <- bike_orderlines_tbl |>
  select(order_id, order_line, total_price, quantity) |>
  group_by(order_id) |>

  summarize(total_quantity = sum(quantity), total_price = sum(total_price)) |>
  ungroup()


# Scatter Plot

order_value_tbl |>
  # canvas
  ggplot(aes(x = total_quantity, y = total_price)) +

  geom_point(alpha = 0.5, size = 2) + ## Scatterplot

  geom_smooth(method = 'lm', se = F)

# 2.0 Line Plots ----
# - Great for time series

# Goal: Describe revenue by Month, expose cyclic nature

# Data Manipulation
#
#
monthly_revenue_tbl <- bike_orderlines_tbl |>
  select(order_date, total_price) |>
  mutate(year_month = floor_date(order_date, "months") |> ymd()) |>
  group_by(year_month) |>

  summarise(revenue = sum(total_price)) |>
  ungroup()

# Line Plot
#
#

monthly_revenue_tbl |>
  ggplot(aes(x = year_month, y = revenue)) +

  geom_line(linewidth = 0.5, linetype = 1) +

  geom_smooth(span = 0.2)

# 3.0 Bar / Column Plots ----
#- Great for categories

# Goal: Sales by Descriptive Category

# Data Manipulation

sales_by_cat2_tbl <- bike_orderlines_tbl |>
  select(category_2, total_price) |>
  group_by(category_2) |>

  summarise(revenue = sum(total_price)) |>
  ungroup()

# Bar Plot

sales_by_cat2_tbl |>

  mutate(category_2 = category_2 |> as_factor() |> fct_reorder((revenue))) |>

  ggplot(aes(x = category_2, y = revenue)) +
  geom_col(fill = '#2C3e50') +

  coord_flip() +
  theme_tq()

# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable

# Goal: Unit price of bicycles
# Histogram

bike_orderlines_tbl |>
  select(model, price) |>
  distinct() |>

  ggplot(aes(price)) +
  geom_histogram(bins = 20, fill = 'blue', color = 'white')

# Goal: Unit price of bicylce, segmenting by frame material
# Histogram

bike_orderlines_tbl |>
  select(model, frame_material, price) |>
  distinct() |>
  ggplot(aes(price, fill = frame_material)) +
  geom_histogram(bins = 20, color = 'white') +
  facet_wrap(~frame_material, ncol = 1) +
  scale_fill_tq() +
  theme_tq()


# Density
#
bike_orderlines_tbl |>
  select(model, frame_material, price) |>
  distinct() |>
  ggplot(aes(price, fill = frame_material)) +
  geom_density(alpha = 0.5) +
  scale_fill_tq() +
  theme_tq()


# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions

# Goal: Unit price of models, segmenting by category 2

# Data Manipulation

sales_price_by_cat2_tbl <- bike_orderlines_tbl |>
  select(category_2, model, price) |>
  distinct() |>
  mutate(category_2 = category_2 |> as_factor() |> fct_reorder((price)))


# Box Plot

sales_price_by_cat2_tbl |>

  ggplot(aes(x = category_2, y = price)) +

  geom_boxplot() +

  coord_flip() +
  theme_tq()

# Violin Plot & Jitter Plot

sales_price_by_cat2_tbl |>
  ggplot(aes(x = category_2, y = price)) +

  geom_violin() +
  geom_jitter(width = 0.2, color = '#2c3e50') +

  coord_flip() +

  theme_tq()

# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation
#

yearly_revenue_tbl <- bike_orderlines_tbl |>
  select(order_date, total_price) |>
  mutate(year = year(order_date)) |>
  group_by(year) |>
  summarise(revenue = sum(total_price)) |>
  ungroup()


# Adding text to bar chart

# Filtering labels to highlight a point

yearly_revenue_tbl |>
  ggplot(aes(x = year, y = revenue)) +
  geom_col(fill = '#2c3e50') +
  geom_text(
    aes(label = scales::dollar(revenue, scale = 1e-6, suffix = "M")),
    vjust = 1.5,
    color = 'white'
  ) +

  geom_label(
    label = "Major demand this year",
    vjust = -0.5,
    size = 5,
    fill = '#1f78b4',
    color = 'white',
    fontface = 'italic',
    data = yearly_revenue_tbl |> filter(year == 2013)
  ) +

  expand_limits(y = 0) +

  theme_tq()

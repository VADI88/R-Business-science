# 0.0 importing library ----
#

library(tidyverse)
library(tidyquant)

bike_orderlines_tbl <- read_rds(
  "data/bike_sales/data_wrangled/bike_order_line.rds"
)

bike_orderlines_tbl


# 1.0 Factor Basics ----

# What is a Factor?
# A way of managing categorical data

# Why do we want factors?
# 1. Can group numeric values into bin (think price = low, medium, high)
# 2. Can reorder categories for visualization (fct_reorder)
# 3. Can manipulate categories much eaiser (fct_lump)
# 4. Machine learning and modeling algorithms may require factor data type for categorical data.

# 2.0 Motivation
#
sales_by_cat2_tbl <- bike_orderlines_tbl |>

  select(category_2, total_price) |>
  group_by(category_2) |>

  summarize(revenue = sum(total_price)) |>
  ungroup() |>
  arrange(desc(revenue)) |>
  mutate(category_2 = category_2 |> as_factor() |> fct_rev()) # Reverse order


sales_by_cat2_tbl |>
  ggplot(aes(y = category_2, x = revenue, group = 1)) +
  geom_point(size = 10, color = '#2C3E50') +
  geom_segment(
    aes(x = 0, xend = revenue, y = category_2, yend = category_2),
    linetype = 'dashed'
  ) +
  geom_text(
    aes(
      label = revenue |> scales::dollar(scale = 1 / 1e6, suffix = "M"),
      x = revenue * 1.05
    ),
    color = 'black'
  ) +
  scale_x_continuous(label = scales::dollar_format()) +
  theme_tq() +
  expand_limits(x = 0) +
  labs(
    x = 'Category 2',
    y = 'Revenue',
    title = 'Sales by Category 2'
  )


# 3.1 Inscepting factors ----
#

sales_by_cat2_tbl |> pull(category_2) |> levels()


sales_by_cat2_tbl |> pull(category_2) |> as.numeric()


sales_by_cat2_tbl |>
  mutate(
    label = category_2 |> as.character(),
    value = category_2 |> as.numeric()
  )

# 3.2 as_factor  vs as.factor -----
#

sales_by_cat2_tbl |>
  mutate(
    category_2 = category_2 |> as.character(),
    category_2_as_factor = category_2 |> as_factor() |> as.numeric(), ## Provide factor as per values
    category_2_as.factor = category_2 |> as.factor() |> as.numeric() ### Still provide A-Z or Z-A
  )


sales_by_cat2_tbl |>
  mutate(negative_sales = -1 * revenue) |>
  mutate(category_2 = category_2 |> fct_reorder(negative_sales)) |> ## Reorder by particular columns
  ggplot(aes(y = category_2, x = revenue, group = 1)) +
  geom_point(size = 10, color = '#2C3E50') +
  geom_segment(
    aes(x = 0, xend = revenue, y = category_2, yend = category_2),
    linetype = 'dashed'
  ) +
  geom_text(
    aes(
      label = revenue |> scales::dollar(scale = 1 / 1e6, suffix = "M"),
      x = revenue * 1.05
    ),
    color = 'black'
  ) +
  scale_x_continuous(label = scales::dollar_format()) +
  theme_tq() +
  expand_limits(x = 0) +
  labs(
    x = 'Category 2',
    y = 'Revenue',
    title = 'Sales by Category 2'
  )


# 3.3 Fct reorder 2  -----
#
#
#
sales_by_cate2_q_tbl <- bike_orderlines_tbl |>
  select(order_date, category_2, total_price) |>
  mutate(order_date = order_date |> floor_date("quarter") |> ymd()) |>

  group_by(category_2, order_date) |>

  summarize(revenue = sum(total_price)) |>
  ungroup()


sales_by_cate2_q_tbl |>
  mutate(category_2 = category_2 |> fct_reorder2(order_date, revenue)) |>
  ggplot(aes(y = revenue, x = order_date, color = category_2)) +
  geom_line(linetype = 'dashed') +
  geom_point() +
  facet_wrap(~category_2) +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M")
  ) +
  labs(
    x = 'Category 2',
    y = 'Revenue',
    title = 'Sales by Category 2'
  )

# 3.4 fct_lump ----
#
#

sales_by_cat2_tbl |>
  mutate(
    category_2 = category_2 |>
      fct_lump(n = 6, w = revenue, other_level = 'all other bike category')
  ) |>
  group_by(category_2) |>
  summarize(revenue = sum(revenue)) |>
  ungroup() |>
  mutate(
    category_2 = category_2 |> fct_relevel("all other bike category", after = 0)
  ) |>
  ggplot(aes(y = category_2, x = revenue, group = 1)) +
  geom_point(size = 10, color = '#2C3E50') +
  geom_segment(
    aes(x = 0, xend = revenue, y = category_2, yend = category_2),
    linetype = 'dashed'
  ) +
  geom_text(
    aes(
      label = revenue |> scales::dollar(scale = 1 / 1e6, suffix = "M"),
      x = revenue * 1.05
    ),
    color = 'black'
  ) +
  scale_x_continuous(label = scales::dollar_format()) +
  theme_tq() +
  expand_limits(x = 0) +
  labs(
    x = 'Category 2',
    y = 'Revenue',
    title = 'Sales by Category 2'
  )

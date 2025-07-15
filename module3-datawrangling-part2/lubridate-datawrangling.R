# 0.0 importing library ----
#

library(tidyverse)
library(lubridate)
library(tidyquant)
library(fs)

wrangled_data_path <- fs_path('data/bike_sales/data_wrangled')

bike_orderlines_tbl <- read_rds(wrangled_data_path / 'bike_order_line.rds')
bike_orderlines_tbl |>
  glimpse()

# 1.0 Date & Lubridate Basics ----

bike_orderlines_tbl |>
  select(order_date) |>
  pull() |>
  class()

# "POSIXct" "POSIXt"

# 1.1 YMD vs YMD_HMS

bike_orderlines_tbl |>
  select(order_date) |>
  mutate(
    order_date_chr = as.character(order_date),
    order_date_chr_2 = order_date_chr |> str_c(" 00:00:00")
  ) |>
  mutate(order_date_date = order_date_chr |> ymd()) |>
  mutate(order_date_dtm = order_date_chr_2 |> ymd_hms())


# 1.2  lubridate functions
# 1.2.1 Conversion

"06/01/25" |> mdy()

"06/01/25 06:30:30" |> mdy_hms()


"January 1 1985" |> mdy()

#ymd vs dmy ..

# 1.2.2 Extractor
# year , month, day

"2025-03-15" |> ymd() |> year()

"2025-03-15" |> ymd() |> month(label = T)
#[1] Mar

"2025-03-15" |> ymd() |> month(label = F)
# 3

"2025-03-15" |> ymd() |> wday(label = TRUE, abbr = TRUE)

#  Sat
#

# 1.3 helpers
#

now()
# [1] "2025-06-23 18:21:49 CEST"
#
today()
# [1] "2025-06-23"

# 1.4 Periods & Duration
#

today() + days(12)

today() + ddays(12)


# 1.5 Intervals
#

interval(today(), today() + ddays(12)) / ddays(1) # interval in days

interval(today(), today() + ddays(12)) / dminutes(1) # interval in minute


bike_orderlines_tbl |>
  select(order_date) |>
  mutate(
    today_date = today(),
    days_interval = interval(order_date, today_date) / ddays(1)
  )

# 2.0  Time Series Aggregation ----
#

bike_sales_yearly_tbl <- bike_orderlines_tbl |>
  select(order_date, total_price) |>
  # lubridate
  mutate(order_date = ymd(order_date)) |>
  mutate(order_year = order_date |> year()) |>

  # dplyr
  group_by(order_year) |>
  summarize(sales = sum(total_price)) |>
  ungroup()

bike_sales_yearly_tbl


bike_sales_monthly_tbl <- bike_orderlines_tbl |>
  select(order_date, total_price) |>
  # lubridate
  mutate(order_date = ymd(order_date)) |>
  mutate(
    order_year = order_date |> year(),
    order_month = order_date |> month(label = TRUE, abbr = TRUE)
  ) |>
  # dplyr
  group_by(order_year, order_month) |>
  summarize(sales = sum(total_price)) |>
  ungroup()

bike_sales_monthly_tbl

# 2.1  floor date
#

bike_orderlines_tbl |>
  select(order_date, total_price) |>
  mutate(order_date = ymd(order_date)) |>
  mutate(order_year_month = floor_date(order_date, unit = 'month')) |>
  group_by(order_year_month) |>
  summarize(sales = sum(total_price)) |>
  ungroup()

# 3.0 Measuring Change ----
# 3.1 Lag
bike_sales_yearly_tbl |>
  mutate(sales_lag_1 = lag(sales, 1)) |>
  mutate(
    sales_lag_1 = case_when(
      is.na(sales_lag_1) ~ sales,
      TRUE ~ sales_lag_1
    )
  ) |>
  mutate(yoy = (sales - sales_lag_1) / sales_lag_1) |>
  mutate(yoy_pct = scales::percent(yoy))


calculate_pct_diff <- function(data) {
  data |>
    mutate(sales_lag_1 = lag(sales, 1)) |>
    mutate(
      sales_lag_1 = case_when(
        is.na(sales_lag_1) ~ sales,
        TRUE ~ sales_lag_1
      )
    ) |>
    mutate(pct_diff = (sales - sales_lag_1) / sales_lag_1) |>
    mutate(pct_diff_text = scales::percent(pct_diff))
}

bike_sales_monthly_tbl |>
  calculate_pct_diff()

# 3.2 first

bike_sales_yearly_tbl |>
  mutate(sales_2011 = first(sales)) |>
  mutate(sales_increase_since_2011 = (sales - sales_2011) / sales_2011) |>
  mutate(
    sales_increase_since_2011_pct = scales::percent(sales_increase_since_2011)
  )


bike_sales_monthly_tbl |>
  group_by(order_year) |>
  mutate(sales_jan = first(sales)) |>
  mutate(sales_increase_since_jan = (sales - sales_jan) / sales_jan) |>
  mutate(
    sales_increase_since_jan_pct = scales::percent(sales_increase_since_jan)
  ) |>
  ungroup()


# 3.3 Cummulative
#
#

bike_sales_yearly_tbl |>
  mutate(cummulative_sales = sales |> cumsum())


bike_sales_yearly_tbl |>
  mutate(cummulative_sales_avg = sales |> cummean())


bike_sales_monthly_tbl |>
  group_by(order_year) |>
  mutate(cummulative_sales = sales |> cumsum())


bike_sales_yearly_tbl |>
  mutate(cummulative_sales = sales |> cumsum()) |>
  mutate(cummulative_sales_pct = cummulative_sales / sum(sales))


bike_sales_monthly_tbl |>
  group_by(order_year) |>
  mutate(cummulative_sales = sales |> cumsum()) |>
  mutate(cummulative_sales_pct = cummulative_sales / sum(sales)) |>
  mutate(cummulative_sales_pct_chr = cummulative_sales_pct |> scales::percent())

# 4.0 Rollmean ----
# moving average / moving medians

bike_sales_monthly_tbl |>
  # rollmean(x:vector, k:Number of duration, fill = 0 or NA, align = 'right' will correct )
  mutate(roll_mean_3M = rollmean(sales, k = 3, align = 'right', fill = 0)) |>
  mutate(roll_mean_6M = rollmean(sales, k = 6, align = 'right', fill = 0))


# 5.0 filter
#
#

bike_orderlines_tbl |>
  mutate(order_date = ymd(order_date)) |>
  filter(
    order_date |> between(left = ymd('2012-01-01'), right = ymd('2013-01-01'))
  ) |>
  tail()

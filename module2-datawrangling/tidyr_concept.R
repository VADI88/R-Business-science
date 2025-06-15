# Understanding the concept of tidyr
#
#
#

library(tidyverse)
library(readxl)

# Reading the file
#
#
bike_shop_revenue_wide_tbl <- read_excel(
  'data/bikeshop_revenue_formatted_wide.xlsx'
)


bike_shop_revenue_wide_tbl

# Convert the long format

bike_shop_revenue_long_tbl <- bike_shop_revenue_wide_tbl |>
  select(-Total) |>
  gather(key = 'category_1', value = 'sales', Mountain, Road)


bike_shop_revenue_long_tbl

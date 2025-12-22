# FUNCTIONAL PROGRAMMING ----

library(tidyverse)
library(lubridate)
library(tidyquant)
library(ggrepel)
library(fs)
library(rlang)
library(ggrepel)


library(logger)

log_threshold(DEBUG)

bike_orderlines_tbl <- read_rds(
  "data/bike_sales/data_wrangled/bike_order_line.rds"
)

glimpse(bike_orderlines_tbl)


# 1.0 ANATOMY OF A FUNCTION ----

# 1.1 Examining the mean() function ----
x <- c(0:10, 50, NA_real_)
x

mean(x) # [1] NA

mean(x, na.rm = T) #[1] 8.75

mean(x, na.rm = T, trim = 0.1) #[1] 5.5


mean_remove_na <- function(x, na.rm = TRUE, ...) {
  # body
  #

  log_info(str_glue('Calculating Average of {length(x)} elements '))

  avg <- mean(x, na.rm = na.rm, ...)

  return(avg)
}


mean_remove_na(x)


rolling_avg3_tbl <- bike_orderlines_tbl |>
  select(order_date, category_1, category_2, total_price) |>
  mutate(order_date = ymd(order_date)) |>
  mutate(
    month_end = ceiling_date(order_date, unit = 'month') -
      period(1, units = 'day')
  ) |>
  group_by(category_1, category_2, month_end) |>

  summarise(total_price = sum(total_price)) |>
  mutate(
    rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = 'right')
  ) |>

  ungroup() |>

  mutate(
    category_2 = as_factor(category_2) |> fct_reorder2(month_end, total_price)
  )

rolling_avg3_tbl |>
  ggplot(aes(x = month_end, y = total_price, color = category_2)) +
  geom_point() +
  geom_line(aes(y = rolling_avg_3), color = 'blue') +
  facet_wrap('category_2', scales = 'free_y') +
  theme_tq() +
  scale_color_tq()


## 3.0 Controlling flow ----
##

detect_value <- function(x) {
  if (is.numeric(x)) {
    message("Value is numeric!!!")
    print(x)
  } else if (is.character(x)) {
    warning('Value should be numeric, but can be accepted')
    print(x)
  } else if (is.logical(x)) {
    stop('Value cant be logically. Value can be numeric or character')
    print(x)
  } else {
    message('Unknow class')
    print(x)
  }
}


1 |> detect_value()

'1' |> detect_value()

TRUE |> detect_value()


## 4.0 creating the class / functions for ----

bikes_tbl <- bike_orderlines_tbl |>
  distinct(model, category_1, price)

# Viz

bikes_tbl |>
  ggplot(aes(x = category_1, y = price)) +
  geom_boxplot() +
  theme_tq() +
  scale_color_tq()


### 4.1 detect_Outliers ----

detect_outliers <- function(x) {
  if (!is.numeric(x)) stop("x must be numeric")

  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q)

  lo <- q[1] - 1.5 * iqr
  hi <- q[2] + 1.5 * iqr

  ifelse(is.na(x), NA, x < lo | x > hi)
}

bikes_w_outliers_tbl <- bikes_tbl |>
  group_by(category_1) |>
  mutate(outliers = detect_outliers(price)) |>
  ungroup()


bikes_w_outliers_tbl |>
  ggplot(aes(x = category_1, y = price)) +
  geom_boxplot() +
  geom_label_repel(
    aes(label = model),
    data = bikes_w_outliers_tbl |> filter(outliers),
    color = 'red',
    size = 3
  ) +
  theme_tq() +
  scale_color_tq() +
  labs(x = 'Primary Category', y = 'Price')


# 5.0 DATA FRAME: FEATURE ENGE

# bikes_tbl |>
#     select(model) |>
#     mutate(
#         model = case_when(
#             model == 'CAAD Disc Ultegra' ~ 'CAAD12 Disc Ultegra',
#             model == 'Syapse Carbon Tiagra' ~ 'Synapse Carton Tiagar',
#             model == 'Supersix Evo Hi-Mod Utegra' ~ 'Supersix Evo Hi-Mod Ultegra',
#             TRUE ~ model
#         )
#     ) |> # Fix typo
#     separate(
#         col = model,
#         into = str_c("model_", 1:7),
#         sep = ' ',
#         remove = FALSE,
#         fill = 'right',
#         extra = 'drop'
#     ) |>
#     # Creating the base feature
#     mutate(
#         base_model = case_when(
#             str_detect(str_to_lower(model_1), "supersix") ~
#                 str_c(model_1, model_2, sep = ' '),
#
#             str_detect(str_to_lower(model_1), "fat") ~
#                 str_c(model_1, model_2, sep = ' '),
#
#             str_detect(str_to_lower(model_1), "beast") ~
#                 str_c(model_1, model_2, model_3, model_4, sep = ' '),
#
#             str_detect(str_to_lower(model_1), "bad") ~
#                 str_c(model_1, model_2, sep = ' '),
#
#             str_detect(str_to_lower(model_1), 'Scalpel') |
#                 str_detect(model_2, '29') ~
#                 str_c(model_1, model_2, sep = ' '),
#
#             TRUE ~ model_1
#         )
#     ) |>
#     mutate(model_tier = model |> str_replace(base_model, '') |> str_trim()) |>
#
#     select(-contains('model_'), 'model_tier') |>
#
#     mutate(
#         black = model_tier |> str_to_lower() |> str_detect('black') |> as.numeric(),
#
#         hi_mod = model_tier |>
#             str_to_lower() |>
#             str_detect('hi-mod') |>
#             as.numeric(),
#
#         ultegra = model_tier |>
#             str_to_lower() |>
#             str_detect('ultegra') |>
#             as.numeric(),
#
#         team = model_tier |> str_to_lower() |> str_detect('team') |> as.numeric(),
#
#         red = model_tier |> str_to_lower() |> str_detect('red') |> as.numeric(),
#
#         dura_ace = model_tier |>
#             str_to_lower() |>
#             str_detect('dura ace') |>
#             as.numeric(),
#
#         disc = model_tier |> str_to_lower() |> str_detect('disc') |> as.numeric()
#     )
#
#

separate_bike_model <- function(data, keep_model_column = TRUE, append = TRUE) {
  if (!append) data <- data |> select(model)

  output_tbl <- data |>
    mutate(
      model = case_when(
        model == 'CAAD Disc Ultegra' ~ 'CAAD12 Disc Ultegra',
        model == 'Syapse Carbon Tiagra' ~ 'Synapse Carton Tiagar',
        model == 'Supersix Evo Hi-Mod Utegra' ~ 'Supersix Evo Hi-Mod Ultegra',
        TRUE ~ model
      )
    ) |> # Fix typo
    separate(
      col = model,
      into = str_c("model_", 1:7),
      sep = ' ',
      remove = FALSE,
      fill = 'right',
      extra = 'drop'
    ) |>
    # Creating the base feature
    mutate(
      base_model = case_when(
        str_detect(str_to_lower(model_1), "supersix") ~
          str_c(model_1, model_2, sep = ' '),

        str_detect(str_to_lower(model_1), "fat") ~
          str_c(model_1, model_2, sep = ' '),

        str_detect(str_to_lower(model_1), "beast") ~
          str_c(model_1, model_2, model_3, model_4, sep = ' '),

        str_detect(str_to_lower(model_1), "bad") ~
          str_c(model_1, model_2, sep = ' '),

        str_detect(str_to_lower(model_1), 'Scalpel') |
          str_detect(model_2, '29') ~
          str_c(model_1, model_2, sep = ' '),

        TRUE ~ model_1
      )
    ) |>
    mutate(model_tier = model |> str_replace(base_model, '') |> str_trim()) |>

    select(-contains('model_'), 'model_tier') |>

    mutate(
      black = model_tier |>
        str_to_lower() |>
        str_detect('black') |>
        as.numeric(),

      hi_mod = model_tier |>
        str_to_lower() |>
        str_detect('hi-mod') |>
        as.numeric(),

      ultegra = model_tier |>
        str_to_lower() |>
        str_detect('ultegra') |>
        as.numeric(),

      team = model_tier |> str_to_lower() |> str_detect('team') |> as.numeric(),

      red = model_tier |> str_to_lower() |> str_detect('red') |> as.numeric(),

      dura_ace = model_tier |>
        str_to_lower() |>
        str_detect('dura ace') |>
        as.numeric(),

      disc = model_tier |> str_to_lower() |> str_detect('disc') |> as.numeric()
    )

  if (!keep_model_column) output_tbl <- output_tbl |> select(-model)

  return(output_tbl)
}


## TO create directory using fs

#fs::dir_create('scripts/')

## TO create R Code using fs

fs::file_create(path = 'scripts/utils.R')


file_header <- str_glue(
  "
# Separate Bikemodels and detect outliers ----

# separate_bike_model(): A tidy function that separate model column into engineered features 

# detect_outliers(): A vectorized function that detects outliers and returns TRUE / FALSE output

# Loading Library ----

library(tidyverse)
"
)


write_lines(file_header, path = 'scripts/utils.R')

# Add the function to the file with dump ---

c("separate_bike_model", "detect_outliers") |>
  dump(file = 'scripts/utils.R', append = TRUE)


# Source the function with source ---
#

source('scripts/utils.R')

bikes_tbl |>
  separate_bike_model()

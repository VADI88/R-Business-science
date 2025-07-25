# 0.0 importing library ----
#

library(tidyverse)
library(lubridate)
library(tidyquant)
library(fs)
library(readxl)
library(janitor)
library(scales)


wrangled_data_path <- fs_path('data/bike_sales/data_wrangled')
raw_data_path <- fs_path('data/bike_sales/data_raw')

bike_orderlines_tbl <- read_rds(wrangled_data_path / 'bike_order_line.rds')
bikes_tbl <- read_excel(raw_data_path / "bikes.xlsx")

bikes_tbl <- bikes_tbl |> clean_names()

# 1.0 Basics ----

## 1.1 Detection: Used with filter() ----

# Vector

c("Supersix Evo Black Inc.", "Supersix Evo Hi-Mod Utegra") |>
  str_detect("Supersix")


# Tibble

bikes_tbl |>
  select(model) |>
  mutate(
    supersix = model |> str_to_lower() |> str_detect("supersix") |> as.numeric()
  ) |>
  mutate(black = model |> str_to_lower() |> str_detect("black") |> as.numeric())

## 1.2 Case & Concatenation ----

bikeshop_name <- "Ithaca Mountain Climbers"

bikeshop_name |> str_to_lower()

bikeshop_name |> str_to_title()

bikeshop_name |> str_to_sentence()

bikeshop_name |> str_to_upper()


# Concatenation

# Vector
#
order_id <- 1
order_line <- 1

str_c(
  "Order Line: ",
  order_id,
  '.',
  order_line,
  " sent to customer ",
  bikeshop_name
)

str_glue(
  "Order line : {order_id}.{order_line} sent to customer: {bikeshop_name |> str_to_upper()}"
)


# Tibble
#

bike_orderlines_tbl |>
  select(bikeshop_name, order_id, order_line) |>
  mutate(
    purchase_statement = str_glue(
      "Order line : {order_id}.{order_line} sent to customer: {bikeshop_name |> str_to_title()}"
    ) |>
      as.character()
  )

# 1.3 Separating Text: See tidyr::separate() ----

# Vector
#

c("Road - Elite Road - Carbon", "Road - Elite Road") |>
  str_split(pattern = ' - ', simplify = TRUE)


# Tibble
#
bikes_tbl |>
  select(description) |>
  separate(
    col = description,
    sep = " - ",
    into = c("category_1", "category_2", "frame_material"),
    remove = FALSE
  )


# 1.4 Trimming Text ----
" Text with space. " |> str_trim()


# 1.5 Replacement: Used with mutate() [and optionally case_when()] ----

# Vector
c("CAD12", "CAD", "CAD8") |>
  str_replace(pattern = "[0-9]", replacement = "")

c("CAD12", "CAD", "CAD8") |>
  str_replace_all(pattern = "[0-9]", replacement = "")

# Tibble

bikes_tbl |>
  select(model) |>
  mutate(
    model_num_removed = model |>
      str_replace_all(pattern = "[0-9]", replacement = "") |>
      str_trim()
  )

# 1.6 Formatting Numbers ----

# values
(1e6 / 1e6) |> number(perfix = "$", suffix = "M") ## Same same but different
(1e6) |> number(scale = 1 / 1e6, perfix = "$", suffix = "M")


1e6 |> number(big.mark = ',')

1e6 |> dollar()


# percents

0.15 |> percent()


# 1.7 Formatting Column Names ----

# Replacing text in column names
bike_orderlines_tbl |>
  set_names(
    names(bike_orderlines_tbl) |> str_replace(pattern = '_', replacement = '.')
  )


# Appending text to column names

bike_orderlines_tbl |>
  set_names(str_glue("{names(bike_orderlines_tbl)}_bike"))
# Appending text to specific column names
#

bike_orderlines_tbl |>
  rename_at(.vars = vars(model:frame_material), .funs = ~ str_c("prod_", .)) |>
  rename_at(.vars = vars(bikeshop_name:state), .funs = ~ str_c("cust_", .)) |>
  glimpse()

# 2.0 Feature engineering with text ----
# Extract model column and extracting well formatted features
#

bikes_tbl |>
  select(model) |>
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
    black = model_tier |> str_to_lower() |> str_detect('black') |> as.numeric(),

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

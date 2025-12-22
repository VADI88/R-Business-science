# Separate Bike models and detect outlier ----

# separate_bike_model(): A tidy function that separate model column into engineered features 

# detect_outlier(): A vectorized function that detects outliers and returns TRUE / FALSE output

# Loading Library ----

library(tidyverse)
separate_bike_model <-
function(data, keep_model_column = TRUE, append = TRUE) {
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
  
  if(!keep_model_column) output_tbl <- output_tbl |> select(-model)
  
    return(output_tbl)
}
detect_outliers <-
function(x) {
  if (!is.numeric(x)) stop("x must be numeric")
  
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q)
  
  lo <- q[1] - 1.5 * iqr
  hi <- q[2] + 1.5 * iqr
  
  ifelse(is.na(x), NA, x < lo | x > hi)
}

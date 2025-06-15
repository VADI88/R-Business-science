# Data basics----

library(tidyverse)


# Data types -----

chr_vector <- c('a', 'b', 'c')
chr_vector |>
  class()

num_vector <- c(1, 2, 3)

num_vector |>
  class()

# Data structure ----

ex_tbl <- tibble(chr_vector, num_vector)

ex_tbl |>
  class()

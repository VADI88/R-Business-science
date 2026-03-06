# LIBRARY ----
#

library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)
library(fs)

# Get the data ----
#

file_path <- fs_path('data/bike_sales/data_wrangled/bike_order_line.rds')

bike_orderlines_tbl <- read_rds(file_path)


glimpse(bike_orderlines_tbl)


# 1. Customer trends ---
#

customer_trends_tbl <- bike_orderlines_tbl |>
  select(
    bikeshop_name,
    price,
    model,
    category_1,
    category_2,
    frame_material,
    quantity
  ) |>
  group_by(
    bikeshop_name,
    price,
    model,
    category_1,
    category_2,
    frame_material
  ) |>

  summarise(
    quantity_purchased = sum(quantity),
  ) |>
  ungroup() |>

  group_by(
    bikeshop_name
  ) |>

  mutate(
    prop_of_total = quantity_purchased / sum(quantity_purchased)
  ) |>
  ungroup()


## CONVERT CUSTOMER TRENDS TO CUSTOMER PRODUCT (USER ITEM MATRIX)

customer_product_tbl <- customer_trends_tbl |>
  select(bikeshop_name, model, prop_of_total) |>
  spread(key = model, value = prop_of_total, fill = 0) |>
  janitor::clean_names()

## Creating the kmeans ----

kmeans_obj <- customer_product_tbl |>
  select(-bikeshop_name) |>
  kmeans(centers = 5, nstart = 100) ## Required argument ; Centers  # additional nstart - provides ensures higher highlikehood that a good centers is found

# BROOM::TIDY ==> PROVIDES CLUSTER NUMBER and it's CENTER
kmeans_obj |>
  tidy() |>
  glimpse()


# BROOM::GLANCE get tot.withinss values for sree plot
kmeans_obj |>
  glance()


# BROOM::AUGMENT add the data with clusters informations
#

kmeans_obj |>
  augment(customer_product_tbl) |>
  select(bikeshop_name, .cluster)


# Identify the best centers values
#
#

kmeans_map <- function(data, centers = 3) {
  data |>
    select(where(is.numeric)) |>
    kmeans(centers = centers, nstart = 100)
}


kmeans_center <- tibble(center = 1:15)


kmeans_mapped_tbl <- kmeans_center |>
  mutate(
    kmeans_obj = center |>
      map(~ kmeans_map(data = customer_product_tbl, centers = .x))
  ) |>
  mutate(glance = kmeans_obj |> map(glance))


## Viz sree plot

kmeans_mapped_tbl |>
  unnest(glance) |>
  select(center, tot.withinss) |>

  ggplot(aes(x = center, y = tot.withinss)) +
  geom_point(color = '#2c3e50', size = 4) +
  geom_line(color = '#2c3e50', linewidth = 1) +
  ggrepel::geom_label_repel(aes(label = center)) +
  scale_y_continuous(labels = scales::number_format()) +
  theme_tq() +
  labs(
    title = 'Skree plot',
    subtitle = 'measures the total within for each centers'
  )

##  UMAP ----

umap_obj <- customer_product_tbl |>
  select(-bikeshop_name) |>
  umap()


kmeans_best_centers_tbl <- kmeans_mapped_tbl |>
  filter(center == 4) |>
  pull(kmeans_obj) |>
  pluck(1) |>
  augment(customer_product_tbl) |>
  select(bikeshop_name, .cluster)

umap_results_tbl <- umap_obj$layout |>
  as_tibble() |>
  bind_cols(customer_product_tbl |> select(bikeshop_name)) |>
  left_join(kmeans_best_centers_tbl) |>
  mutate(
    label_text = str_glue(
      "Customer:{bikeshop_name}
                                 Cluster:{.cluster}"
    )
  )


umap_results_tbl |>
  ggplot(aes(x = V1, y = V2, color = .cluster)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = label_text), size = 3) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = 'Customer segmentation: 2D Projections',
    subtitle = 'UMAP 2D Projections with KMeans cluster segments'
  ) +
  theme(
    legend.position = "none"
  )


## ANALYSING THE PRICE TREND ----

cluster_trends_tbl <- customer_trends_tbl |>
  left_join(umap_results_tbl |> select(-label_text)) |>

  mutate(
    price_bin = case_when(
      price <= 2240 ~ 'low',
      price <= 4260 ~ 'medium',
      TRUE ~ 'high'
    )
  ) |>

  #
  select(
    .cluster,
    model,
    contains('price'),
    category_1:quantity_purchased,
    everything()
  ) |>

  group_by_at(.vars = vars(.cluster:frame_material)) |>
  summarise(
    total_quantity = sum(quantity_purchased)
  ) |>
  ungroup() |>
  group_by(.cluster) |>
  mutate(prop_of_total = total_quantity / sum(total_quantity)) |>
  ungroup()


cluster_trends_tbl |>
  filter(.cluster == 1) |>
  arrange(desc(prop_of_total)) |>
  mutate(cumtotal = cumsum(prop_of_total)) |>
  group_by(price_bin, category_1, frame_material) |>
  summarise(avg = mean(total_quantity)) |>
  arrange(desc(avg))


cluster_trends_tbl |>
  filter(.cluster == 2) |>
  arrange(desc(prop_of_total)) |>
  mutate(cumtotal = cumsum(prop_of_total)) |>
  group_by(price_bin, category_1, frame_material) |>
  summarise(avg = mean(total_quantity)) |>
  arrange(desc(avg))


cluster_trends_tbl |>
  filter(.cluster == 3) |>
  arrange(desc(prop_of_total)) |>
  mutate(cumtotal = cumsum(prop_of_total)) |>
  group_by(price_bin, category_1, frame_material) |>
  summarise(avg = mean(total_quantity)) |>
  arrange(desc(avg))


cluster_trends_tbl |>
  filter(.cluster == 4) |>
  arrange(desc(prop_of_total)) |>
  mutate(cumtotal = cumsum(prop_of_total)) |>
  group_by(price_bin, category_1, frame_material) |>
  summarise(avg = mean(total_quantity)) |>
  arrange(desc(avg))


## UPDATE VISUALATION ----
##

cluster_label_tbl <- tibble(
  .cluster = 1:4,
  .cluster_label = c(
    "High End Price, Mountain, Carbon Frame",
    "Low/Medium Price, Road, Aluminum Frame",
    "High End Price, Road, Carbon Frame",
    "Low/Medium Price, Mountain, Aluminum Frame"
  )
) |>
  mutate(.cluster = as_factor(as.character(.cluster)))


umap_results_tbl |>
  left_join(cluster_label_tbl) |>
  mutate(
    label_text = str_glue(
      "Customer: {bikeshop_name}
                                 Cluster: {.cluster}
                                 {.cluster_label}
                                 "
    )
  ) |>
  ggplot(aes(x = V1, y = V2, color = .cluster)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = label_text), size = 3) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = 'Customer segmentation: 2D Projections',
    subtitle = 'UMAP 2D Projections with KMeans cluster segments'
  ) +
  theme(
    legend.position = "none"
  )

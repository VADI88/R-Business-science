## 0.0 Library -----
##

library(tidyverse)
library(tidyquant)
library(forcats)

bike_orderlines_tbl <- read_rds(
  "data/bike_sales/data_wrangled/bike_order_line.rds"
)

## 1.0 Top 10 Customer Lollipop chart ----
##

n <- 12
top_customer_tbl <- bike_orderlines_tbl |>
  select(bikeshop_name, total_price) |>
  mutate(
    bikeshop_name = bikeshop_name |>
      as_factor() |>
      fct_lump(n = n, w = total_price)
  ) |>
  group_by(bikeshop_name) |>
  summarize(revenue = sum(total_price)) |>
  ungroup() |>

  mutate(
    bikeshop_name = bikeshop_name |> fct_reorder(revenue)
  ) |>
  mutate(bikeshop_name = bikeshop_name |> fct_relevel('Other', after = 0)) |>
  arrange(desc(bikeshop_name)) |>
  mutate(revenue_text = scales::dollar(revenue, scale = 1e-6, suffix = "M")) |>
  mutate(cumm_pct = revenue |> cumsum() / sum(revenue)) |>
  mutate(cumm_pct_txt = scales::percent(cumm_pct)) |>

  mutate(rank = row_number()) |>

  mutate(
    rank = case_when(
      rank == max(rank) ~ NA_integer_,
      TRUE ~ rank
    )
  ) |>

  mutate(
    label_text = str_glue(
      "Rank : {rank}\n Rev: {revenue_text}\n CumPct: {cumm_pct_txt}"
    )
  )


### 1.1 VIZ  ------
##
##
##

top_customer_tbl |>
  ggplot(aes(revenue, bikeshop_name)) +
  geom_segment(aes(xend = 0, yend = bikeshop_name), color = '#2c3e50') +
  geom_label(
    aes(label = label_text),
    hjust = 'inward',
    size = 3,
    color = '#2c3e50'
  ) +

  scale_x_continuous(
    labels = scales::dollar_format(scale = 1e-6, suffix = 'M')
  ) +

  labs(
    title = str_glue('Top {n} Customers'),
    subtitle = str_glue(
      'Start Date: {year(min(bike_orderlines_tbl$order_date))} 
                            End Date:  {year(max(bike_orderlines_tbl$order_date))} 
                            '
    ),
    y = 'Customers',
    x = 'Revenue'
  ) +
  theme_tq() +
  theme(legend.position = "none")


## 2.0 HEATMAPS ----
##


pct_sales <- bike_orderlines_tbl |> 
    select(bikeshop_name,category_1,category_2,quantity) |> 
    group_by(bikeshop_name,category_1,category_2) |> 
    summarise(total_qty = sum(quantity)) |> 
    ungroup() |> 
    group_by(bikeshop_name) |> 
    mutate(pct = total_qty / sum(total_qty)) |> 
    ungroup() |> 
    
    mutate(bikeshop_name = as.factor(bikeshop_name) |> fct_rev()) |> 
    mutate(bikeshop_name_num = as.numeric(bikeshop_name))
    
    

pct_sales |> 
    ggplot(aes(category_2, bikeshop_name)) + 
    geom_tile(aes(fill = pct)) + 
    
    geom_text(aes(label = pct |> scales::percent(accuracy = 0.1)),
              size = 3
              ) + 
    
    facet_wrap('~category_1',scale='free_x') + 
    
    scale_fill_gradient(low='white',high = '#2c3e50') + 
    
    labs(
        title = 'Heatmap of purchasing habits',
        x = 'Bike type(Category2)', 
        y = 'Customers'
        
    ) + 
    
    theme_tq() + 
    theme(
        axis.text.x = element_text(angle = 45,hjust = 0.9),
        legend.position = 'none'
    )
    
    
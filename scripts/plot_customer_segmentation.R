get_customer_segments <-
function(k = 4, seed = 123) {
    
    # 1.0 CUSTOMER TRENDS
    
    customer_trends_tbl <- bike_orderlines_tbl |> 
        select(bikeshop_name,price,model,category_1,
               category_2,frame_material,quantity) |> 
        group_by_at(.vars = vars(bikeshop_name:frame_material)) |> 
        summarize(total_quantity = sum(quantity)) |> 
        ungroup() |> 
        group_by(bikeshop_name) |> 
        mutate(pct = total_quantity/sum(total_quantity)) |> 
        ungroup()
        
    customer_product_tbl <- customer_trends_tbl |> 
        select(bikeshop_name,model,pct) |> 
        spread(key = model,value=pct,fill=0)
    
    
    # 2.0 MODELING: K-MEANS CLUSTERING
    # 
    RNGkind(sample.kind = "Rounding")
    set.seed(seed=seed)
    
    kmeans_obj <- customer_product_tbl |> 
        select(-bikeshop_name) |> 
        kmeans(centers = k,nstart = 100) 
        
    
    kmeans_tbl <- kmeans_obj |> 
        augment(customer_product_tbl) |> 
        select(bikeshop_name,.cluster)
    
    # 3.0 UMAP
    umap_configuration <- umap.defaults 
    umap_configuration$random_state <- seed
    
    
    
   umap_obj <-  customer_product_tbl |> 
        select(-bikeshop_name) |> 
        as.matrix() |> 
        umap(config = umap_configuration) 
    
    umap_tbl <- umap_obj |> 
        pluck("layout") |> 
        as_tibble() |> 
        set_names(c("x","y")) |> 
        bind_cols(
            customer_product_tbl |> select(bikeshop_name)
        )
    
    
    # 4.0 COMBINE UMAP & K-MEANS
    # 
    combine_tbl <- umap_tbl |> 
        left_join(kmeans_tbl,by='bikeshop_name') |> 
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                     CLuster: {.cluster}
                                     " ))
    return(combine_tbl)
    
}
plot_customer_segments <-
function(k = 4, seed = 123, interactive = TRUE) {
    
    # DATA MANIPULATION
    
    combined_tbl <- get_customer_segments(k = k , seed =  seed)
    
    
    # VISUALIZATION
    
    g <- combined_tbl |> 
        ggplot(aes(x = x, y = y , color =.cluster)) +
        geom_point(aes(text =label_text),size= 4) + 
        theme_tq() + 
        scale_color_tq() + 
        labs(
            title = "Customer Segmentation 2D Proporitation",
            subtitle = "UMAP 2D Propertions with kmeans clustering"
        ) +
        theme(
            legend.position = "none"
        )
    
    # INTERACTIVE VS STATIC
    
    if(interactive){
        return(ggplotly(g,tooltip = 'text'))
        
    }else{
        return(g + ggrepel::geom_label_repel(aes(label = label_text),size = 2 ))
    }
    
    
}
plot_customer_heatmap <-
function(interactive = TRUE) {
    
    # DATA MANIPULATION
    # 
    pct_sales_by_customer_tbl <- bike_orderlines_tbl |> 
        select(bikeshop_name,category_1,category_2,quantity) |> 
        group_by(bikeshop_name,category_1,category_2) |> 
        summarise(total_qty = sum(quantity)) |> 
        ungroup() |> 
        
        group_by(bikeshop_name) |> 
        mutate(pct = total_qty/sum(total_qty)) |> 
        ungroup() |> 
        
        mutate(bikeshop_name = as_factor(bikeshop_name) |> fct_rev()) |> 
        
        mutate(label_text = str_glue("Customer {bikeshop_name}
                                     Category: {category_1}
                                     Sub Category: {category_2}
                                     Total Quantity:  {total_qty}
                                     Percent of Sales: {scales::percent(pct)}
                                    "))
    
    
    # VISUALIZATION
    
    
   g <-  pct_sales_by_customer_tbl |> 
        ggplot(aes(category_2,bikeshop_name))  + 
        geom_tile(aes(fill = pct))  + 
        geom_text(aes(label = scales::percent(pct,accuracy = 0.01),
                      text = label_text),size = 3 ) + 
        facet_wrap(~category_1,scales='free_x') + 
        scale_fill_gradient(low = "white",high = "#2c3e50") +
        theme_tq() + 
        theme(
            axis.text.x =  element_text(angle = 45,hjust = 1),
            legend.position = "none",
            plot.title = element_text(face="bold"),
            strip.text.x = element_text(margin = margin(0, 5, 5, 5, unit = "pt"))
            
        ) + 
        labs(
            title = 'Heatmap of purchasing habits',
           
        )
        
    
    # INTERACTIVE VS STATIC
    # 
    if (interactive){
        
        g <- g + labs( x ='',
                       y = '')
        return(ggplotly(g,tooltip='label_text'))
    }
    else{
        g <- g + labs( x ='BikeCategory',
                       y = 'Bikeshop Name')
        return(g)
    }
    
}
plot_customer_behavior_by_cluster <-
function(top_n_products = 10, 
                                              k = 4, seed = 123, 
                                              interactive = TRUE) {
    
    # DATA MANIPULATION
    
    combined_tbl <- get_customer_segments(k = k , seed =  seed) |> 
        select(bikeshop_name,.cluster)
    
     top_n_bikes <- bike_orderlines_tbl |> 
         select(bikeshop_name,model,category_1,
                category_2,price,quantity) |> 
         group_by_at(.vars = vars(bikeshop_name:price)) |> 
         summarize(total_quantity = sum(quantity)) |> 
         ungroup() |> 
         
         group_by(bikeshop_name) |> 
         
         arrange(desc(total_quantity),.by_group = TRUE) |> 
         slice(1:top_n_products) |> 
         
         ungroup() |> 
         left_join(
             combined_tbl
             , by='bikeshop_name'
         ) |> 
         mutate(label_text = str_glue("BikeshopName: {bikeshop_name}
                                     Model: {model}
                                     Category: {category_1}
                                     SubCategory: {category_2}
                                     Price: {scales::dollar(price)}
                                     " ))
         
         
    
    # VISUALIZATION
    
     
     g <- top_n_bikes |> 
         ggplot(aes(category_1,price, color=.cluster))  + 
         geom_violin() + 
         geom_jitter(aes(text= label_text),alpha = 0.5,width = 0.2) + 
         facet_wrap(~.cluster,ncol = 2) + 
         scale_y_log10(labels = scales::dollar_format(accuracy = 1)) + 
         theme_tq() +
         scale_color_tq() + 
         theme(
             strip.text.x = element_text(margin = margin(0,5,5,5,unit = "pt"))
         )+
         labs(
             title = str_glue("Top {top_n_products} Bike Models by Customer and Cluster"),
             x = "Category",
             y = "Price"
         )
        
         
    
    # INTERACTIVE VS STATIC
    # 
     if(interactive){
        return( ggplotly(g,tooltip = 'text'))
         
     }else{
         return(g) 
     }
    
    
}

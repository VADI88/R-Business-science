get_bike_features <-
function() {
    
    bike_feature_tbl <- bike_orderlines_tbl |> 
        
        select(price,model,category_1,category_2,frame_material) |> 
        
        distinct() |> 
        mutate(id = row_number()) |> 
        select(id , everything()) |> 
        separate_bike_model(keep_model_column = T,append = T)
    
    return(bike_feature_tbl)
}
plot_bike_features <-
function(interactive = TRUE) {
    
    # DATA MANIPULATION
    
    bike_feature_tbl <- get_bike_features() 
    
    # VISUALIZATION
    
    g <- bike_feature_tbl |> 
        mutate(category_2 = fct_reorder(category_2,price),
               label_text = str_glue("Model: {model}
                                     Price: {scales::dollar(price)}
                                     ")
        ) |> 
        ggplot(aes(category_2,price)) + 
        geom_violin() + 
        geom_jitter(aes(text = label_text),width = 0.1,color = '#2c3e50',alpha = 0.5) +
        facet_wrap(~frame_material) + 
       coord_flip() + 
        theme_tq()+
        theme(strip.text.x = element_text(margin = margin(5,5,5,5,unit = "pt"))) + 
        labs(
            title = 'Product Gap Analysis',
            x = '',
            y = ''
        )+
        scale_y_continuous(labels = scales::dollar_format())
    
    
    if(interactive){
        return(ggplotly(g , tooltip = 'label_text'))
        
    }else{
        return(g)
        
    }
    
    
  
    
    
}

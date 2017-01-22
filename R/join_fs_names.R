# Adding names to the faostat data with the codes
join_fs_names <-
  function(df, 
           ref_tbls = "data/references.Rdata") {
    
    load(ref_tbls)
    
    # Loading data from the respective files
    all_items <-
      all_items %>%
      mutate(ItemCode = as.integer(ItemCode))
    
    all_elements <-
      all_elements %>%
      select(ElementCode, ElementName, Unit) %>%
      mutate(ElementCode = as.integer(ElementCode))
    
    all_areas <-
      all_areas %>%
      bind_rows(all_regions) %>% 
      tbl_df() %>%
      mutate(AreaCode = as.integer(AreaCode))
    
    # Ensuring the codes in the original dataframe are defined as integers
    
    # Carrying checks and, if necessary, cleaning the data frame
    if ("Unit" %in% colnames(df) & "ElementCode" %in% colnames(df))
    {
      df <- df %>% select(-Unit)
    }
    
    if ("AreaName" %in% colnames(df))
    {
      df <- df %>% select(-AreaName)
    }
    
    if ("ItemName" %in% colnames(df))
    {
      df <- df %>% select(-ItemName)
    }
    
    if ("ElementName" %in% colnames(df) & "ElementCode" %in% colnames(df) )
    {
      df <- df %>% select(-ElementName)
    }
    
    
    if ("AreaCode" %in% colnames(df))
    {
      df$AreaCode <- as.integer(df$AreaCode)
      df <- df %>%
        left_join(all_areas, by = "AreaCode")
    }
    
    if ("ItemCode" %in% colnames(df))
    {
      df$ItemCode <- as.integer(df$ItemCode)
      df <- df %>%
        left_join(all_items, by = "ItemCode")
    }
    
    if ("ElementCode" %in% colnames(df))
    {
      df$ElementCode <- as.integer(df$ElementCode)
      df <- df %>%
        left_join(all_elements, by = "ElementCode")
    }
    
    df
  }
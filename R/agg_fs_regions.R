# Function for ggregating fostat regions used in research
agg_fs_regions <- function(df,
                           use_china_agg = FALSE,
                           returnFlags = FALSE,
                           mapping_tbl_path =
                             "data/references.Rdata") {
  require(dplyr)
  
  load(mapping_tbl_path)
  
  # Loading the list of areas to map
  regions <-
    regionas_agg %>% 
    select(GroupCode, AreaCode)
  
  if (use_china_agg) {
    regions <-
      regions %>%
      filter(!(AreaCode %in% c(41, 96, 128, 214, 357)))
  } else {
    regions <-
      regions %>%
      filter(!(AreaCode %in% c(357, 351)))
  }
  
  if (!("AreaCode" %in% names(df))) {
    stop("The variable 'AreaCode' must be present in the suplied dataframe")
  }
  
  if ("Flag" %in% names(df)) {
    df <-
      df %>%
      select(-Flag)
  }
  
  # Calculating the results and returning output
  df <-
    df %>%
    left_join(regions, by = "AreaCode") %>%
    select(-AreaCode) %>%
    dplyr::rename(AreaCode = GroupCode) %>%
    filter(!is.na(AreaCode))
  
  # all possible groups that countl be used for aggregation
  groups <- c(
    "AreaCode",
    "ItemCode",
    "ElementCode",
    "Year",
    "d.source",
    "AreaName",
    "ItemName",
    "ElementName",
    "DomainName",
    "DomainCode"
  )
  
  # Summarising data
  df <-
    df %>%
    group_by_(.dots = groups[groups %in% names(df)]) %>%
    summarise_each(funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  if (returnFlags) {
    df <-
      df %>%
      mutate(Flag = "A")
    message("New flag 'A' was assign to all regional aggregates")
  }
  
  df
  
}

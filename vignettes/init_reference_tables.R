library(dplyr)

all_items <-
  read.csv("data_raw/items_names.csv", 
           stringsAsFactors = FALSE) %>% 
  tbl_df()

all_elements <-
  read.csv("data_raw/elements_names.csv", 
           stringsAsFactors = FALSE) %>% 
  tbl_df()

all_areas <-
  read.csv("data_raw/faostat_areas.csv", 
           stringsAsFactors = FALSE) %>% 
  tbl_df()

all_regions <-
  read.csv("data_raw/faostat_regions.csv", 
           stringsAsFactors = FALSE) %>% 
  tbl_df()

china_agg <-
  read.csv("data_raw/china_aggregate.csv", 
           stringsAsFactors = FALSE) %>% 
  tbl_df()

item_agg <-
read.csv("data_raw/items_aggregate.csv", 
         stringsAsFactors = FALSE) %>% 
  tbl_df()

regionas_agg <-
  read.csv("data_raw/areas_regions.csv", 
           stringsAsFactors = FALSE) %>% 
  tbl_df()
  
save("all_items", "all_elements", "all_areas" ,"all_regions", "china_agg", "item_agg", "regionas_agg",
     file = "data/references.Rdata")

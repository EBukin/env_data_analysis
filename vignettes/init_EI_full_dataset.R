# Analyzing data on the greenhouse gas intencities

# Setups ------------------------------------------------------------------

# Setuping the working environment.
packs <- c("plyr", "dplyr", "tidyr", "stringr", "data.table", "ggplot2")
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = "Depends")
lapply(packs, require, character.only = TRUE)

plyr::l_ply(paste0("R/", list.files("R/", "*.R")), source)


# Plan --------------------------------------------------------------------

# Plan of work:
# 1. Combining emisisons intensities data with the productivity data.
#     a. Make sure that the regions used in the data calculation are the same.
#     b. Analyze the yields terics and convert it to the suitale numbers


# Loading data ------------------------------------------------------------


# Emissions intensity data 
EI <- 
  read.fs.bulk("data_raw/Environment_Emissions_intensities_E_All_Data_(Normalized).csv") 

# Crop produciton data 
CR <- 
  read.fs.bulk("data_raw/Production_Crops_E_All_Data_(Normalized).csv")

# Live_animals Data
LA <- 
  read.fs.bulk("data_raw/Production_LivestockPrimary_E_All_Data_(Normalized).csv")

# Cleaning crops production data

# We need to extract data on rice and cereals excluding rice
#   Since there is no data on cereals excluding rice we will calculate it manually
#   by substracting rice from cereals.
CR_sample <-
  CR %>%
  select(-Flag, -ItemName, -ElementName, -Unit) %>%
  filter(ItemCode %in% c(unique(EI$ItemCode), 1717)) %>%
  spread(ItemCode, Value) %>%
  
  # Calculating item ceeals ecluding rice
  mutate(`1718` = `1717` - replace(`27`, is.na(`27`), 0)) %>%
  gather(ItemCode, Value, 5:length(.)) %>%
  mutate(ItemCode = as.numeric(ItemCode)) %>% 
  spread(ElementCode, Value) %>%
  select(-AreaName) %>%
  
  # Aggregating non annex 1 countries
  bind_rows(agg_fs_regions(.) %>% filter(AreaCode %in% c(5848, 5849, 5873))) %>%
  
  # Rcalculating yields
  mutate(`5419` = `5510` / `5312`) %>%
  # gather(ElementCode, Value, 4:length(.)) %>%
  
  # Filtering only yields
  filter(ItemCode != 1717, !is.na(`5419`)) %>% 
  select(AreaCode,  Year, ItemCode, `5419`)


# We need to extract data on some animalsonly and on the yields related to these animals
# In practice, we sould extract only production data and then calculate all we need.

LA_sampl <-
  LA %>%
  select(-Flag, -ItemName, -ElementName, -Unit) %>%
  filter(ItemCode %in% c(unique(EI$ItemCode))) %>%
  
  # Aggregating Annexone and not annex one countries
  spread(ElementCode, Value) %>% 
  select(-AreaName) %>% 
  bind_rows(agg_fs_regions(.) %>% filter(AreaCode %in% c(5848, 5849, 5873))) %>% 
  # join_fs_names() %>% 
  
  # Recalculating Yields
  mutate(`5410` = `5510` / `5313` * 10000,
         `5417` = `5510` / `5320` * 10000,
         `5424` = `5510` / `5321` * 10000,
         `5420` = `5510` / `5318` * 10000) %>% 
    
  mutate(`5410` =  replace(`5410`, is.infinite(`5410`), 0),
         `5417` =  replace(`5417`, is.infinite(`5417`), 0),
         `5424` =  replace(`5424`, is.infinite(`5424`), 0),
         `5420` =  replace(`5420`, is.infinite(`5420`), 0)) %>% 
  
  # Aggregatig=ng non nnex 1countries
  # gather(ElementCode, Value, 4:length(.)) %>% 
  # filter(!is.na(Value)) %>% 
  
  # Filtering only element codeswhich we need
  select(AreaCode, ItemCode, Year, `5424`, `5410`, `5420`, `5417`)

# Combining emissions intencity data with the data from other domains
EI_full <-
  EI %>%
  select(AreaCode, ElementCode, ItemCode, Year, Value) %>%
  spread(ElementCode, Value) %>%
  left_join(CR_sample,  by = c("AreaCode", "ItemCode", "Year")) %>%
  filter(ItemCode %in% c(1718, 27)) %>%
  bind_rows(
    EI %>%
      select(AreaCode, ElementCode, ItemCode, Year, Value) %>%
      spread(ElementCode, Value) %>%
      left_join(LA_sampl,  by = c("AreaCode", "ItemCode", "Year")) %>%
      filter(!ItemCode %in% c(1718, 27))
  ) %>%
  join_fs_names() %>%
  
  # Correcting units of yields from whatever it is to kg per something
  mutate(
    `5419` = `5419`,
    `5424` = `5424` / 10000,
    `5410` = `5410` / 10000,
    `5420` = `5420` / 10000,
    `5417` = `5417` / 10000
  ) %>%
  dplyr::rename(
    `Production, t` = `5510`,
    `Emissions, Gg CO2eq` = `7231`,
    `Emissions intensity, kg CO2eq/kg product` = `71761`,
    `Yield, t/ha` = `5419`,
    `Yield/Carcass Weight, kg/head` = `5424`,
    `Yield, kg/head` = `5410`,
    `Yield, t/head` = `5420`,
    `Yield, t/head` = `5417`
  )


# Saving ------------------------------------------------------------------

# Saving all Emissions intencities data
save("EI_full", file = "data/EI_full.RData")


  
  
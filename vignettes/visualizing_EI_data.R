
# Setuping the working environment.
packs <- c("plyr", "dplyr", "tidyr", "stringr", "ggplot2")
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = "Depends")
lapply(packs, require, character.only = TRUE)


load("data/EI_full.RData")


EI_full_long %>% 
  mutate(ElementName = ifelse(ElementName == "Yield/Carcass Weight", "Yield", ElementName)) %>% 
  select(-Unit) %>% 
  filter(!AreaCode %in% c(5848, 5849, 5873)) %>% 
  spread(ElementName, Value) %>%  
  filter(AreaCode %in% c(5000, 230, 231, 79)) %>% 
  mutate(Year_lable = ifelse(Year == min(Year) | Year == max(Year), Year, NA)) %>% 
  ggplot(aes(`Yield`, `Emissions intensity`, 
         group = AreaName, fill = AreaName, 
         colour = AreaName)) +
  geom_jitter() +
  geom_path() + 
  geom_text(aes(label = Year_lable))+
  facet_wrap(~ItemName, scales = "free")


# Detecting outliers in data and visualizing graphs for pressreliese 



# Setuping the working environment. ---------------------------------------

packs <- c("plyr", "dplyr", "tidyr", "stringr", "ggplot2")
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = "Depends")
lapply(packs, require, character.only = TRUE)



# Loading data ------------------------------------------------------------


load("data/EI_full.RData")

Area_codes <- 
  read.csv("data_raw/areas_regions_hierarchy.csv", stringsAsFactors = FALSE) %>% 
  tbl_df()


# Cleaning data -----------------------------------------------------------

# dding Preparing data without outliers.
EI_full_wide <- 
  
  EI_full_long %>% 
  mutate(ElementName = ifelse(ElementName == "Yield/Carcass Weight", "Yield", ElementName)) %>% 
  select(-Unit) %>% 
  spread(ElementName, Value)%>%
  left_join(Area_codes, "AreaCode") %>% 
  mutate(RegionCode = ifelse(SubRegionCode == 5203, SubRegionCode, RegionCode),
         RegionName = ifelse(SubRegionCode == 5203, SubRegionName, RegionName),
         RegionName = ifelse(RegionCode == 5200, str_c("Other ", RegionName), RegionName))

# Function for highlighting outliers
detect <-
  function(test) {
    cook_ei <-
      test %>%
      lm(`Emissions intensity` ~ Emissions, data = .) %>%
      cooks.distance() %>% 
      as.data.frame() %>% 
      tbl_df
    names(cook_ei) <- "cook_ei"
    cook_ei$row_name = row.names(cook_ei)
    
    cook_y <-
      test %>%
      lm(Yield ~ Production, data = .) %>%
      cooks.distance() %>% 
      as.data.frame() %>% 
      tbl_df()
    names(cook_y) <- "cook_y"
    cook_y$row_name <- row.names(cook_y)
    
    test$row_name <- row.names(test)
    
    test %>% 
      left_join(cook_y, "row_name")%>% 
      left_join(cook_ei, "row_name") %>% 
      select(-row_name) %>% 
      mutate(
        cook_y = cook_y > 4 * mean(cook_y, na.rm = T),
        cook_ei = cook_ei > 4 * mean(cook_ei, na.rm = T)
      )
  }

# Detecting outliers on both Yields and Emission intensities
EI_full_wide <- 
  EI_full_wide %>% 
  # filter(!is.na())
  # filter(ItemCode == 27, Year == 2008) %>% 

  ddply(., 
        .(ItemCode, Year), 
        detect,
        .progress = "text"
        ) %>% 
    tbl_df() 


# Saving data for random access -------------------------------------------

save("EI_full_wide", file = "data/EI_full_wide.RData")

# Plotting Selected diagrams for Press reliese

EI_full_wide %>%
  filter(AreaCode < 5000, !is.na(GroupCodeAnnex), Year == 2014, ItemCode %in% c(882)) %>%
  filter(!cook_ei) %>% 
  ggplot(
    aes(
      `Yield`,
      `Emissions intensity`,
      group = RegionName,
      fill = RegionName,
      colour = RegionName,
      shape = RegionName
    )
  ) +
  geom_jitter() +
  facet_wrap( ~ ItemName, scales = "free") +
  theme(legend.position = "bottom") +
  xlab("Productivity (kg milk/Cow/Year)") +
  ylab("GHG Intensity (kg CO2eq/kg milk)")+
  ggtitle("Cross-sectional view - pooling all countries - 2014")

ggsave("vignettes/Milk_press.png", width = 15, height = 13, units = "cm", dpi = 150)
ggsave("vignettes/Milk_press.png.svg", width = 15, height = 13, units = "cm", dpi = 150)


# Plot for chicken
EI_full_wide %>%
  filter(AreaCode < 5000, !is.na(GroupCodeAnnex), Year == 2014, ItemCode %in% c(1062)) %>%
  filter(!cook_ei) %>% 
  ggplot(
    aes(
      `Yield`,
      `Emissions intensity`,
      group = RegionName,
      fill = RegionName,
      colour = RegionName,
      shape = RegionName
    )
  ) +
  geom_jitter() +
  facet_wrap( ~ ItemName, scales = "free") +
  theme(legend.position = "bottom") +
  xlab("Productivity (kg egg/hen/Year)") +
  ylab("GHG Intensity (kg CO2eq/kg egg)")+
  ggtitle("Cross-sectional view - pooling all countries - 2014")

ggsave("vignettes/Egg_press.png", width = 15, height = 13, units = "cm", dpi = 150)
ggsave("vignettes/Egg_press.svg", width = 15, height = 13, units = "cm", dpi = 150)




# EI_full_wide %>%
#   filter(Year == 2014, AreaCode < 5000, !is.na(GroupCodeAnnex)) %>%
#   filter(!cook_ei, !cook_y) %>% 
#   # mutate(Year_lable = ifelse(Year == min(Year) | Year == max(Year), Year, NA)) %>%
#   ggplot(aes(`Yield`, `Emissions intensity`,
#              group = GroupNameAnnex, fill = GroupNameAnnex, colour = GroupNameAnnex)) +
#   geom_jitter() +
#   # geom_smooth(method = "lm") +
#   # geom_path() +
#   # geom_text(aes(label = Year_lable))+
#   facet_wrap(~ItemCode + ItemName, scales = "free") +
#   theme(legend.position = "bottom")


# EI_full_long %>% 
#   mutate(ElementName = ifelse(ElementName == "Yield/Carcass Weight", "Yield", ElementName)) %>% 
#   select(-Unit) %>% 
#   filter(!AreaCode %in% c(5848, 5849, 5873), ItemCode == 1718) %>% 
#   spread(ElementName, Value) %>%  
#   filter(AreaCode %in% c(5000, 5100, 5200, 5300)) %>% 
#   mutate(Year_lable = ifelse(Year == min(Year) | Year == max(Year), Year, NA)) %>% 
#   ggplot(aes(`Yield`, `Emissions intensity`, 
#              group = AreaName, fill = AreaName, 
#              colour = AreaName)) +
#   geom_jitter() +
#   geom_path() + 
#   geom_text(aes(label = Year_lable))+
#   facet_wrap(~ItemName, scales = "free") +
#   theme(legend.position = "bottom") + 
#   ggtitle("Temporal View")


# Plotting temporal View on graphs
pdf("vignettes/temporal_view.pdf",
    width = 29.7 / 1.8,
    height = 21 / 1.8)

d <- 
  EI_full_long %>%
  mutate(ElementName = ifelse(ElementName == "Yield/Carcass Weight", "Yield", ElementName)) %>%
  select(-Unit) %>%
  filter(!AreaCode %in% c(5848, 5849, 5873)) %>%
  spread(ElementName, Value) %>%
  filter(AreaCode %in% c(5000, 5100, 5200, 5300, 5400, 5500)) %>%
  mutate(Year_lable = ifelse(Year == min(Year) |
                               Year == max(Year), Year, NA))

g <- ggplot(
  data = d,
  aes(
    `Yield`,
    `Emissions intensity`,
    group = AreaName,
    fill = AreaName,
    colour = AreaName
  )
) +
  geom_jitter() +
  geom_path() +
  geom_text(aes(label = Year_lable)) +
  facet_wrap(~ ItemName, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle("Temporal View")
print(g)

  d_ply(d, .(ItemCode),
        function(x) {
          # print(x)
          g <- ggplot(
            data = x,
            aes(
              `Yield`,
              `Emissions intensity`,
              group = AreaName,
              fill = AreaName,
              colour = AreaName
            )
          ) +
            geom_jitter() +
            geom_path() +
            geom_text(aes(label = Year_lable)) +
            facet_wrap(~ ItemName, scales = "free") +
            theme(legend.position = "bottom") +
            ggtitle("Temporal View")
          print(g)
        })

dev.off()

# Plotting cross sectional view on graphs 

pdf("vignettes/cross_sectional.pdf",
    # paper = "a4r"
    width = 29.7 / 1.8,
    height = 21 / 1.8
)

EI_full_wide %>%
  filter(Year > 2009) %>%
  d_ply(., .(Year),
        function(x) {
          g <- x %>%
            filter(AreaCode < 5000,!is.na(GroupCodeAnnex)) %>%
            # mutate(Year_lable = ifelse(Year == min(Year) | Year == max(Year), Year, NA)) %>%
            ggplot(
              aes(
                `Yield`,
                `Emissions intensity`,
                group = GroupNameAnnex,
                fill = GroupNameAnnex,
                colour = GroupNameAnnex
              )
            ) +
            geom_jitter() +
            facet_wrap( ~ ItemName, scales = "free") +
            theme(legend.position = "bottom") +
            ggtitle(str_c("Cross-Sectional View pooling all countries ", unique(x$Year)))
          print(g)
        })

EI_full_wide %>%
  filter(Year == 2014) %>%
  d_ply(., .(Year, ItemCode),
        function(x) {
          g <- x %>%
            filter(AreaCode < 5000,!is.na(GroupCodeAnnex)) %>%
            # mutate(Year_lable = ifelse(Year == min(Year) | Year == max(Year), Year, NA)) %>%
            ggplot(
              aes(
                `Yield`,
                `Emissions intensity`,
                group = RegionName,
                fill = RegionName,
                colour = RegionName
              )
            ) +
            geom_jitter() +
            facet_wrap( ~ ItemName, scales = "free") +
            # geom_smooth()+
            theme(legend.position = "bottom") +
            ggtitle(str_c("Cross-Sectional View pooling all countries ", unique(x$Year)))
          
          print(g)
        })

dev.off()



# Plotting cross sectional view on graphs 

pdf("vignettes/cross_section_1970_1980_1990_2000_2010_2014.pdf",
    paper = "a4r"
    # width = 29.7 / 1.8,
    # height = 21 / 1.8
)

EI_full_wide %>%
  filter(Year %in% c(1970, 1980, 1990, 2000, 2010, 2014))%>%
  d_ply(., .(Year),
        function(x) {
          g <- x %>%
            filter(AreaCode < 5000,!is.na(GroupCodeAnnex)) %>%
            # mutate(Year_lable = ifelse(Year == min(Year) | Year == max(Year), Year, NA)) %>%
            ggplot(
              aes(
                `Yield`,
                `Emissions intensity`,
                group = GroupNameAnnex,
                fill = GroupNameAnnex,
                colour = GroupNameAnnex
              )
            ) +
            geom_jitter() +
            facet_wrap( ~ ItemName, scales = "free") +
            theme(legend.position = "bottom") +
            ggtitle(str_c("Cross-Sectional View pooling all countries ", unique(x$Year)))
          print(g)
        })

EI_full_wide %>%
  filter(Year %in% c(1970, 1980, 1990, 2000, 2010, 2014)) %>%
  d_ply(., .(ItemCode, Year),
        function(x) {
          g <- x %>%
            filter(AreaCode < 5000,!is.na(GroupCodeAnnex)) %>%
            # mutate(Year_lable = ifelse(Year == min(Year) | Year == max(Year), Year, NA)) %>%
            ggplot(
              aes(
                `Yield`,
                `Emissions intensity`,
                group = RegionName,
                fill = RegionName,
                colour = RegionName,
                shape = RegionName
              )
            ) +
            geom_jitter() +
            facet_wrap( ~ ItemName, scales = "free") +
            # geom_smooth()+
            theme(legend.position = "bottom") +
            ggtitle(str_c("Cross-Sectional View pooling all countries ", unique(x$Year)))
          
          print(g)
        })

dev.off()

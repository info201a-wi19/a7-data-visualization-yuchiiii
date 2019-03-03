# install.packages("maps")
library("maps")
library("dplyr")
library("tidyr")
library("ggplot2")
forest_data <- read.csv("data/WBI_Forest_Area_Cleaned.csv", stringsAsFactors = FALSE)
options(scipen = 999)

# Intended to make a line chart that shows the average amount of forested areas 
# (% of land area) world-wide over 14 years(1992 - 2016). 
# Edit the data first.
world_forest <- forest_data %>% 
  filter(Series.Name == "Forest area (% of land area)") %>% 
  select(-X, -Series.Code, -Country.Code, -Series.Name) %>% 
  gather(key = Year,
         value = Forest.area,
         -Country.Name) %>% 
  group_by(Year) %>% 
  summarise(average_global_forest_area = mean(Forest.area, na.rm = T)) %>% 
  mutate(Year = gsub("YR", "", Year))

# Intended to make scatterplots that show the relationship between forest area
# and CO2 emissions by breaking the plot into six facets(at 4-year interval).
# Use the data from World Bank to calculate the average glabal CO2 emission 
# which is 4.4, in order to divide all countries into two categorical variables.
# (Over or below the average)
co2_forest <- forest_data %>% 
  filter(Series.Name == "Forest area (% of land area)" | Series.Name == 
           "CO2 emissions (metric tons per capita)") %>% 
  select(Series.Name, Country.Name, YR1992, YR1996, YR2000, YR2004, YR2008, YR2012) %>% 
  gather(key = Year,
        value = Value,
        -Country.Name,
        -Series.Name) %>% 
  spread(key = Series.Name,
         value = Value) %>%
  filter(!is.na(`CO2 emissions (metric tons per capita)`)) %>% 
  mutate(co2_over_average = if_else(`CO2 emissions (metric tons per capita)` > 4.4,
                                    "Over global average", "Equal or below global average")) %>% 
  mutate(Year = gsub("YR", "", Year))

# Make a map that shows the change in forest area levels for each country from 
# 1992 to 2016.
# Extract the forest area data and calculate the change.  
forest_loss <- forest_data %>% 
  filter(Series.Name == "Forest area (% of land area)") %>%  
  select(Series.Name, Country.Code, YR1992, YR2016) %>%  
  mutate(loss = YR2016 - YR1992) %>% 
  select(-YR2016, -YR1992) 

# Edit the world map data and join two data frames.
world_shape <-  map_data("world") %>%
  mutate(Country.Code = iso.alpha(region, n = 3)) %>% 
  left_join(forest_loss, by = "Country.Code") %>% 
  filter(!is.na(loss))

# Analyze the data to decide breaks for each level.
world_shape_summary <- world_shape %>% 
  summarise(max = max(loss),
            min = min(loss),
            median = median(loss),
            sd = sd(loss), 
            mean = mean(loss))

# Cut the data.
loss_level <- cut(world_shape$loss, breaks = c(-31, -20, -10, 0, 10, 20, 30), 
                  labels = c("-30 - -20", "-19 - -10", "-9 - 0", "1 - 10", "11 - 20",
                             "21 - 30"))
world_shape <- world_shape %>%
  mutate(Loss.level = loss_level) 
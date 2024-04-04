# Package installations
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gapminder")



#package libraries
library("tidyverse")
library(plotly)
library(dplyr)
library(ggplot2)
library(gapminder)


#importing of files
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

#Data join
data_join <- full_join(unicef_metadata, unicef_indicator_1, by =c("country" = "country"))

data_join <- unicef_metadata %>%
  full_join(unicef_indicator_1, by = c("country", "year" = "time_period"))

#Map
installed.packages("maps")
library(maps)

map_world <- map_data("world")

#Vis 1
map_data_join <- full_join(unicef_indicator_1, map_world, by = c("country" = "region"))

#Plot the map 

ggplot(map_data_join) + 
  aes(x = long, y = lat, group = group, fill = obs_value) + 
  geom_polygon() + 
  scale_fill_gradient(low = "blue", high = "orange", name = "%PBSS") + 
  labs(x = "Longitude", y = "Latitude", title = "Proportion of Schools with Basic Sanitation") + 
  theme_void()

# vis 1.1 
ggplot(map_data_join) + 
  aes(x = long, y = lat, group = group, fill = obs_value) + 
  geom_polygon() + 
  scale_fill_gradient(low = "blue", high = "orange", name = "%PBSS") + 
  labs(x = "Longitude", y = "Latitude", title = "Proportion of Schools with Basic Sanitation") + 
  theme_void()

 #Vis 2
 p <- data_join %>%
   filter(country %in% c("Germany", "India", "Niger", "Mexico", "Afghanistan")) %>%
   ggplot(aes(x = time_period, y = obs_value, group = country, colour = country)) +
   geom_line() +
   labs(x = "Year", y = "Value", title = "Time Series by Country") + 
   theme(legend.position = "none")
  
 ggplotly(p)
 
 #Vis 3
vis_3 <- data_join %>%
  filter(time_period == 2020) %>%
  group_by(country) %>%
  summarise(
    Avg_GDP_per_capita = mean(`GDP per capita (constant 2015 US$)`, na.rm = TRUE),
    Obs_value_avg = mean(obs_value, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = Obs_value_avg, y = Avg_GDP_per_capita, color = country)) +
  geom_point() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 20000)) +
  labs(
    y = "Average GDP per capita (constant 2015 US$)",
    x = "Average Obs Value",
    title = "Average GDP per Capita by Country for 2020"
  )
ggplotly(vis_3)

#Vis 4

vis_4 <- data_join %>%
  filter(time_period == 2020, country %in% c("Germany", "India", "Niger", "Mexico", "Afghanistan")) %>%
  group_by(country) %>%
  summarise(Avg_obs_value = mean(obs_value, na.rm = TRUE)) %>%
  ggplot(aes(x = country, y = Avg_obs_value, fill = country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Average Obs Value", x = "Country", title = "Average Obs Value by Country for 2020") +
  theme(legend.position = "none")

ggplotly(vis_4)



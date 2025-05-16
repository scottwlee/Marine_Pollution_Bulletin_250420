##############################
### FL Keystone Taxa Plot ###
##############################

# Load required packages
library(dplyr)
library(ggplot2)
library(gridExtra)

# Set working directory (generalized path)
setwd("path/to/your/data")

# Load data (generalized filename)
data_FL <- read.csv("AllSS_FL_Key_RA.csv", check.names = FALSE)

# Check data structure
str(data_FL)

# Separate data by season and reorder taxa by average relative abundance
spring_data_FL <- data_FL %>% filter(SEASON == "Spring") %>% mutate(Key = reorder(Key, -AVG))
summer_data_FL <- data_FL %>% filter(SEASON == "Summer") %>% mutate(Key = reorder(Key, -AVG))
autumn_data_FL <- data_FL %>% filter(SEASON == "Autumn") %>% mutate(Key = reorder(Key, -AVG))
winter_data_FL <- data_FL %>% filter(SEASON == "Winter") %>% mutate(Key = reorder(Key, -AVG))

# Create bar plots for each season
plot_spring_FL <- ggplot(spring_data_FL, aes(x = Key, y = AVG)) +
  geom_bar(stat = "identity", fill = "#66c2a5") +
  geom_errorbar(aes(ymin = pmax(0, AVG - SD), ymax = AVG + SD), width = 0.2) +
  labs(title = "Spring", x = "Keystone taxa", y = "Relative abundance (%)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))

plot_summer_FL <- ggplot(summer_data_FL, aes(x = Key, y = AVG)) +
  geom_bar(stat = "identity", fill = "#fc8d62") +
  geom_errorbar(aes(ymin = pmax(0, AVG - SD), ymax = AVG + SD), width = 0.2) +
  labs(title = "Summer", x = "Keystone taxa", y = "Relative abundance (%)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))

plot_autumn_FL <- ggplot(autumn_data_FL, aes(x = Key, y = AVG)) +
  geom_bar(stat = "identity", fill = "#8da0cb") +
  geom_errorbar(aes(ymin = pmax(0, AVG - SD), ymax = AVG + SD), width = 0.2) +
  labs(title = "Autumn", x = "Keystone taxa", y = "Relative abundance (%)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))

plot_winter_FL <- ggplot(winter_data_FL, aes(x = Key, y = AVG)) +
  geom_bar(stat = "identity", fill = "#e78ac3") +
  geom_errorbar(aes(ymin = pmax(0, AVG - SD), ymax = AVG + SD), width = 0.2) +
  labs(title = "Winter", x = "Keystone taxa", y = "Relative abundance (%)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))

# Combine all seasonal plots into one figure
combined_plot_FL <- grid.arrange(
  plot_spring_FL, plot_summer_FL,
  plot_autumn_FL, plot_winter_FL,
  ncol = 2
)

# Visualization of Seasonal Robustness Curves for Free-Living (FL) Bacterial Networks

######################################################
# 1. Set Working Directory and Load Data
######################################################
# Set working directory (modify as needed)
setwd("path/to/your/project/folder")

# Load CSV data (replace with your actual filename)
data <- read.csv("FL_Robustness_Data.csv")

######################################################
# 2. Load Required Packages
######################################################
library(ggplot2)

######################################################
# 3. Create Robustness Plot by Season
######################################################
# Initialize ggplot with x-axis
p <- ggplot(data, aes(x = Fraction.of.nodes.removed)) +  
  # Spring
  geom_line(aes(y = Spring_mean, color = 'Spring'), size = 0.75) + 
  geom_errorbar(aes(ymin = Spring_mean - Spring_sd, ymax = Spring_mean + Spring_sd, color = 'Spring'), 
                width = 0.02, size = 0.5) + 
  # Summer
  geom_line(aes(y = Summer_mean, color = 'Summer'), size = 0.75) + 
  geom_errorbar(aes(ymin = Summer_mean - Summer_sd, ymax = Summer_mean + Summer_sd, color = 'Summer'), 
                width = 0.02, size = 0.5) + 
  # Autumn
  geom_line(aes(y = Autumn_mean, color = 'Autumn'), size = 0.75) + 
  geom_errorbar(aes(ymin = Autumn_mean - Autumn_sd, ymax = Autumn_mean + Autumn_sd, color = 'Autumn'), 
                width = 0.02, size = 0.5) + 
  # Winter
  geom_line(aes(y = Winter_mean, color = 'Winter'), size = 0.75) + 
  geom_errorbar(aes(ymin = Winter_mean - Winter_sd, ymax = Winter_mean + Winter_sd, color = 'Winter'), 
                width = 0.02, size = 0.5)

######################################################
# 4. Customize Labels and Theme
######################################################
p <- p + labs(
  x = 'Fraction of nodes removed',
  y = 'Size of largest component',
  title = 'Seasonal robustness curves of FL bacterial community networks'
) +
  ylim(0, 1)

# Final theme and color setup
fl_robustness <- p +
  scale_color_manual(
    values = c('Spring' = '#66c2a5', 'Summer' = '#fc8d62', 'Autumn' = '#8da0cb', 'Winter' = '#e78ac3'),
    breaks = c('Spring', 'Summer', 'Autumn', 'Winter')
  ) +
  guides(color = guide_legend(title = 'Season')) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 12, margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, margin = margin(r = 15)),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.box.margin = margin(t = 15),
    plot.margin = margin(30, 30, 30, 30),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )

# Print the plot
print(fl_robustness)

######################################################
# 5. Save Plot to File
######################################################
ggsave("FL_Robustness_Curve.tiff", fl_robustness, dpi = 300, width = 24, height = 20, units = "cm")

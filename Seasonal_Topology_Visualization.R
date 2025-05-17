###############################################
### Setup
###############################################

# Set working directory (generalized)
setwd("path/to/your/working/directory")
getwd()

# Load required libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(grid)

# Load topological metrics table (Free-Living, seasonal)
FL_topo <- read.csv("FL_Global_Network_Properties.csv", row.names = "Topology") %>% t()


###############################################
### Section: Topology_1 – Total Nodes (TN)
###############################################

TN <- c(FL_topo["Spring", "TN"], FL_topo["Summer", "TN"], FL_topo["Autumn", "TN"], FL_topo["Winter", "TN"])
seasons <- c("Spring", "Summer", "Autumn", "Winter")
data <- data.frame(seasons = factor(seasons, levels = seasons), TN = TN)

plot_TN <- ggplot(data, aes(x = seasons, y = TN, fill = seasons)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Total Nodes") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("Spring" = "#66C2A5", "Summer" = "#FC8D62",
                               "Autumn" = "#8DA0CB", "Winter" = "#e78AC3"))
ggsave("FL_plot_TN.tiff", plot_TN, width = 5, height = 5, dpi = 600)


###############################################
### Section: Topology_2 – Total Links (TL)
###############################################

TL <- c(FL_topo["Spring", "TL"], FL_topo["Summer", "TL"], FL_topo["Autumn", "TL"], FL_topo["Winter", "TL"])
data_TL <- data.frame(seasons = factor(seasons, levels = seasons), TL = TL)

plot_TL <- ggplot(data_TL, aes(x = seasons, y = TL, fill = seasons)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Total Links") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("Spring" = "#66C2A5", "Summer" = "#FC8D62",
                               "Autumn" = "#8DA0CB", "Winter" = "#e78AC3"))
ggsave("FL_plot_TL.tiff", plot_TL, width = 5, height = 5, dpi = 600)


###############################################
### Section: Topology_3 – Average Degree (avgK)
###############################################

avgK <- c(FL_topo["Spring", "avgK"], FL_topo["Summer", "avgK"], FL_topo["Autumn", "avgK"], FL_topo["Winter", "avgK"])
data_avgK <- data.frame(seasons = factor(seasons, levels = seasons), avgK = avgK)

plot_avgK <- ggplot(data_avgK, aes(x = seasons, y = avgK, fill = seasons)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "avgK") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("Spring" = "#66C2A5", "Summer" = "#FC8D62",
                               "Autumn" = "#8DA0CB", "Winter" = "#e78AC3"))
ggsave("FL_plot_avgK.tiff", plot_avgK, width = 5, height = 5, dpi = 600)


###############################################
### Section: Topology_4 – Average Clustering Coefficient (avgCC)
###############################################

avgCC <- c(FL_topo["Spring", "avgCC"], FL_topo["Summer", "avgCC"], FL_topo["Autumn", "avgCC"], FL_topo["Winter", "avgCC"])
data_avgCC <- data.frame(seasons = factor(seasons, levels = seasons), avgCC = avgCC)

plot_avgCC <- ggplot(data_avgCC, aes(x = seasons, y = avgCC, fill = seasons)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "avgCC") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("Spring" = "#66C2A5", "Summer" = "#FC8D62",
                               "Autumn" = "#8DA0CB", "Winter" = "#e78AC3"))
ggsave("FL_plot_avgCC.tiff", plot_avgCC, width = 5, height = 5, dpi = 600)


###############################################
### Section: Topology_5 – Modularity
###############################################

Modularity <- c(FL_topo["Spring", "Modularity"], FL_topo["Summer", "Modularity"],
                FL_topo["Autumn", "Modularity"], FL_topo["Winter", "Modularity"])
data_Modularity <- data.frame(seasons = factor(seasons, levels = seasons), Modularity = Modularity)

plot_Modularity <- ggplot(data_Modularity, aes(x = seasons, y = Modularity, fill = seasons)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Modularity") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("Spring" = "#66C2A5", "Summer" = "#FC8D62",
                               "Autumn" = "#8DA0CB", "Winter" = "#e78AC3"))
ggsave("FL_plot_Modularity.tiff", plot_Modularity, width = 5, height = 5, dpi = 600)


###############################################
### Section: Topology_6 – Relative Modularity (RM)
###############################################

RM <- c(FL_topo["Spring", "RM"], FL_topo["Summer", "RM"],
        FL_topo["Autumn", "RM"], FL_topo["Winter", "RM"])
data_RM <- data.frame(seasons = factor(seasons, levels = seasons), RM = RM)

plot_RM <- ggplot(data_RM, aes(x = seasons, y = RM, fill = seasons)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Relative Modularity") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("Spring" = "#66C2A5", "Summer" = "#FC8D62",
                               "Autumn" = "#8DA0CB", "Winter" = "#e78AC3"))
ggsave("FL_plot_RM.tiff", plot_RM, width = 5, height = 5, dpi = 600)


###############################################
### Section: Grid Arrangement of All Plots
###############################################

FL_topo_properties <- grid.arrange(plot_TN, plot_TL, plot_avgK, plot_avgCC, plot_RM,
                                   nrow = 1, ncol = 5,
                                   top = textGrob("Microbial Network Topological Properties (Free Living)",
                                                  gp = gpar(hjust = 0.5)))
# To save full combined plot as TIFF (uncomment below)
# ggsave("FL_topo_properties_combined.tiff", FL_topo_properties, width = 20, height = 5, dpi = 600)

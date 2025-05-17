###########################################
### FL ASV Relative Abundance by Season ###
###########################################

# Load required packages
library(vegan)
library(tidyverse)

# Set working directory (generalized)
setwd("path/to/your/directory")

###########################################
### [1] Spring Season
###########################################

# Load ASV table for Spring (filtered based on seasonal network)
fl_asv.table_spr <- read.csv("FL_Spring_ASV_Table.csv", header = TRUE, row.names = 1)
fl_asv.table_spr <- t(fl_asv.table_spr)
View(fl_asv.table_spr)

# Calculate row sums and normalize to get relative abundance (%)
row_sums <- rowSums(fl_asv.table_spr)
fl_asv.table_spr_relative <- fl_asv.table_spr / row_sums[row(fl_asv.table_spr)]
fl_asv.table_spr_relative_percent <- fl_asv.table_spr_relative * 100
View(fl_asv.table_spr_relative_percent)

# Save as CSV
write.csv(fl_asv.table_spr_relative_percent, file = "FL_Spring_RelAbundance_Percent.csv", row.names = TRUE)


###########################################
### [2] Summer Season
###########################################

fl_asv.table_sum <- read.csv("FL_Summer_ASV_Table.csv", header = TRUE, row.names = 1)
fl_asv.table_sum <- t(fl_asv.table_sum)
View(fl_asv.table_sum)

row_sums <- rowSums(fl_asv.table_sum)
fl_asv.table_sum_relative <- fl_asv.table_sum / row_sums[row(fl_asv.table_sum)]
fl_asv.table_sum_relative_percent <- fl_asv.table_sum_relative * 100
View(fl_asv.table_sum_relative_percent)

write.csv(fl_asv.table_sum_relative_percent, file = "FL_Summer_RelAbundance_Percent.csv", row.names = TRUE)


###########################################
### [3] Autumn Season
###########################################

fl_asv.table_aut <- read.csv("FL_Autumn_ASV_Table.csv", header = TRUE, row.names = 1)
fl_asv.table_aut <- t(fl_asv.table_aut)
View(fl_asv.table_aut)

row_sums <- rowSums(fl_asv.table_aut)
fl_asv.table_aut_relative <- fl_asv.table_aut / row_sums[row(fl_asv.table_aut)]
fl_asv.table_aut_relative_percent <- fl_asv.table_aut_relative * 100
View(fl_asv.table_aut_relative_percent)

write.csv(fl_asv.table_aut_relative_percent, file = "FL_Autumn_RelAbundance_Percent.csv", row.names = TRUE)


###########################################
### [4] Winter Season
###########################################

fl_asv.table_win <- read.csv("FL_Winter_ASV_Table.csv", header = TRUE, row.names = 1)
fl_asv.table_win <- t(fl_asv.table_win)
View(fl_asv.table_win)

row_sums <- rowSums(fl_asv.table_win)
fl_asv.table_win_relative <- fl_asv.table_win / row_sums[row(fl_asv.table_win)]
fl_asv.table_win_relative_percent <- fl_asv.table_win_relative * 100
View(fl_asv.table_win_relative_percent)

write.csv(fl_asv.table_win_relative_percent, file = "FL_Winter_RelAbundance_Percent.csv", row.names = TRUE)

############################################
### Package: NetSwan – FL Network Robustness
############################################

# Load required libraries
library(igraph)
library(NetSwan)
library(dplyr)

# Set working directory (generalized)
setwd("path/to/your/working/directory")


####################################################
### [1] Southern Region – Edge Table & Robustness
####################################################

# Load edge list for Southern (e.g., FL-Summer)
elec2 <- read.csv("FL_Summer_Edge-attribute.csv") %>%
  select(NF, NT) %>%
  as.matrix()

# Inspect the data
head(elec2)
class(elec2)
View(elec2)

# Construct igraph object and compute node-based metrics
gra <- graph.edgelist(elec2, directed = FALSE)
f <- swan_efficiency(gra)
vertex_attr(gra, "efficiency_loss", index = V(gra)) <- f
vertex_attr(gra)

# Closeness & betweenness
f2 <- swan_closeness(gra)
bet <- betweenness(gra)
reg <- lm(bet ~ f2)
summary(reg)

# Connectivity & combinatory robustness (Southern)
f3 <- swan_connectivity(gra)
f4 <- swan_combinatory(gra, 10)

# Plot cascading and betweenness node removal results (Southern)
robustness <- plot(f4[,1], f4[,4], type = 'o', col = 'dark green',
                   xlab = "Fraction of nodes removed", ylab = "Connectivity loss", cex = 0.5)
lines(f4[,1], f4[,2], type = 'o', col = 'green', cex = 0.5)


####################################################
### [2] Eastern Region – Edge Table & Robustness
####################################################

# Load edge list for Eastern (e.g., FL-Autumn)
elec3 <- read.csv("FL_Autumn_Edge-attribute.csv") %>%
  select(NF, NT) %>%
  as.matrix()

# Inspect the data
head(elec3)
class(elec3)
View(elec3)

# Construct igraph object and compute node-based metrics
gra2 <- graph.edgelist(elec3, directed = FALSE)
f5 <- swan_efficiency(gra2)
vertex_attr(gra2, "efficiency_loss", index = V(gra2)) <- f5
vertex_attr(gra2)

# Closeness & betweenness
f6 <- swan_closeness(gra2)
bet2 <- betweenness(gra2)
reg2 <- lm(bet2 ~ f6)
summary(reg2)

# Connectivity & combinatory robustness (Eastern)
f7 <- swan_connectivity(gra2)
f8 <- swan_combinatory(gra2, 10)

# Plot cascading and betweenness node removal results (Eastern)
lines(f8[,1], f8[,4], type = 'o', col = 'brown', cex = 0.5)
lines(f8[,1], f8[,2], type = 'o', col = 'orange', cex = 0.5)


############################################
### [3] Legend & Export Option (Optional)
############################################

legend('bottomright',
       c("Cascading Light-yellow (Eastern)", "Betweenness Light-yellow (Eastern)",
         "Cascading Dark-green (Southern)", "Betweenness Dark-green (Southern)"),
       lty = c(1,1,1,1), pch = c(1,1,1,1),
       col = c("brown", "orange", "dark green", "green"))

# Optional: Save plot as TIFF (uncomment to activate)
# tiff("Robustness_final.tiff", width = 6, height = 6, units = "in", res = 600)
# plot(f4[,1], f4[,4], type='o', col='dark green', xlab="Fraction of nodes removed",
#      ylab="Connectivity loss", cex=0.5)
# lines(f4[,1], f4[,2], type='o', col='green', cex=0.5)
# lines(f8[,1], f8[,4], type='o', col='brown', cex=0.5)
# lines(f8[,1], f8[,2], type='o', col='orange', cex=0.5)
# legend('bottomright', c("Cascading Light-yellow (Eastern)", "Betweenness Light-yellow (Eastern)",
#                         "Cascading Dark-green (Southern)", "Betweenness Dark-green (Southern)"),
#        lty = c(1,1,1,1), pch = c(1,1,1,1),
#        col = c("brown", "orange", "dark green", "green"))
# dev.off()

############################################################
# Cohesion Metric Calculation Script
# Adapted from Herren & McMahon (ISME J, 2017)
# Modified for general use and GitHub publication
############################################################

###############################
# 1. Set Working Directory
# - Define the folder containing input data and where output will be saved
###############################
setwd("path/to/your/project/folder")
getwd()

###############################
# 2. Define Utility Functions
# - Functions for counting zeros and calculating mean of positive/negative values
###############################
zero <- function(vec) {
  return(length(which(vec == 0)))
}

neg.mean <- function(vector) {
  neg.vals <- vector[which(vector < 0)]
  return(ifelse(length(neg.vals) == 0, 0, mean(neg.vals)))
}

pos.mean <- function(vector) {
  pos.vals <- vector[which(vector > 0)]
  return(ifelse(length(pos.vals) == 0, 0, mean(pos.vals)))
}

###############################
# 3. Analysis Parameters
# - Set persistence threshold, number of iterations, and shuffling method
###############################
pers.cutoff <- 0.10    # Minimum fraction of presence to retain a taxon
iter <- 75             # Number of iterations for null model
tax.shuffle <- TRUE    # Shuffle taxa (TRUE) or rows (FALSE)
use.custom.cors <- FALSE  # Whether to use a custom correlation matrix

###############################
# 4. Load Input Data
# - Read the sample table and normalize to relative abundance
###############################
input_data <- read.csv("your_data_file.csv", header = TRUE, row.names = 1)
total_counts <- rowSums(input_data)
b <- input_data / total_counts * 100  # Convert to relative abundance

###############################
# 5. (Optional) Load Custom Correlation Matrix
###############################
if (use.custom.cors) {
  custom.cor.mat <- as.matrix(read.csv("your_custom_correlation_matrix.csv", header = TRUE, row.names = 1))
  stopifnot(dim(b)[2] == dim(custom.cor.mat)[2])
}

###############################
# 6. Data Cleaning
# - Remove samples and taxa with all zeros; apply persistence cutoff
###############################
c <- as.matrix(b)
c <- c[rowSums(c) > 0, colSums(c) > 0]
rowsums.orig <- rowSums(c)
zero.cutoff <- ceiling(pers.cutoff * nrow(c))
d <- c[, apply(c, 2, zero) < (nrow(c) - zero.cutoff)]
d <- d[rowSums(d) > 0, ]
rel.d <- d / rowsums.orig  # Final relative abundance matrix

if (use.custom.cors) {
  custom.cor.mat.sub <- custom.cor.mat[
    apply(c, 2, zero) < (nrow(c) - zero.cutoff),
    apply(c, 2, zero) < (nrow(c) - zero.cutoff)
  ]
}

###############################
# 7. Observed Correlation Matrix
###############################
cor.mat.true <- cor(rel.d)

###############################
# 8. Null Model Simulation
# - Generate expected correlation values through permutation
###############################
med.tax.cors <- vector()

if (!use.custom.cors) {
  if (tax.shuffle) {
    for (which.taxon in 1:ncol(rel.d)) {
      perm.cor.vec.mat <- vector()
      for (i in 1:iter) {
        perm.rel.d <- matrix(numeric(0), nrow(rel.d), ncol(rel.d))
        rownames(perm.rel.d) <- rownames(rel.d)
        colnames(perm.rel.d) <- colnames(rel.d)
        for (j in 1:ncol(rel.d)) {
          perm.rel.d[, j] <- sample(rel.d[, j])
        }
        perm.rel.d[, which.taxon] <- rel.d[, which.taxon]
        cor.mat.null <- cor(perm.rel.d)
        perm.cor.vec.mat <- cbind(perm.cor.vec.mat, cor.mat.null[, which.taxon])
      }
      med.tax.cors <- cbind(med.tax.cors, apply(perm.cor.vec.mat, 1, median))
      if (which.taxon %% 20 == 0) print(which.taxon)
    }
  } else {
    for (which.taxon in 1:ncol(rel.d)) {
      perm.cor.vec.mat <- vector()
      for (i in 1:iter) {
        perm.rel.d <- rel.d
        for (j in 1:nrow(rel.d)) {
          which.replace <- which(rel.d[j, ] > 0)
          which.replace.nonfocal <- setdiff(which.replace, which.taxon)
          perm.rel.d[j, which.replace.nonfocal] <- sample(rel.d[j, which.replace.nonfocal])
        }
        cor.mat.null <- cor(perm.rel.d)
        perm.cor.vec.mat <- cbind(perm.cor.vec.mat, cor.mat.null[, which.taxon])
      }
      med.tax.cors <- cbind(med.tax.cors, apply(perm.cor.vec.mat, 1, median))
      if (which.taxon %% 20 == 0) print(which.taxon)
    }
  }
}

###############################
# 9. Compute Observed - Expected Correlation Differences
###############################
obs.exp.cors.mat <- if (use.custom.cors) custom.cor.mat.sub else cor.mat.true - med.tax.cors
diag(obs.exp.cors.mat) <- 0

###############################
# 10. Calculate Connectedness and Cohesion
###############################
connectedness.pos <- apply(obs.exp.cors.mat, 2, pos.mean)
connectedness.neg <- apply(obs.exp.cors.mat, 2, neg.mean)
cohesion.pos <- rel.d %*% connectedness.pos
cohesion.neg <- rel.d %*% connectedness.neg

###############################
# 11. Export Output
# - Save connectedness and cohesion metrics as CSV
###############################
output <- list(connectedness.neg, connectedness.pos, cohesion.neg, cohesion.pos)
names(output) <- c("Negative Connectedness", "Positive Connectedness", "Negative Cohesion", "Positive Cohesion")

output_df1 <- as.data.frame(output[c("Negative Connectedness", "Positive Connectedness")])
output_df2 <- as.data.frame(output[c("Negative Cohesion", "Positive Cohesion")])

write.csv(output_df1, file = "Connectedness_Output.csv", row.names = TRUE)
write.csv(output_df2, file = "Cohesion_Output.csv", row.names = TRUE)

# 1. Set Working Directory: Define the working folder
# 2. Define Utility Functions: Support for counting and averaging values
# 3. Analysis Parameters: Configure persistence and randomization settings
# 4. Load Input Data: Read abundance data and normalize
# 5. Load Custom Matrix (Optional): Use user-defined correlation matrix if enabled
# 6. Data Cleaning: Apply persistence filter and remove empty data
# 7. Observed Correlation Matrix: Compute baseline taxon correlations
# 8. Null Model Simulation: Permute data to estimate expected correlations
# 9. Observed vs. Expected: Calculate difference matrix
# 10. Compute Metrics: Derive connectedness and cohesion values
# 11. Export Output: Save results to CSV

# Load necessary libraries
library(ggplot2)
library(cluster)
library(factoextra)
library(gridExtra)

# Load data
Trainp_data <- read.csv("Trainp.csv")
str(Trainp_data)

# Check for missing values
colSums(is.na(Trainp_data))

# Select numerical columns (update the range if needed)
numerical_data <- Trainp_data[, 1:88]
head(numerical_data)

# Normalize the data
data_normalized <- scale(numerical_data)
head(data_normalized)

# Perform PCA
pca_result <- prcomp(data_normalized, scale. = FALSE)

# Calculate variance explained and eigenvalues
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
eigenvalues <- pca_result$sdev^2

# Create a data frame for variance and eigenvalues
variance_df <- data.frame(
  PC = paste0("PC", 1:length(variance_explained)),
  Variance = variance_explained,
  Eigenvalue = eigenvalues
)

# Plot variance explained
variance_plot <- ggplot(variance_df, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Variance Explained by Principal Components", x = "Principal Component", y = "Variance Explained") +
  theme_minimal()

# Plot eigenvalues
eigenvalue_plot <- ggplot(variance_df, aes(x = PC, y = Eigenvalue)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Eigenvalues of Principal Components", x = "Principal Component", y = "Eigenvalue") +
  theme_minimal()

# Perform k-means clustering with 4 clusters on the PCA result
set.seed(123)
kmeans_result <- kmeans(pca_result$x[, 1:4], centers = 4, nstart = 25) # Use first 4 PCs for clustering

# Add cluster information to the PCA result
pca_df <- data.frame(pca_result$x[, 1:2], Cluster = as.factor(kmeans_result$cluster)) # Use first 2 PCs for visualization

# Visualize the clustering result using ggplot2
cluster_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "PCA with 4 Clusters", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Combine the plots into one frame
grid.arrange(variance_plot, eigenvalue_plot, cluster_plot, ncol = 2)

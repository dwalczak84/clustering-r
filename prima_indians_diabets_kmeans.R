# Performing k-means clustering on Pima Indians Diabetes Data Set

# We import the data from:
# http://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
Data <- read.table(url, sep = ",", header = FALSE)

# predicton variables are column 1 through 8.
# responce variable is the last column: 9.
pred_X <- Data[, 1:8]
resp_Y <- Data[, 9]

# principal components analysis using correlation matrix
pca <- princomp(pred_X, cor = TRUE) 

# principal component 5 scores (negated for convenience)
pc_scr5 <- pca$scores[, 5] 

# principal component 6 scores (negated for convenience)
pc_scr6 <- pca$scores[, 6] 

# 2) K-Means

# In R, kmeans performs the K-means clustering analysis, ()$cluster provides 
# the clustering results and ()$centers provides the centroid vector (i.e., 
# the mean) for each cluster.

X <- cbind(pc_scr5, pc_scr6)

# Initialise ratio_ss
ratio_ss <- rep(0, 25)

# Clustering on X with different values of k.
for (k in 1:25) {
  # Apply k means to X: X_km
  X_km <- kmeans(X, k, nstart = 1)
  
  # Save the ratio between of WSS (Within Sum of Squares) to TSS 
  # (Total Sum of Squares) in kth element of ratio_ss
  ratio_ss[k] <- X_km$tot.withinss / X_km$totss
}

# Make a scree plot with type "b" and xlab "k"
plot(ratio_ss, type = "b", xlab = "k")

# There is no major improvement in ratio_ss when k is > 13
# Will provide a meaningful clustering with overall compact and separeted 
# clusters.

# We choose k = 10 and perform kmeans on X with that k:
k <- 13
cl <- kmeans(X, k, nstart = 1)
cl$cluster

plot(pc_scr5, pc_scr6, col = cl$cluster)

# Mark the centres of the clusters - solid black boxes:
points(cl$centers, pch = 15)

setwd("E:/MNCH 74-75W Report to press basnet/PCA analyis Folder")
Biku<-read.csv("E:/MNCH 74-75W Report to press basnet/PCA analyis Folder/PCA anaylis of the unused data set of over Location with wrong value of RLOD and SlOD.csv")
attach(Biku)
print(Biku)
View(Biku)
Elimin<-Biku[,-c(1,1)]
Elimin
options(max.print = 1000)
options(scipen = 1000)
library(factoextra)
library(ggplot2)
fviz_nbclust(Elimin, kmeans, method = "wss")
fviz_nbclust(Elimin, kmeans, method = "silhouette")
library(NbClust)
NbClust(data = Elimin, diss =NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        
        method = "kmeans" , index = "all", alphaBeale = 0.1)
setk<-kmeans(Elimin, 3)

library(NbClust)
NbClust(data = Elimin, diss =NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "ward.D2" , index = "all", alphaBeale = 0.1)

any(is.na(Elimin))
Elimin_clean<-na.omit(Elimin)
?hclust
# Convert the data frame to a distance matrix
dist_matrix<-dist(Elimin_clean, method = "euclidean")

# Cluster the data
ward_clusters<- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram with the original name of the genotypes
plot(ward_clusters, labels = Biku$Gen, srt = 90, cex = .6, main = "Dendrogram Representation of Maize Genotypes Parameters")
rect.hclust(ward_clusters, k=8, border = "purple")
rect.hclust(ward_clusters, k=8, border = 2:6)
#to calculate the disimilarities coefficient of variations

# Load the required library
library(fpc)

# Calculate the dissimilarity matrix
distance <- dist(Elimin)

# Perform hierarchical clustering
hc <- hclust(distance)

# Define a custom function to calculate the dissimilarity coefficient of variation
dissimilarity_coef_var <- function(dist_matrix, hclust_object, k) {
  cluster_sizes <- table(cutree(hclust_object, k = k))
  total_dissimilarity <- sum(dist_matrix)
  within_cluster_dissimilarity <- 0
  for (i in 1:k) {
    cluster_indices <- which(cutree(hclust_object, k = k) == i)
    within_cluster_dissimilarity <- within_cluster_dissimilarity + sum(dist_matrix[cluster_indices])
  }
  return(within_cluster_dissimilarity / total_dissimilarity)
}

# Calculate the dissimilarity coefficient of variation for k=4
k <- 4
coef_var <- dissimilarity_coef_var(distance, hc, k)

# Print the coefficient of variation
print(coef_var)



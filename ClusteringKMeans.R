#CLUSTERING K-MEANS ALGORITHM
iris_data <- iris[1:4] #remove last column
iris_data <- as.matrix(iris_data)
iris_cluster <- kmeans(iris_data, 3) # Select number of clusters
cluster_data <- cbind(iris, iris_cluster$cluster)
View(cluster_data)
#We can see all setosa specie has been gruoped in cluster 2 and the rest in cluster 1 and 2
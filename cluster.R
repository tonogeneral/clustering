#Gráfico en cluster
#Creado por Gabriel General
#22-12-2020 01:05

set.seed(1234)
par(mar = c(0,0,0,0))
x <-rnorm(12, mean = rep(1:3,each = 4), sd = 0.2)
y <-rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot(x,y, col = "blue",pch = 19, cex =2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))



# CLUSTERING JERÁRQUICO POR DISTANCIA EUCLIDIANA.
dataframe <- data.frame(x = x, y = y )
#Calcula la distancia entre los puntos de un dataframe
dist(dataframe)


#Clustering jerárquico con Hclust
dataframe <- data.frame(x = x, y = y )
distxy <- dist(dataframe)
hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each =4))


#HEATMAP
dataframe <- data.frame( x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataframe)[sample(1:12),]
heatmap(dataMatrix)


#KMEANS con agreupación de puntos
dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
kmeansObj$cluster


par(mar = rep(0.2,4))
plot(x,y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)


#KMEANS GRAFICADO CON HEATMAPS
set.seed(1234)
dataMatrix <- as.matrix(dataframe)[sample(1:12),]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1,2), mar = c(2,4,0.1,0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj2$cluster)], yaxt = "n")





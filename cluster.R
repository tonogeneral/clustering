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


#Matriz de datos con heatmap
set.seed(12345)
par(mar = rep(0.2,4))
dataMatrix <- matrix(rnorm(400),nrow = 40)
image(1:10,1:40, t(dataMatrix)[,nrow(dataMatrix):1])

#cluster data
heatmap(dataMatrix)






#agragamos un patrón al conjunto de datos
set.seed(678910)
for (i in 1:40) {
  #es como tirar una moneda
  coinflip <- rbinom(1, size = 1, prob = 0.5)
  # si sale cara agrega un patrón común a esa columna
  if (coinflip) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0.3), each = 5)
  }
}

par(mar = rep(0.2,4))
image(1:10,1:40, t(dataMatrix)[,nrow(dataMatrix):1])
par(mar = rep(0.2,4))
heatmap(dataMatrix)


#PATRONES EN FILAS Y COLUMNAS
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1, xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)


###### SVD (Singular vector) y PCA (Principal Components)

#Reducción de dimensiones (Componentes u,v  de SVD)
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First of left singular vector", pch = 19)
plot(svd1$v[,1], xlab = "Column", ylab = "First right singular vector", pch = 19)


#Componentes de SVD --- Varianza explicada
par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop of variance explained", pch = 19)


#Relacionamiento entre los principales componentes
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[,1], svd1$v[,1], pch = 19, xlab = "Componente principal 1", ylab = "Right Singular vector 1")
abline(c(0,1))


#Purgando valores faltantes (Missing values)
# Usando Impute

library(impute)
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size = 40,replace = FALSE)] <-NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow = c(1,2));plot(svd1$v[,1],pch = 19); plot(svd2$v[,1], pch = 19)



#Face example for variance explained
load("data/face.rda")
image(t(faceData)[,nrow(faceData):1])

svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Vector singular", ylab = "Varianza explicada")


#aproximaciones

svd1 <- svd(scale(faceData))
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]

approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])

#plot approximations
par(mfrow = c(1,4))
image(t(approx1)[, nrow(approx1):1], main ="(a)")
image(t(approx5)[, nrow(approx1):1], main ="(b)")
image(t(approx10)[, nrow(approx1):1], main ="(c)")
image(t(faceData)[, nrow(approx1):1], main ="(d)")
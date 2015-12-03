# exemplo de gráfico 3D rotacional
library(mclust)
my.prev <- as.data.frame(predict(pca1, newdata=my.newdata))
my.prev <- cbind(my.newdata, PC1 = my.prev$PC1, PC2 = my.prev$PC2, PC3 = my.prev$PC3)
#fit <- Mclust(my.prev[1:4], G=4, modelNames = "EEV")
plot3d(my.prev[1:4], col = my.prev$TIPOUSER)

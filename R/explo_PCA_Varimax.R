# exemplo de PCA + Varimax rotation com Psysh and prcomp packages com memso resultado
library(pracma)
library (psych)
irisX <- iris[2:nrow(iris), 1:4]      # Iris data
ncomp <- 2

pca_iris_rotated <- psych::principal(irisX, rotate="varimax", nfactors=ncomp, scores=TRUE)
print(pca_iris_rotated$scores[1:5,])  # Scores returned by principal()

pca_iris        <- prcomp(irisX, center=T, scale=T)
rawLoadings     <- pca_iris$rotation[,1:ncomp] %*% diag(pca_iris$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(irisX) %*% invLoadings
print(scores[1:5,])                   # Scores computed via rotated loadings

scores <- scale(pca_iris$x[,1:2]) %*% varimax(rawLoadings)$rotmat
print(scores[1:5,])                   # Scores computed via rotating the scores

# exemplo de gráfico de dados reais mesclados com previsões
# ideia: usar as cores para prever:KROTON (estudante), AMBEV (Empresa privada), FUNC PUBLICO (empresa pública)
###pca - calculated for the first 4 columns of the data set that correspond to biometric measurements ("Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width")
data(iris)
dat <- as.matrix(iris[,-5])
pca <- prcomp(dat, retx=TRUE, center=TRUE, scale=TRUE)

###Create new data sets for each of the three species. 
#Biometric values are based on the distributions of the original data means
#and the covariances between these parameters. 
setosa.mean <- apply(iris[iris$Species=="setosa",-5], 2, mean)
setosa.cov <- cov(iris[iris$Species=="setosa",-5])

versicolor.mean <- apply(iris[iris$Species=="versicolor",-5], 2, mean)
versicolor.cov <- cov(iris[iris$Species=="versicolor",-5])

virginica.mean <- apply(iris[iris$Species=="virginica",-5], 2, mean)
virginica.cov <- cov(iris[iris$Species=="virginica",-5])

#Make new random data based on the calculated biometry info. each species
#The MASS package allows for the calculation of correlated/covarying random 
#numbers using this information.
require(MASS)
set.seed(1)
n <- 30
new.setosa <- mvrnorm(n, setosa.mean, setosa.cov)
new.versicolor <- mvrnorm(n, versicolor.mean, versicolor.cov)
new.virginica <- mvrnorm(n, virginica.mean, virginica.cov)

###Predict PCs by projecting the new data using the predict.prcomp function
pred.setosa <- predict(pca, new.setosa)
pred.versicolor <- predict(pca, new.versicolor)
pred.virginica <- predict(pca, new.virginica)

###Plot result
SPP <- iris$Species
COLOR <- c(2:4)
PCH <- c(1,16)

pc <- c(1,2)
plot(pca$x[,pc[1]], pca$x[,pc[2]], col=COLOR[SPP], cex=PCH[1], xlab=paste0("PC ", pc[1], " (", round(pca$sdev[pc[1]]/sum(pca$sdev)*100,0), "%)"), ylab=paste0("PC ", pc[2], " (", round(pca$sdev[pc[2]]/sum(pca$sdev)*100,0), "%)"))
points(pred.setosa[,pc[1]], pred.setosa[,pc[2]], col=COLOR[levels(SPP)=="setosa"], pch=PCH[2])
points(pred.versicolor[,pc[1]], pred.versicolor[,pc[2]], col=COLOR[levels(SPP)=="versicolor"], pch=PCH[2])
points(pred.virginica[,pc[1]], pred.virginica[,pc[2]], col=COLOR[levels(SPP)=="virginica"], pch=PCH[2])
legend("topright", legend=levels(iris$Species), col=COLOR, pch=17)
legend("topleft", legend=c("Original data", "New data"), col=1, pch=PCH)
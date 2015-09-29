# FASE 2: parametric modeling (após feature selection realizada)
# linear discriminant
# obtain data
# APLICAR COM IRIS
# pot data for analysis
require(MASS)
require(ggplot2)
require(scales)

# plot de pares para visualizar separação entre classes
# 3,4 ou 2,4 melhor
plot(iris[,c(3,4)],
     col=iris[,5])

# perform the LDA (Linear Disctiminant Analysis)
# Target must be categorical!!!!!!
# Features devem ser numéricos ou se categóricos, convertidos para números
iris.lda <- lda( Species ~ .,
                  data = iris)
# amount of the between-group variance that is explained by each linear discriminant
# ou seja, como percentualmente cada linear discriminant (LD) contribui para explicar
# a variância entre grupos
prop.lda = iris.lda$svd^2/sum(iris.lda$svd^2)
# obtain prediction (class)
iris.lda.p <- predict(iris.lda,
                       newdata = iris
                       )

# determine how well the model fits
table(iris.lda.p$class, iris[,5])

# Cross validade the model
# perform LDA in data1
iris.lda.2 <- lda( Species ~ .,
                    data = iris,
                    CV = TRUE)

# look at the assigned classes for observation
table(iris.lda.2$class, iris[,5])

# plota distribuição de linear discriminants
ds <- data.frame(species = iris[,"Species"],
                 lda = iris.lda.p$x)

p1 <- ggplot(ds) + geom_point(aes(iris.lda.p$x[,1], iris.lda.p$x[,2], colour = species, shape = species), size = 2.5) + 
    labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
         y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))


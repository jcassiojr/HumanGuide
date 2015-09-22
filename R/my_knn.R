# k-NN k-nearest neighbour
library (dplyr)
library(tm)
# creating a data.frame with a sample instance
Sepal.Length <- 4.8
Sepal.Width <- 2.9
Petal.Length <- 3.7
Petal.Width <- 1.7
new_sample <- data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
# selecting only the features needed
iris_features <-
    iris %>%
        select(Sepal.Length,Sepal.Width,Petal.Length, Petal.Width)
# função para calcular distância euclidiana en dois data.frames
dist_eucl <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
# calcula as distâncias euclidianas entre as features (colunas do dataframe) e a coluna da amostra
distances <- apply(iris_features, 1, function(x) dist_eucl(x, new_sample))
# ordena para rankear mais próximos
distances_sorted <- sort(distances, index.return = T)
str(distances_sorted)
# obtem as cinco linhas (k = 5) com menor distância em relação a amostra
nn_5 <- iris[distances_sorted$ix[1:5],]
nn_5


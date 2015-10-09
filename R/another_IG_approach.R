# another information gain approach to select atributes

# seleciona atributos que tem maior information gain
library(FSelector)
data(iris)
result <- cfs(Species ~ ., iris)
f <- as.simple.formula(result, "Species")

# another
library(mlbench)
data(HouseVotes84)
# selecionafeature com mais information gain
weights <- chi.squared(Class~., HouseVotes84)
print(weights)
# seleciona os 5 maiores
subset <- cutoff.k(weights, 5)
# mota a fórmula com os 5 maiores
f <- as.simple.formula(subset, "Class")
print(f)
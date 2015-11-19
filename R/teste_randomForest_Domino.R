# teste de deply com exemplo de Ramdon Forest
library(randomForest)
# carregar aqui algum file
df$quality <- factor(df$quality)
# aqui pega somente as colunas de features (todas numéricas)
# a coluna de target é um ranking
cols <- names(df)[1:11]
clf <- randomForest(df$quality ~ ., data = df[,cols], ntree = 40, node ????)
# como salvar est emodelo!!!??? Funciona para caret?
save (clf, file = "classifier.Rda")
save(cols, file ="cols.RDA")


# e outro arquvo para a previsão
library(randomForest)
load("classifier.Rda")
load("cols.Rda")
predictQuality <- function(features) {
    to_predict <- as.data.frame(features)
    colnames(to_predict) <-cols
    prediction <- predict(clf, to_predict)
    cbind(prediction)[1]
}
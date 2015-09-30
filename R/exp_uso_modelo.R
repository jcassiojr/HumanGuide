# uso do modelo com exemplo usando iris dataset
# aplicar o modelo a um exemplo para ver a probabilidade dele fazer parte da classe
# usando regressão logística
testidx <- which(1:length(iris[,1]) %% 5 == 0)
# cria dados para treino a partir dos índices que não são múltiplos de 5
iristrain <- iris[-testidx,]
# cria dados para teste a partir dos índices que são múltiplos de 5
iristest <- iris[testidx,]

# cria data.frame com uma coluna TRUE/FALSE para Setosa
newcol <- data.frame(isSetosa=(iristrain$Species == 'setosa')) 
# mescla esta coluna no dataframe de treino
traindata <- cbind(iristrain, newcol)
# mostra o inicio do dataset
head(traindata)
# cria fórmula de regressão (símbolo ~) para este trget variable
formula <- isSetosa ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width
# PODERIA SER ASSIM TB formula <- isSetosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
# aplica o modelo logistico
logisticModel <- glm(formula, data=traindata, family='binomial') 
logisticModel
# aplicando forward selection
fs <- step(glm(formula, data=traindata, family='binomial'))
fs

# escolhendo o melhor modelo (menor AIC (Akaike Information Criterion))
bestFormula <- isSetosa ~ Petal.Length
bestLogisticModel <- glm(formula, data=traindata, family='binomial')
# modelo somente com Petal.Length é o de melhor fitting!!!
# Predict the probability for test data
#prob <- predict(logisticModel, newdata=iristest, type= 'response') 
#round(prob, 3)

# prediction for one example
# Petal.Length = 4.4 (modelo deveria dar FALSE para setosa)
# Petal.Length = 1.4 (modelo deveria dar TRUE para setosa)

prediction = 77.85 + (-32.74 * 1.4)
print(prediction)

# At this point we have the logit value.  We need to get it 
# back to a probability by exp(logit) to get odds, and then
# p = Odds / Odds+1.  And then we're done!  

odds = exp(prediction)

print (odds)

prob = odds / (odds + 1)
print(prob)

# criar um plot para ver a distribuição de probabilidades 
# para identificar Species = setosa
petal_L <- seq(from = 0.1, to = 7, by = 0.01)
pred_val <- NULL
for (i in 1:length(petal_L)) {
    prediction <- 77.85 + (-32.74 * petal_L[i])
    odds = exp(prediction)
    pred_val[i] <- odds / (odds + 1)
} 
# plotando as probabilidades de acordo com os valores de Petal.length
plot(petal_L,pred_val)

# obtendo previsão
predictions <- predict(bestLogisticModel, iris[,1:4])
#setosaPredictions = ifelse(predictions > 0,1,0)
#table(predictions,traindata$isSetosa)

## Data preparation
# PAREI AQUI: rodar de novo passo a passo até a linha 
library("caret")
source("./R/f_simula_dados_HG2.R")
# carrega dados simulados de Human Guide
df_hg <- f_simula_dados_HG2()
turnover <- df_hg[,1] # transformando em vetor de fatores
descr <- df_hg[,-1] # transformando em vetor de fatores

set.seed(1)
#inTrain <- createDataPartition(mutagen, p = 3/4, list = FALSE)
inTrain <- createDataPartition(turnover, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]

trainClass <- turnover[inTrain]
testClass  <- turnover[-inTrain]

#prop.table(table(mutagen))
prop.table(table(turnover))
prop.table(table(trainClass))

ncol(trainDescr)

# PULANDO ESTA PARTE POR ENQUANTO
# Remove predictors with one distinct value
isZV <- apply(trainDescr, 2, function(u) length(unique(u)) == 1)
trainDescr <- trainDescr[, !isZV]
testDescr  <-  testDescr[, !isZV]

# -------- ANALISANDO E ELIMINANDO CORRELAÇÕES ENTRE FEATURES
# Plot #1: Basic scatterplot matrix of the four measurements
library(corrplot)
M <- cor(df_hg[,-1])
corrplot.mixed(M)
# mostrando teste de significância
cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
            uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(mtcars, 0.95)
res2 <- cor.mtest(mtcars, 0.99)
## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1[[1]], sig.level = 0.2)

# If using "hclust", corrplot() can draw rectangles around the chart of 
# corrrelation matrix based on the results of hierarchical clustering.

# usin anither color scheme
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
                           "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", 
                           "cyan", "#007FFF", "blue", "#00007F"))
wb <- c("white", "black")

## using these color spectrums
corrplot(M, order = "hclust", addrect = 2, col = col1(100))
#corrplot(M, order = "hclust", addrect = 2)
## leave blank on no significant coefficient
corrplot(M, p.mat = res1[[1]], insig = "blank")
## add p-values on no significant coefficient
corrplot(M, p.mat = res1[[1]], insig = "p-value")
## add all p-values
corrplot(M, p.mat = res1[[1]], insig = "p-value", sig.level = -1)




descrCorr <- cor(trainDescr)

# elimina colunas de features com correlação par-wise acima de 0.01, p. ex.
highCorr <- findCorrelation(descrCorr, 0.01)

trainDescr <- trainDescr[, -highCorr]
testDescr  <-  testDescr[, -highCorr]
ncol(trainDescr)

#------------

# ----- NORMALIZANDO OS DADOS
# normaliza os dados. É melhor chamar esta função
# dentro da função trans()
# abaixo, somente normaliza os dados dos sets de train e test
xTrans <- preProcess(trainDescr)
trainDescr <- predict(xTrans, trainDescr)
testDescr  <- predict(xTrans,  testDescr)

## Building and tuning models (métodos para regression (ou dual) in caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3,classProbs = TRUE)
# no caso abaixo, usa método bootstrap como default
# bootControl <- trainControl(number = 200)
# SVM (Dual) usando Acurácia
set.seed(2)
svmFit <- train(
                trainDescr, trainClass,
                method = "svmRadial",
                tuneLength = 5,
                trControl = control,
                scaled = FALSE)
svmFit
svmFit$finalModel
# Stochastic Gradient Boost
# At each step of the GBM algorithm, a new decision tree is constructed. 
# The question when growing a decision tree is 'when to stop?'. The 
# furthest you can go is to split each node until there is only 1 
# observation in each terminal node. This would correspond to 
# n.minobsinnode=1. Alternatively, the splitting of nodes can cease 
# when a certain number of observations are in each node. The default 
# for the R GBM package is 10.
gbmGrid <- expand.grid(
                       .interaction.depth = (1:5) * 2,
                       .n.trees = (1:10)*25,
                       .shrinkage = .1,
                       .n.minobsinnode = 1)
set.seed(2)
gbmFit <- train(
                trainDescr, trainClass,
                method = "gbm",
                trControl = control,
                verbose = FALSE,
                bag.fraction = 0.5,                
                tuneGrid = gbmGrid)

gbmFit
gbmFit$finalModel
## Prediction of new samples
# retorna o valor previsto pelo modelo para os dados de teste
# obs. pode ser usado como saída, juntamente com o id de cada candidato 
# montado no mesmo dataframe com id do candidato
x <- predict(svmFit$finalModel, newdata = testDescr)[1:5]
predict(gbmFit$finalModel, newdata = testDescr)[1:5]
# predizendo probabilidade de ser da classe 
predict(svmFit$finalModel,newdata = testDescr, type = "prob")
#predict(svmFit, newdata = testDescr)[1:5]
#predict(svmFit, newdata = testDescr)

# consolidando previsões de diversos modelos eu um output
models <- list(svm = svmFit,
               gbm = gbmFit)
testPred <- predict(models, newdata = testDescr)

#lapply(testPred,
#       function(x) x[1:5])
predValues <- extractPrediction(
                                models,
                                testX = testDescr,
                                testY = testClass)
# obtém somente o subset de dados de treino
testValues <- subset(
                     predValues,
                     dataType == "Test")
head(testValues)
table(testValues$model)
nrow(testDescr)

# CONFUSION MATRIX ----- TESTAR!!
tb_cf <- confusionMatrix(x, trainClass)
print (tb_cf$table) # confusion matrix as a table
print (tb_cf$byClass) # estatístics as a matrix
print (tb_cf$overall) # acuracy as a numeric vector

# plota previsto x observado
#---------- ADENDO --------------------------------
# abaixo está um excelente exemplo para classification
library(mlbench)
#classification example
data(Satellite)
numSamples <- dim(Satellite)[1]
set.seed(716)
# extrai índices de linhas para amostra aleatória dos dados de tamanho 150 para train
varIndex <- 1:numSamples
trainSamples <- sample(varIndex, 150)
# extrai índices de linhas para amostra aleatória dos dados restantes de tamanho 100 para test
varIndex <- (1:numSamples)[-trainSamples]
testSamples <- sample(varIndex, 100)
# extrai índices de linhas para amostra aleatória dos dados restantes de tamanho 50 para target desconhecido
varIndex <- (1:numSamples)[-c(testSamples, trainSamples)]
unkSamples <- sample(varIndex, 50)
# gera os dataframes de train, test e inknow
# gera dataframe trainX, somente com features
trainX <- Satellite[trainSamples, -37]
# gera vetor de fatores trainY, somente com target variable
trainY <- Satellite[trainSamples, 37]
# gera dataframe testX, somente com features
testX <- Satellite[testSamples, -37]
# gera vetor de fatores testY, somente com target variable
testY <- Satellite[testSamples, 37]
# gera dataframe unkX, somente com features e target desconhecido
unkX <- Satellite[unkSamples, -37]
# aplica modelos
knnFit  <- train(trainX, trainY, "knn")
rpartFit <- train(trainX, trainY, "rpart")
# extrai predição dos modelos
predTargets <- extractPrediction(list(knnFit, rpartFit), 
                                 testX = testX, 
                                 testY = testY, 
                                 unkX = unkX)

plotObsVsPred(predTargets)
#-------------------------------
# plota acurácia dos modelos (falta ver porque são 2 pontos!)
plotObsVsPred(predValues)

# extraindo probabilidades das classes
probValues <- extractProb(
                          models,
                          testX = testDescr,
                          testY = testClass)

testProbs <- subset(
                    probValues,
                    dataType == "Test")
str(testProbs)
### ESTA PODE SER A SAIDA DO MODELO!!!!
### ordenando as probabilidades para escolher acima de um threshold!!!
### Se criarmos um valor monetário para benefício (TP) ou custo (TN),
# podemos usar como critério de corte!!!!! FAZER ISSO (ver expected value matrix)
proValues_sort <-
    probValues %>%
        arrange(desc(s))

## Characterizing performance

svmPred <- subset(testValues, model == "svmRadial")
confusionMatrix(svmPred$pred, svmPred$obs)

svmProb <- subset(testProbs, model == "svmRadial")
#svmROC <- roc(svmProb$mutagen, svmProb$obs)

# COLOCAR AQUI TODO O TRATAMENTO DE ROC CURVE
svmROC <- roc(svmProb$obs, svmProb$s)
str(svmROC)
plot(svmROC)


## Predictor importance

gbmImp <- varImp(gbmFit, scale = FALSE)
gbmImp



## Parallel processing

library("caretNWS")
set.seed(2)
svmFit <- trainNWS(
                   trainDescr, trainClass,
                   method = "svmRadial",
                   tuneLength = 5,
                   scaled = FALSE)
#------------ ADENDO -----------------------------------------------------------------------
# obs. Colocar aqui análise de exp_uso_model.R e my_ROC.R (super completo para gerar ROC curves)!!!!!
# aplicar o modelo a um exemplo para ver a probabilidade dele fazer parte da classe
# usando regressão logística
# cria fórmula de regressão (símbolo ~) para este trget variable
#formula <- isSetosa ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width

# TERMINANDO COLOCAR AQUI ESTE ADENDO



#-----------------------------------------------------------------------------------

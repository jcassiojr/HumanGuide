# Este é o roadmap para fazer as análises dos dados do Human Guide
# supondo que as features sejam os 8 fatores
require("caret")
require("corrplot")
require("ggplot2")
require("gridExtra")
require("pROC")
require("doMC")
registerDoMC(5) # parallel processing
#############################################
## DATA PREPARATION
#############################################

# ----- carrega dados simulados de Human Guide

source("./R/f_simula_dados_HG2.R")

df_hg <- f_simula_dados_HG2()
turnover <- df_hg[,1] # transformando em vetor de fatores
descr <- df_hg[,-1] # transformando em vetor de fatores

# ----- cria datasets de treino e teste

set.seed(1)
#inTrain <- createDataPartition(mutagen, p = 3/4, list = FALSE)
inTrain <- createDataPartition(turnover, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]

trainClass <- turnover[inTrain]
testClass  <- turnover[-inTrain]

# ----- verifica balanceamento das classes nos dados de treino

prop.table(table(turnover))
prop.table(table(trainClass))
ncol(trainDescr)

# ----- elimina near zero deviation nos dados de treino

# Remove predictors with one distinct value
isZV <- apply(trainDescr, 2, function(u) length(unique(u)) == 1)
trainDescr <- trainDescr[, !isZV]
testDescr  <-  testDescr[, !isZV]

# -------- analisando e eliminando correlações entre as features

# Plot #1: Basic scatterplot matrix of the four measurements

#M <- cor(df_hg[,-1])
descrCorr <- cor(trainDescr)
# plotando com p-value para cada correlação
#corrplot.mixed(M, col = c("black", "white"), insig = "p-value",sig.level = -1)
corrplot.mixed(descrCorr, insig = "p-value",sig.level = -1)
# plotando com cluster hierárquico
corrplot(descrCorr, order = "hclust", addrect = 2)
# função para mostrar teste de significância
# ATENÇÃO: está com erro por deslocar valores no gráfico
#cor.mtest <- function(mat, conf.level = 0.95) {
#    mat <- as.matrix(mat)
#    n <- ncol(mat)
#    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
#    diag(p.mat) <- 0
 #   diag(lowCI.mat) <- diag(uppCI.mat) <- 1
 #   for (i in 1:(n - 1)) {
 #       for (j in (i + 1):n) {
 #           tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
 #           p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
 #           lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
 #           uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
 #       }
 #   }
 #   return(list(p.mat, lowCI.mat, uppCI.mat))
#}

#res1 <- cor.mtest(mtcars, 0.95)
#res2 <- cor.mtest(mtcars, 0.99)

## specialized the insignificant value according to the significant level
#corrplot(M, p.mat = res1[[1]], sig.level = 0.2)

# If using "hclust", corrplot() can draw rectangles around the chart of 
# corrrelation matrix based on the results of hierarchical clustering.

# usin anither color scheme
#col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
#                           "#007FFF", "blue", "#00007F"))
#col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
#                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
#col3 <- colorRampPalette(c("red", "white", "blue"))
#col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", 
#                           "cyan", "#007FFF", "blue", "#00007F"))
#wb <- c("white", "black")

## using these color spectrums
#corrplot(M, order = "hclust", addrect = 2, col = col1(100))
#corrplot(M, order = "hclust", addrect = 2)
## leave blank on no significant coefficient
#corrplot(M, p.mat = res1[[1]], insig = "blank")
## add p-values on no significant coefficient
#corrplot(M, p.mat = res1[[1]], insig = "p-value")
## add all p-values
#corrplot(M, p.mat = res1[[1]], insig = "p-value", sig.level = -1)

# elimina colunas de features com correlação par-wise acima de 0.01, p. ex.

highCorr <- findCorrelation(descrCorr, 0.05)

trainDescr <- trainDescr[, -highCorr]
testDescr  <-  testDescr[, -highCorr]
ncol(trainDescr)

####################
# ALTERNATIVA AUTOMATICA PARA TIRAR NZV E CORRELACOES ENTRE FEATURES(TESTAR)
###################
#-------------------------------------------------
# REMOVENDO NEAR ZERO VARIANCE AND CORRELATIONS (FOR CORRELATION, NUMERIC FEATURES ONLY)
#-------------------------------------------------

# eliminando zero-variance predictors da amostra
# There are many models where predictors with a single unique 
# value (also known as “zero- variance predictors”) will 
# cause the model to fail.
# recomendação: descartar preditor se
# 1. se percentagem de valores únicos menor que 20% E
# 2. razão entre o mais frequente e o segundo mais frequente for maior que 20
# nearZeroVar testa para estes dois casos!
# saveMetrics = TRUE retorna dataframe com todas as informações
#nearZeroVar(trainDescr, freqCut = 20, uniqueCut = 20)
#trn_nzvar <- nearZeroVar(trainDescr, freqCut = 20, uniqueCut = 20, saveMetrics = TRUE)
#tst_nzvar <- nearZeroVar(testDescr, freqCut = 20, uniqueCut = 20, saveMetrics = TRUE)
trn_nzvar <- nearZeroVar(trainDescr, freqCut = 20, uniqueCut = 20)
tst_nzvar <- nearZeroVar(testDescr, freqCut = 20, uniqueCut = 20)


#----- eliminando features com menor importância
# run the RFE algorithm
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(trainDescr, trainClass, sizes=c(1:6), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# ----- NORMALIZANDO OS DADOS: será feita na chamada da função train

# normaliza os dados. É melhor chamar esta função
# dentro da função trans()
# abaixo, somente normaliza os dados dos sets de train e test
#xTrans <- preProcess(trainDescr)
#trainDescr <- predict(xTrans, trainDescr)
#testDescr  <- predict(xTrans,  testDescr)

#---------- criando os modelos usando acurácia

## Building and tuning models (métodos para regression (ou dual) in caret)
# definindo os parâmetros de controle para uso nos modelos
control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary )

# no caso abaixo, usa método bootstrap como default
# bootControl <- trainControl(number = 200)

# SVM MODEL
#---------------

set.seed(2)
svm_model <- trainNWS(
                trainDescr, trainClass,
                method = "svmRadial",
                tuneLength = 5,
                preProcess = c("scale", "center"),
                trControl = control,
                scaled = FALSE)
svm_model
svm_model$finalModel
# estimating feature importance

svm_importance <- varImp(svm_model, scale=FALSE)
# summarize importance
print(svm_importance)
# plot importance
plot(svm_importance)

# STOCHASTIC GRADIENT BOOST MODEL (GBM)
#---------------

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
gbm_model <- train(
                trainDescr, trainClass,
                method = "gbm",
                trControl = control,
                verbose = FALSE,
                bag.fraction = 0.5,                
                tuneGrid = gbmGrid)

gbm_model
gbm_model$finalModel
# estimating feature importance
gbm_importance <- varImp(gbm_model, scale=FALSE)
# summarize importance
print(gbm_importance)
# plot importance
plot(gbm_importance)

# TREE BAG MODEL
#---------------
#train_control <- trainControl(method = "repeatedcv", number=10, repeats = 5,
#                              classProbs = TRUE,
#                              summaryFunction = twoClassSummary
#)
tbg_model <- train(trainDescr, trainClass, 
                   nbagg = 50,
                   metric = "ROC",
                   preProcess=c("center", "scale"),
                   trControl=control, 
                   method="treebag")

tbg_model
tbg_model$finalModel
# estimating feature importance
tbg_importance <- varImp(tbg_model, scale=FALSE)
# summarize importance
print(tbg_importance)
plot(tbg_importance)

# CONDITIONAL INFERENCE TREE MODEL
#-------

ctree2_model <- train(trainDescr, trainClass, 
                      metric = "ROC",
                      preProcess=c("center", "scale"),
                      trControl=control, 
                      method="ctree2")
ctree2_model
ctree2_model$finalModel
# estimating feature importance
ctree2_importance <- varImp(ctree2_model, scale=FALSE)
# summarize importance
print(ctree2_importance)
# plot importance
plot(ctree2_importance)

# K NEIGHBORS MODEL ( ESTÁ DANDO ERRO???!!!)
#--------

knn_model <- train(trainDescr, trainClass, 
                   k = 5,
                   metric = "ROC",
                   preProcess=c("center", "scale"),
                   trControl=control,  
                   method="knn")
knn_model
knn_model$finalModel
# estimating feature importance
knn_importance <- varImp(knn_model, scale=FALSE)
# summarize importance
print(knn_importance)
# plot importance
plot(knn_importance)

# BAYESIAN GENERALIZING LINEAR MODEL
#----------

bglm_model <- train(trainDescr, trainClass, 
                    metric = "ROC",
                    preProcess=c("center", "scale"),
                    trControl=control,
                    method="bayesglm")
bglm_model
bglm_model$finalModel

# estimating feature importance
bglm_importance <- varImp(bglm_model, scale=FALSE)
# summarize importance
print(bglm_importance)
# plot importance
plot(bglm_importance)

# GENERALIZING LINEAR MODEL
#----------

glm_model <- train(trainDescr, trainClass, 
                   metric = "ROC",
                   preProcess=c("center", "scale"),
                   trControl=control, 
                   method="glm")
glm_model
glm_model$finalModel
# estimating feature importance
glm_importance <- varImp(glm_model, scale=FALSE)
# summarize importance
print(glm_importance)
# plot importance
plot(glm_importance)

# BOOSTED LOGISTIC REGRESSION MODEL
#---------

logb_model <- train(trainDescr, trainClass, 
               #nbagg = 50,
               metric = "ROC",
               preProcess=c("center", "scale"),
               trControl=control,  
               method="LogitBoost")
logb_model
logb_model$finalModel
# estimating feature importance
logb_importance <- varImp(logb_model, scale=FALSE)
# summarize importance
print(logb_importance)
# plot importance
plot(logb_importance)

# colocar aqui grid com todos os plots de importância acima!!!

#par(mfrow=c(1,1))
plot(svm_importance)
plot(gbm_importance)
plot(tbg_importance)
plot(ctree2_importance)
plot(bglm_importance)
plot(glm_importance)
plot(logb_importance)
################
##################
#################
#--------- Realizando predições com os modelos com probabilidade de classes

## Prediction of new samples
# retorna o valor previsto pelo modelo para os dados de teste
# obs. pode ser usado como saída, juntamente com o id de cada candidato 
# montado no mesmo dataframe com id do candidato
#x <- predict(svmFit$finalModel, newdata = testDescr)[1:5]
#predict(gbmFit$finalModel, newdata = testDescr)[1:5]
# predizendo probabilidade de ser da classe 
#predict(svmFit$finalModel,newdata = testDescr, type = "prob")
#predict(svmFit, newdata = testDescr)[1:5]
#predict(svmFit, newdata = testDescr)

# consolidando previsões de diversos modelos eu um output
models <- list(svm = svm_model,
               gbm = gbm_model,
               tbg = tbg_model,
               ctree2 = ctree2_model,
               bglm = bglm_model,
               glm = glm_model,
               logb = logb_model)
testPred <- predict(models, newdata = testDescr, type = "prob")

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

#--------- Characterizing performance per model

svmPred <- subset(testValues, model == "svmRadial")
svm_cf <- confusionMatrix(svmPred$pred, svmPred$obs)
print (svm_cf$table) # confusion matrix as a table
print (svm_cf$byClass) # estatistics as a matrix
print (svm_cf$overall) # acuracy as a numeric vector

gbmPred <- subset(testValues, model == "gbm")
gbm_cf <- confusionMatrix(gbmPred$pred, gbmPred$obs)
print (gbm_cf$table) # confusion matrix as a table
print (gbm_cf$byClass) # estatistics as a matrix
print (gbm_cf$overall) # acuracy as a numeric vector

tbgPred <- subset(testValues, model == "treebag")
tbg_cf <- confusionMatrix(tbgPred$pred, tbgPred$obs)
print (tbg_cf$table) # confusion matrix as a table
print (tbg_cf$byClass) # estatistics as a matrix
print (tbg_cf$overall) # acuracy as a numeric vector

ctree2Pred <- subset(testValues, model == "ctree2")
ctree2_cf <- confusionMatrix(ctree2Pred$pred, ctree2Pred$obs)
print (ctree2_cf$table) # confusion matrix as a table
print (ctree2_cf$byClass) # estatistics as a matrix
print (ctree2_cf$overall) # acuracy as a numeric vector

bglmPred <- subset(testValues, model == "bayesglm")
bglm_cf <- confusionMatrix(bglmPred$pred, bglmPred$obs)
print (bglm_cf$table) # confusion matrix as a table
print (bglm_cf$byClass) # estatistics as a matrix
print (bglm_cf$overall) # acuracy as a numeric vector

glmPred <- subset(testValues, model == "glm")
glm_cf <- confusionMatrix(glmPred$pred, glmPred$obs)
print (glm_cf$table) # confusion matrix as a table
print (glm_cf$byClass) # estatistics as a matrix
print (glm_cf$overall) # acuracy as a numeric vector

logbPred <- subset(testValues, model == "LogitBoost")
logb_cf <- confusionMatrix(logbPred$pred, logbPred$obs)
print (logb_cf$table) # confusion matrix as a table
print (logb_cf$byClass) # estatistics as a matrix
print (logb_cf$overall) # acuracy as a numeric vector

#--------- analysing ROC curves

# COLOCAR AQUI TODO O TRATAMENTO DE ROC CURVE
# SVM
svmProb <- subset(testProbs, model == "svmRadial")
svmROC <- roc(svmProb$obs, svmProb$s)
str(svmROC)
plot(svmROC)
# GBM
gbmProb <- subset(testProbs, model == "gbm")
gbmROC <- roc(gbmProb$obs, gbmProb$s)
str(gbmROC)
plot(gbmROC)
# TREE BAG
tbgProb <- subset(testProbs, model == "treebag")
tbgROC <- roc(tbgProb$obs, tbgProb$s)
str(tbgROC)
plot(tbgROC)
# CTREE
ctree2Prob <- subset(testProbs, model == "ctree2")
ctree2ROC <- roc(ctree2Prob$obs, ctree2Prob$s)
str(ctree2ROC)
plot(ctree2ROC)
# BAYESIAN GLM
bglmProb <- subset(testProbs, model == "bayesglm")
bglmROC <- roc(bglmProb$obs, bglmProb$s)
str(bglmROC)
plot(bglmROC)
# GLM
glmProb <- subset(testProbs, model == "glm")
glmROC <- roc(glmProb$obs, glmProb$s)
str(glmROC)
plot(glmROC)
# LOGISTIC BOOST
logbProb <- subset(testProbs, model == "LogitBoost")
logbROC <- roc(logbProb$obs, logbProb$s)
str(logbROC)
plot(logbROC)

#------------ ADENDO -----------------------------------------------------------------------
# obs. Colocar aqui análise de exp_uso_model.R e my_ROC.R (super completo para gerar ROC curves)!!!!!
# aplicar o modelo a um exemplo para ver a probabilidade dele fazer parte da classe
# usando regressão logística
# cria fórmula de regressão (símbolo ~) para este trget variable
#formula <- isSetosa ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width

# TERMINANDO COLOCAR AQUI ESTE ADENDO



#-----------------------------------------------------------------------------------

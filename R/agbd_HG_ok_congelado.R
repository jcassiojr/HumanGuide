# Este é o roadmap para fazer as análises dos dados do Human Guide
# supondo que as features sejam os 8 fatores
require("caret")
require("corrplot")
require("ggplot2")
require("gridExtra")
require("pROC")
require("ROCR")
require("xlsx")
require("doMC")
registerDoMC(5) # parallel processing
#############################################
## DATA PREPARATION
#############################################

# ----- carrega dados simulados de Human Guide
#source("./R/f_simula_dados_HG2.R")
#df_hg <- f_simula_dados_HG2()

# ----- carrega dados reais de Human Guide
source("./R/f_le_dados_HG.R")
df_hg <- f_le_dados_HG()
# ----- calcula scores a partir dos dados originais de Human Guide
source("./R/f_calc_scores_HG.R")
df_scores_hg <- f_calc_scores_HG(df_hg)

class <- df_scores_hg[,2] # transformando em vetor de fatores de target
descr <- df_scores_hg[,-2] # transformando em vetor de fatores de features

# para teste com iris datset
#df_hg <- iris
#class <- df_hg[,5] # transformando em vetor de fatores
#descr <- df_hg[,-5] # transformando em vetor de fatores
# ----- cria datasets de treino e teste

set.seed(1)
#inTrain <- createDataPartition(mutagen, p = 3/4, list = FALSE)
inTrain <- createDataPartition(class, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]

trainClass <- class[inTrain]
testClass  <- class[-inTrain]

# ----- verifica balanceamento das classes nos dados de treino
prop.table(table(class))
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
descrCorr <- cor(trainDescr[,-1])
# plotando com p-value para cada correlação
#corrplot.mixed(M, col = c("black", "white"), insig = "p-value",sig.level = -1)
corrplot.mixed(descrCorr, insig = "p-value",sig.level = -1)
# plotando com cluster hierárquico
#corrplot(descrCorr, order = "hclust", addrect = 2)
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

# elimina colunas de features com correlação par-wise acima de 0.90, p. ex.

highCorr <- findCorrelation(descrCorr, 0.90)

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
# eliminando das features a coluna ID
results <- rfe(trainDescr[,-1], trainClass, sizes=c(1:6), rfeControl=control)
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
# default metrics to estimate best model is Kappa (great for
# unbalanced classes) e Accuracy
# se quisermos estimar sensitivity and specificity, ROC and AUC
# we need to tell train to produce class probability, estimate
# these statistics and to rank models by the ROC AUC
# twoClassSummary function calculates all of this
control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary # comentar para uso com iris
                        )

# no caso abaixo, usa método bootstrap como default
# bootControl <- trainControl(number = 200)

# SVM MODEL
#---------------

set.seed(2)
svm_model <- train(
                trainDescr[,-1], trainClass,
                method = "svmRadial",
                tuneLength = 5,
                preProcess = c("scale", "center"),
                trControl = control,
                scaled = FALSE)
svm_model
svm_model$finalModel
ggplot(svm_model) + theme(legend.position = "top")

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
                trainDescr[,-1], trainClass,
                method = "gbm",
                trControl = control,
                verbose = FALSE,
                metric = "ROC",
                preProcess=c("center", "scale"),
                bag.fraction = 0.5,                
                tuneGrid = gbmGrid)

gbm_model
gbm_model$finalModel
ggplot(gbm_model) + theme(legend.position = "top")

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
tbg_model <- train(trainDescr[,-1], trainClass, 
                   nbagg = 50,
                   metric = "ROC",
                   preProcess=c("center", "scale"),
                   trControl=control, 
                   method="treebag")

tbg_model
tbg_model$finalModel
#ggplot(tbg_model) + theme(legend.position = "top")

# estimating feature importance
tbg_importance <- varImp(tbg_model, scale=FALSE)
# summarize importance
print(tbg_importance)
plot(tbg_importance)

# CONDITIONAL INFERENCE TREE MODEL
#-------

ctree2_model <- train(trainDescr[,-1], trainClass, 
                      metric = "ROC",
                      preProcess=c("center", "scale"),
                      trControl=control, 
                      method="ctree2")
ctree2_model
ctree2_model$finalModel
ggplot(ctree2_model) + theme(legend.position = "top")

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

bglm_model <- train(trainDescr[,-1], trainClass, 
                    metric = "ROC",
                    preProcess=c("center", "scale"),
                    trControl=control,
                    method="bayesglm")
bglm_model
bglm_model$finalModel
# ggplot(bglm_model) + theme(legend.position = "top")

# estimating feature importance
bglm_importance <- varImp(bglm_model, scale=FALSE)
# summarize importance
print(bglm_importance)
# plot importance
plot(bglm_importance)

# GENERALIZING LINEAR MODEL
#----------

glm_model <- train(trainDescr[,-1], trainClass, 
                   metric = "ROC",
                   preProcess=c("center", "scale"),
                   trControl=control, 
                   method="glm")
glm_model
glm_model$finalModel
#ggplot(glm_model) + theme(legend.position = "top")

# estimating feature importance
glm_importance <- varImp(glm_model, scale=FALSE)
# summarize importance
print(glm_importance)
# plot importance
plot(glm_importance)

# BOOSTED LOGISTIC REGRESSION MODEL
#---------

logb_model <- train(trainDescr[,-1], trainClass, 
               #nbagg = 50,
               metric = "ROC",
               preProcess=c("center", "scale"),
               trControl=control,  
               method="LogitBoost")
logb_model
logb_model$finalModel
ggplot(logb_model) + theme(legend.position = "top")

# estimating feature importance
logb_importance <- varImp(logb_model, scale=FALSE)
# summarize importance
print(logb_importance)
# plot importance
plot(logb_importance)

# NAIVE BAYES MODEL
#---------

nb_model <- train(trainDescr[,-1], trainClass, 
                    #nbagg = 50,
                    metric = "ROC",
                    preProcess=c("center", "scale"),
                    trControl=control,  
                    method="nb")

nb_model
nb_model$finalModel
ggplot(nb_model) + theme(legend.position = "top")

# estimating feature importance
nb_importance <- varImp(nb_model, scale=FALSE)
# summarize importance
print(nb_importance)
# plot importance
plot(nb_importance)
# colocar aqui grid com todos os plots de importância acima!!!

#par(mfrow=c(1,1))
plot(svm_importance)
plot(gbm_importance)
plot(tbg_importance)
plot(ctree2_importance)
plot(bglm_importance)
plot(glm_importance)
plot(logb_importance)
plot(nb_importance)
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
               logb = logb_model,
               nb = nb_model)
testPred <- predict(models, newdata = testDescr[,-1], type = "prob")

#lapply(testPred,
#       function(x) x[1:5])
predValues <- extractPrediction(
                                models,
                                testX = testDescr[,-1],
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
                          testX = testDescr[,-1],
                          testY = testClass)

testProbs <- subset(
                    probValues,
                    dataType == "Test")
str(testProbs)
### ESTA PODE SER A SAIDA DO MODELO!!!!
### ordenando as probabilidades para escolher acima de um threshold!!!
### Se criarmos um valor monetário para benefício (TP) ou custo (TN),
# podemos usar como critério de corte!!!!! FAZER ISSO (ver expected value matrix)
probValues_sort <-
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

nbPred <- subset(testValues, model == "nb")
nb_cf <- confusionMatrix(nbPred$pred, nbPred$obs)
print (nb_cf$table) # confusion matrix as a table
print (nb_cf$byClass) # estatistics as a matrix
print (nb_cf$overall) # acuracy as a numeric vector

#--------- analysing ROC curves (usando library pROC)
# NÃO USAR POR ENQUANTO MAS ESTÁ FUNCIONANDO
#------------- SVM
#svmProb <- subset(testProbs, model == "svmRadial")
#svmROC <- roc(svmProb$obs, svmProb$s)
#str(svmROC)
#plot(svmROC)
#plot.roc(svmROC,  
#         print.thres = TRUE, 
#         main="Confidence interval of a threshold - SVM",
#         percent=TRUE,
#         print.auc=TRUE,
#         col="#1c61b6")

# GBM
#gbmProb <- subset(testProbs, model == "gbm")
#gbmROC <- roc(gbmProb$obs, gbmProb$s)
#str(gbmROC)
#plot.roc(gbmROC,  
#         print.thres = TRUE, 
#         main="Confidence interval of a threshold - SVM",
#         percent=TRUE,
#         print.auc=TRUE,
#         col="#1c61b6")

# TREE BAG
#tbgProb <- subset(testProbs, model == "treebag")
#tbgROC <- roc(tbgProb$obs, tbgProb$s)
#str(tbgROC)

#plot.roc(tbgROC,  
#         print.thres = TRUE, 
#         main="Confidence interval of a threshold - SVM",
#         percent=TRUE,
#         print.auc=TRUE,
#         col="#1c61b6")

# CTREE
#ctree2Prob <- subset(testProbs, model == "ctree2")
#ctree2ROC <- roc(ctree2Prob$obs, ctree2Prob$s)
#str(ctree2ROC)
#plot.roc(ctree2ROC,  
#         print.thres = TRUE, 
#         main="Confidence interval of a threshold - SVM",
#         percent=TRUE,
#         print.auc=TRUE,
#         col="#1c61b6")

# BAYESIAN GLM
#bglmProb <- subset(testProbs, model == "bayesglm")
#bglmROC <- roc(bglmProb$obs, bglmProb$s)
#str(bglmROC)
#plot.roc(bglmROC,  
##         print.thres = TRUE, 
#         main="Confidence interval of a threshold - SVM",
#         percent=TRUE,
#         print.auc=TRUE,
#         col="#1c61b6")
# GLM
#glmProb <- subset(testProbs, model == "glm")
#glmROC <- roc(glmProb$obs, glmProb$s)
#str(glmROC)
#plot.roc(glmROC,  
#         print.thres = TRUE, 
#         main="Confidence interval of a threshold - SVM",
#         percent=TRUE,
#         print.auc=TRUE,
#         col="#1c61b6")

# LOGISTIC BOOST
#logbProb <- subset(testProbs, model == "LogitBoost")
#logbROC <- roc(logbProb$obs, logbProb$s)
#str(logbROC)
#plot(logbROC)
#plot.roc(logbROC,  
#         print.thres = TRUE, 
#         main="Confidence interval of a threshold - SVM",
#         percent=TRUE,
#         print.auc=TRUE,
#         col="#1c61b6")








#---- ROC Curves dos modelos usando library ROCR
# These characteristics of ROC graphs have become increasingly important
# as research continues into the areas of cost-sensitive learning and 
# learning in the presence of unbalanced classes.
# um ponto no espaço ROC representa um modleo que retorna true/false para
# a pergunta se a instância pertence ou não á classe alvo
# um ponto no espaço ROC representa uma única confusion matrix
# (uma decision tree gera, portanto, um ponto no espaço ROC)
# o lado direito superior do espaço ROC é considerado mais liberal
# o lado esquerdo é considerado conservador
# probabilistic classifiers:
# Naive Bayes ou Neural Network classifier gera uma probabilidade ou score de cada instância
# (grau no qual a instância pertence à classe), gerando uma curva ROC para as instâncias
# consideradas
# este tipo de classifier permite criar um threshold para criar um classifier binário,
# ou seja, acima dele considera-se positivo para a classe e abaixo dele, negativo
# baixando o threshold vou movendo da área mais conservativa para a área mais liberal.
# Assim, seleciona-se o threshold com melhor custo x benefício ou acurácia, ou seja,
# acima dele o modelo retorna Y e abaix N!!!
# A classifier need not produce accurate, calibrated probability estimates; 
# it need only produce relative accurate scores that serve to discriminate positive 
# and negative instances
# ROC curves have an attractive property: they are insensitive to changes in class
# distribution.

svmProbs <- subset(testProbs, model == "svmRadial", type = "prob")
pred <- prediction(svmProbs$s, svmProbs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="gbm")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

# GBM
gbmProbs <- subset(testProbs,model == "gbm")
#prob <- predict(gbmPred, newdata=testPred, type="prob")
# ++++ ALTERNATIVA
rocCurve <-roc(response = testClass,
               predictor = gbmProbs[, "s"],
               levels = rev(levels(testClass)))
rocCurve
plot(rocCurve)
plot(rocCurve,
     print.thres = c(.5,.2),
     print.thres.pch = 16,
     print.thres.cex = 1,2,
     print.auc = TRUE
)
# plot sensitivy + specitivity x threshold (cut-off)
plot(specificity + sensitivity ~ threshold, t(coords(rocCurve, seq(0, 1, 0.01))), type = "l")
# ++++

pred <- prediction(gbmProbs$s, gbmProbs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GBM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

# TREE BAG
tbgProbs <- subset(testProbs,model == "treebag")
#prob <- predict(gbmPred, newdata=testPred, type="prob")
pred <- prediction(tbgProbs$s, tbgProbs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="TREE BAG")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

# CTREE
ctree2Probs <- subset(testProbs,model == "ctree2")
#prob <- predict(gbmPred, newdata=testPred, type="prob")
pred <- prediction(ctree2Probs$s, ctree2Probs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="CTREE")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

# BAYESIAN GLM
bglmProbs <- subset(testProbs,model == "bayesglm")
#prob <- predict(gbmPred, newdata=testPred, type="prob")
pred <- prediction(bglmProbs$s, bglmProbs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="BAYESIAN GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

# GLM
glmProbs <- subset(testProbs,model == "glm")
#prob <- predict(gbmPred, newdata=testPred, type="prob")
pred <- prediction(glmProbs$s, glmProbs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

# LOGISTIC BOOST
logbProbs <- subset(testProbs,model == "LogitBoost")
#prob <- predict(gbmPred, newdata=testPred, type="prob")
pred <- prediction(logbProbs$s, logbProbs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="LOGISTIC BOOST")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

# NAIVE BAYES
nbProbs <- subset(testProbs,model == "nb")
# aqui, sortear nbProbs decrescente de coluna "s" e usando threshold
# considerar positivo "s" acima do threshold nos dados a prever!!!
pred <- prediction(nbProbs$s, nbProbs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="NAIVE BAYES")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

# ++++ ALTERNATIVA
rocCurve <-roc(response = testClass,
               predictor = nbProbs[, "s"],
               levels = rev(levels(testClass)),
               plot = TRUE, print.auc = TRUE, 
               print.thres = c(.5,.2),print.thres.pch = 16,
               print.thres.cex = 1,2)
rocCurve
#plot(rocCurve)
#plot(rocCurve,
#     print.thres = c(.5,.2),
#     print.thres.pch = 16,
#     print.thres.cex = 1,2,
#     print.auc = TRUE
#)

# plot sensitivy + specitivity x threshold (cut-off)
plot(specificity + sensitivity ~ threshold, t(coords(rocCurve, seq(0, 1, 0.01))), type = "l")
# ++++
# desenvolver abaixo a tree induction
# tree induction 
form <- as.formula(turnover ~ .)
tree.2 <- rpart(form,df_scores_hg)			# A more reasonable tree
prp(tree.2)   
# gerando probabilidades para rankear!
# A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle



# TESTE DE COST PARA FP e TP em ROC CURVE USING performence!!
# TESTAR ESTE MODELO COM DADOS DE BREAST C E MUSHROOMS

# Este é o roadmap para fazer as análises dos dados do Human Guide
# supondo que as features sejam os 8 fatores
require("caret")
require("corrplot")
require("ggplot2")
require("pROC")
require("ROCR")
require("rpart")
require("rattle")					# Fancy tree plot
require("rpart.plot")
#require("xlsx")
#require("plyr")
require("dplyr")
require("doMC")
registerDoMC(5) # parallel processing
#############################################
## DATA PREPARATION
#############################################

# ----- carrega dados simulados de Human Guide
#source("./R/f_simula_dados_HG2.R")
#df_hg <- f_simula_dados_HG2()

# ----- carrega dados reais de Human Guide
# retorna lista com: dataset para treino do modelo, dataset para uso do modelo
source("./R/f_le_raw_HG.R")
l_df <- f_le_raw_HG()
df_hg_train <- l_df$df_hg_train # data set a ser usado para treino dos modelos
df_hg_use <- l_df$df_hg_use # dataset a ser usado para fazer previsões

# ----- calcula scores a partir dos dados originais de Human Guide

# calcula score a partir dos dados brutos
source("./R/f_tidy_scores_HG.R")
df_scores_hg_train <- f_tidy_scores_HG(df_hg_train)
df_scores_hg_use <- f_tidy_scores_HG(df_hg_use)

class <- as.factor(df_scores_hg_train[,2]) # transformando em vetor de fatores de target
descr <- df_scores_hg_train[,-c(1,2)] # transformando em dataframe de features

# ----- cria datasets de treino, teste e uso em previsão
set.seed(1)
inTrain <- createDataPartition(class, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]
useDescr  <- df_scores_hg_use[,-2] # transformando em dataframe de uso sem coluna target

trainClass <- class[inTrain]
testClass  <- class[-inTrain]

# ----- verifica balanceamento das classes nos dados de treino
prop.table(table(class))
prop.table(table(trainClass))
ncol(trainDescr)

# ----- elimina near zero deviation nos dados de treino

# Remove predictors with one distinct value
#isZV <- apply(trainDescr, 2, function(u) length(unique(u)) == 1)
#trainDescr <- trainDescr[, !isZV]
#testDescr  <-  testDescr[, !isZV]

# -------- analisando correlações entre as features
# Plot #1: Basic scatterplot matrix of the four measurements
descrCorr <- cor(trainDescr)
# plotando com p-value para cada correlação
# interessante para ver correlação entre os componentes!!
corrplot.mixed(descrCorr, insig = "p-value",sig.level = -1)

# -------- elimina colunas de features com correlação parwise acima de 0.90, p. ex.
#highCorr <- findCorrelation(descrCorr, 0.90)
#trainDescr <- trainDescr[, -highCorr]
#testDescr  <-  testDescr[, -highCorr]
#ncol(trainDescr)

# ALTERNATIVA AUTOMATICA PARA TIRAR NZV E CORRELACOES ENTRE FEATURES(TESTAR)

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

# removendo colunas que não passaram no teste
if(length(trn_nzvar) != 0  || length(tst_nzvar) != 0) {
    trainDescr <- trainDescr[,-(trn_nzvar)]
    testDescr <- testDescr[,-(tst_nzvar)]
}

#----- eliminando features com menor importância
# run the RFE algorithm
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# eliminando das features a coluna ID
results <- rfe(trainDescr, trainClass, sizes=c(1:3), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#########################
# SELECIONANDO FEATURES
#########################
# aqui mudar dados de treino e teste para somente as features selecionadas
# variavesi selecionadas: imagination(col 5), stability (col 6)
# trainDescr <- trainDescr[,c(5,6)]
# testDescr  <- testDescr[,c(5,6)]

# outra importante técnica de separar features importantes
require(MASS)
trainTotal <- cbind(sexo = trainClass,trainDescr)
initial <- glm(sexo ~ ., data = cbind(sexo = trainClass,trainDescr), family = "binomial")
stepAIC(initial, direction = "both")
# analisando s saída escolhemos as fetatures que dão menor IAC
# neste caso, escolheu stabgility (col 6) e contacts (col 7)
# trainDescr <- trainDescr[,c(6,7)]
# testDescr  <- testDescr[,c(6,7)]

#######################################
## BUILDING AND TUNING MODELS
#######################################

source("./R/f_train_models.R")
models <- f_train_models(trainClass, trainDescr)
# level plot of the accuracies (modelo gbm)
plot(models$gbm, plotType = "level")
# density plot (modelo gbm)
resampleHist(models$gbm)
# FALTA: aplicar para os demais modelos!!!
#------------------------------------

# PLOTANDO VALORES PREVISTOS E REALIZADOS
# objeto predValues é dataframe é lista de todos os modelos com os valores 
# das classes binárias
predValues <- extractPrediction(
                                models,
                                testX = testDescr,
                                testY = testClass)
# obtém somente o subset de dados de teste das previsões
testValues <- subset(
                     predValues,
                     dataType == "Test")
head(testValues)
table(testValues$model)
nrow(testDescr)
# plota previsto x observado
plotObsVsPred(predValues)


# ---------------- A tree induction to observe best features
form <- as.formula(sexo ~ .)
tree.2 <- rpart(form,df_scores_hg)			# A more reasonable tree
prp(tree.2)   
# A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle

#########################################
# ANALISANDO OS MODELOS COM ROC CURVES
#########################################

#--------- Realizando predições com os modelos com probabilidade de classes
source("./R/f_rank_fpRate_models.R")
source("./R/f_rank_bestBal_models.R")
source("./R/f_rank_dfltCost_models.R")
source("./R/f_rank_custmCost_models.R")
source("./R/f_rank_bestAcc_models.R")

# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TREINO

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
#----------------------------------------------------------------
# retorna lista com duas listas: de confusion matrix e de valores de cuttof 
# para cada modelo
l_fpr <- suppressWarnings(f_rank_fpRate_models(models, testClass, testDescr, 0.1))

# Plot roc. objects (para cada modelo)
# DEPOIS COLOCAR EM GRID!!!
#-----------------
plot(l_fpr$rocPerf$svmRadial )
abline(a=0, b= 1)
plot(l_fpr$rocPerf$gbm )
abline(a=0, b= 1)
plot(l_fpr$rocPerf$treebag)
abline(a=0, b= 1)
plot(l_fpr$rocPerf$ctree2)
abline(a=0, b= 1)
plot(l_fpr$rocPerf$bayesglm)
abline(a=0, b= 1)
plot(l_fpr$rocPerf$glm)
abline(a=0, b= 1)
plot(l_fpr$rocPerf$LogitBoost)
abline(a=0, b= 1)
plot(l_fpr$rocPerf$nb)
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print(l_fpr$cf$svmRadial$table)
print(l_fpr$cf$gbm$table)
print(l_fpr$cf$treebag$table)
print(l_fpr$cf$ctree2$table)
print(l_fpr$cf$bayesglm$table)
print(l_fpr$cf$glm$table)
print(l_fpr$cf$LogitBoost$table)
print(l_fpr$cf$nb$table)

# estatistics as a matrix
print(l_fpr$cf$svmRadial$byClass)
print(l_fpr$cf$gbm$byClass)
print(l_fpr$cf$treebag$byClass)
print(l_fpr$cf$ctree2$byClass)
print(l_fpr$cf$bayesglm$byClass)
print(l_fpr$cf$glm$byClass)
print(l_fpr$cf$LogitBoost$byClass)
print(l_fpr$cf$nb$byClass)

# acuracy as a numeric vector
print(l_fpr$cf$svmRadial$overall)
print(l_fpr$cf$gbm$overall)
print(l_fpr$cf$treebag$overall)
print(l_fpr$cf$ctree2$overall)
print(l_fpr$cf$bayesglm$overall)
print(l_fpr$cf$glm$overall)
print(l_fpr$cf$LogitBoost$overall)
print(l_fpr$cf$nb$overall)


# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
#--------------------------------------------------------------------------------------
#l_bestBal <- suppressWarnings(f_rank_best_bal(models, "svmRadial"))
# retorna lista com duas listas: de confusion matrix e de valores de cuttof 
# para cada modelo
l_bestBal <- suppressWarnings(f_rank_bestBal_models(models, testClass, testDescr))

# Plot roc. objects (para cada modelo)
#-----------------
plot(l_bestBal$rocPerf$svmRadial )
abline(a=0, b= 1)
plot(l_bestBal$rocPerf$gbm )
abline(a=0, b= 1)
plot(l_bestBal$rocPerf$treebag)
abline(a=0, b= 1)
plot(l_bestBal$rocPerf$ctree2)
abline(a=0, b= 1)
plot(l_bestBal$rocPerf$bayesglm)
abline(a=0, b= 1)
plot(l_bestBal$rocPerf$glm)
abline(a=0, b= 1)
plot(l_bestBal$rocPerf$LogitBoost)
abline(a=0, b= 1)
plot(l_bestBal$rocPerf$nb)
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print(l_bestBal$cf$svmRadial$table)
print(l_bestBal$cf$gbm$table)
print(l_bestBal$cf$treebag$table)
print(l_bestBal$cf$ctree2$table)
print(l_bestBal$cf$bayesglm$table)
print(l_bestBal$cf$glm$table)
print(l_bestBal$cf$LogitBoost$table)
print(l_bestBal$cf$nb$table)

# estatistics as a matrix
print(l_bestBal$cf$svmRadial$byClass)
print(l_bestBal$cf$gbm$byClass)
print(l_bestBal$cf$treebag$byClass)
print(l_bestBal$cf$ctree2$byClass)
print(l_bestBal$cf$bayesglm$byClass)
print(l_bestBal$cf$glm$byClass)
print(l_bestBal$cf$LogitBoost$byClass)
print(l_bestBal$cf$nb$byClass)

# acuracy as a numeric vector
print(l_bestBal$cf$svmRadial$overall)
print(l_bestBal$cf$gbm$overall)
print(l_bestBal$cf$treebag$overall)
print(l_bestBal$cf$ctree2$overall)
print(l_bestBal$cf$bayesglm$overall)
print(l_bestBal$cf$glm$overall)
print(l_bestBal$cf$LogitBoost$overall)
print(l_bestBal$cf$nb$overall)

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
#----------------------------------------------------------
l_costDflt <- suppressWarnings(f_rank_dfltCost_models(models, testClass, testDescr))

# Plot roc. objects (para cada modelo)
#-----------------
plot(l_costDflt$rocPerf$svmRadial )
abline(a=0, b= 1)
plot(l_costDflt$rocPerf$gbm )
abline(a=0, b= 1)
plot(l_costDflt$rocPerf$treebag)
abline(a=0, b= 1)
plot(l_costDflt$rocPerf$ctree2)
abline(a=0, b= 1)
plot(l_costDflt$rocPerf$bayesglm)
abline(a=0, b= 1)
plot(l_costDflt$rocPerf$glm)
abline(a=0, b= 1)
plot(l_costDflt$rocPerf$LogitBoost)
abline(a=0, b= 1)
plot(l_costDflt$rocPerf$nb)
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print(l_costDflt$cf$svmRadial$table)
print(l_costDflt$cf$gbm$table)
print(l_costDflt$cf$treebag$table)
print(l_costDflt$cf$ctree2$table)
print(l_costDflt$cf$bayesglm$table)
print(l_costDflt$cf$glm$table)
print(l_costDflt$cf$LogitBoost$table)
print(l_costDflt$cf$nb$table)

# estatistics as a matrix
print(l_costDflt$cf$svmRadial$byClass)
print(l_costDflt$cf$gbm$byClass)
print(l_costDflt$cf$treebag$byClass)
print(l_costDflt$cf$ctree2$byClass)
print(l_costDflt$cf$bayesglm$byClass)
print(l_costDflt$cf$glm$byClass)
print(l_costDflt$cf$LogitBoost$byClass)
print(l_costDflt$cf$nb$byClass)

# acuracy as a numeric vector
print(l_costDflt$cf$svmRadial$overall)
print(l_costDflt$cf$gbm$overall)
print(l_costDflt$cf$treebag$overall)
print(l_costDflt$cf$ctree2$overall)
print(l_costDflt$cf$bayesglm$overall)
print(l_costDflt$cf$glm$overall)
print(l_costDflt$cf$LogitBoost$overall)
print(l_costDflt$cf$nb$overall)

# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
#----------------------------------------------------------
l_costCustm <- suppressWarnings(f_rank_custmCost_models(models, testClass, testDescr,1, 10))

# Plot roc. objects (para cada modelo)
#-----------------
plot(l_costCustm$rocPerf$svmRadial )
abline(a=0, b= 1)
plot(l_costCustm$rocPerf$gbm )
abline(a=0, b= 1)
plot(l_costCustm$rocPerf$treebag)
abline(a=0, b= 1)
plot(l_costCustm$rocPerf$ctree2)
abline(a=0, b= 1)
plot(l_costCustm$rocPerf$bayesglm)
abline(a=0, b= 1)
plot(l_costCustm$rocPerf$glm)
abline(a=0, b= 1)
plot(l_costCustm$rocPerf$LogitBoost)
abline(a=0, b= 1)
plot(l_costCustm$rocPerf$nb)
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print(l_costCustm$cf$svmRadial$table)
print(l_costCustm$cf$gbm$table)
print(l_costCustm$cf$treebag$table)
print(l_costCustm$cf$ctree2$table)
print(l_costCustm$cf$bayesglm$table)
print(l_costCustm$cf$glm$table)
print(l_costCustm$cf$LogitBoost$table)
print(l_costCustm$cf$nb$table)

# estatistics as a matrix
print(l_costCustm$cf$svmRadial$byClass)
print(l_costCustm$cf$gbm$byClass)
print(l_costCustm$cf$treebag$byClass)
print(l_costCustm$cf$ctree2$byClass)
print(l_costCustm$cf$bayesglm$byClass)
print(l_costCustm$cf$glm$byClass)
print(l_costCustm$cf$LogitBoost$byClass)
print(l_costCustm$cf$nb$byClass)

# acuracy as a numeric vector
print(l_costCustm$cf$svmRadial$overall)
print(l_costCustm$cf$gbm$overall)
print(l_costCustm$cf$treebag$overall)
print(l_costCustm$cf$ctree2$overall)
print(l_costCustm$cf$bayesglm$overall)
print(l_costCustm$cf$glm$overall)
print(l_costCustm$cf$LogitBoost$overall)
print(l_costCustm$cf$nb$overall)

# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição d apopulação não é confiável)
#----------------------------------------------------------
l_bestAcc <- suppressWarnings(f_rank_bestAcc_models(models, testClass, testDescr))
# Plot roc. objects (para cada modelo)
#-----------------
plot(l_bestAcc$rocPerf$svmRadial)
abline(a=0, b= 1)
plot(l_bestAcc$rocPerf$gbm )
abline(a=0, b= 1)
plot(l_bestAcc$rocPerf$treebag)
abline(a=0, b= 1)
plot(l_bestAcc$rocPerf$ctree2)
abline(a=0, b= 1)
plot(l_bestAcc$rocPerf$bayesglm)
abline(a=0, b= 1)
plot(l_bestAcc$rocPerf$glm)
abline(a=0, b= 1)
plot(l_bestAcc$rocPerf$LogitBoost)
abline(a=0, b= 1)
plot(l_bestAcc$rocPerf$nb)
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print(l_bestAcc$cf$svmRadial$table)
print(l_bestAcc$cf$gbm$table)
print(l_bestAcc$cf$treebag$table)
print(l_bestAcc$cf$ctree2$table)
print(l_bestAcc$cf$bayesglm$table)
print(l_bestAcc$cf$glm$table)
print(l_bestAcc$cf$LogitBoost$table)
print(l_bestAcc$cf$nb$table)

# estatistics as a matrix
print(l_bestAcc$cf$svmRadial$byClass)
print(l_bestAcc$cf$gbm$byClass)
print(l_bestAcc$cf$treebag$byClass)
print(l_bestAcc$cf$ctree2$byClass)
print(l_bestAcc$cf$bayesglm$byClass)
print(l_bestAcc$cf$glm$byClass)
print(l_bestAcc$cf$LogitBoost$byClass)
print(l_bestAcc$cf$nb$byClass)

# acuracy as a numeric vector
print(l_bestAcc$cf$svmRadial$overall)
print(l_bestAcc$cf$gbm$overall)
print(l_bestAcc$cf$treebag$overall)
print(l_bestAcc$cf$ctree2$overall)
print(l_bestAcc$cf$bayesglm$overall)
print(l_bestAcc$cf$glm$overall)
print(l_bestAcc$cf$LogitBoost$overall)
print(l_bestAcc$cf$nb$overall)

##########################################################
# OBTENDO PREVISÕES DE PROBABILIDADE DE CLASSES RANKEADAS
##########################################################

# De acordo com a abordagem selecionada no tuning dos modelos
# seleciona o dataframe rankeado de probabilidade de classificador
# para a lista de modelos candidatos
source("./R/f_prev_rank_class.R")

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
l_rank_fpr <- suppressWarnings(f_prev_rank_class(models, useDescr, l_fpr$cutoff))

# ABORDAGEM 2: melhor balanço entre TPR = max and FPR = min
l_rank_bestBal <- suppressWarnings(f_prev_rank_class(models, useDescr, l_bestBal$cutoff))

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
l_rank_dfltCost <- suppressWarnings(f_prev_rank_class(models, useDescr, l_costDflt$cutoff))

# ABORDAGEM 4: usando custo segundo relação cost.fp/cost.fn)
l_rank_custmCost <- suppressWarnings(f_prev_rank_class(models, useDescr, l_costCustm$cutoff))

# ABORDAGEM 5: usando melhor accurácia vs cuttof (CUIDADO:se existe skew na 
# distribuição da população não é confiável)
l_rank_bestAcc <- suppressWarnings(f_prev_rank_class(models, useDescr, l_bestAcc$cutoff))


# TESTE DE COST PARA FP e TP em ROC CURVE USING performence!!
# TESTAR ESTE MODELO COM DADOS DE BREAST C E MUSHROOMS

# OU RECEBE UM CANDIDATO E RETORNA SUA PROBABILIDADE DE TURNOVER
# testar com dados dos 813 usados na tese
# criar funcao tb p retornar apenas a probabilidade de pertencer à classe de um candidato


# ESCOLHIDO O MODELO AQUI, CRIAR FUNCAO QUE USA O MODELO ESCOLHIDO RECEBENDO
# OS DADOS A PREVER, GERANDO O MODELO E RETORNANDO O DATAFRAME DE 
# PROBABILIDADE DE CLASSES
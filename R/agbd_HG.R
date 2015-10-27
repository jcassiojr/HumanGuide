# Este é o roadmap para fazer as análises dos dados do Human Guide
# supondo que as features sejam os 8 fatores
require("caret")
require("corrplot")
require("ggplot2")
require("pROC")
require("rpart")
require("xlsx")
require("plyr")
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
source("./R/f_le_dados_HG.R")
df_hg <- f_le_dados_HG()

# ----- calcula scores a partir dos dados originais de Human Guide
# considerando target variable sexo como sendo turnover somente para teste
source("./R/f_calc_scores_HG.R")
df_scores_hg <- f_calc_scores_HG(df_hg)
df_scores_hg <-
    df_scores_hg %>%
    select (sexo, power, quality,
            exposure, structure, imagination, stability, contacts) %>%
    mutate(sexo = ifelse(sexo == 1, "m", "f"))
class <- as.factor(df_scores_hg[,1]) # transformando em vetor de fatores de target
descr <- df_scores_hg[,-1] # transformando em vetor de fatores de features

# ----- cria datasets de treino e teste
set.seed(1)
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

# removendo colunas que não passaram no teste
if(length(trn_nzvar) != 0  || length(tst_nzvar) != 0) {
    trainDescr <- trainDescr[,-(trn_nzvar)]
    testDescr <- testDescr[,-(tst_nzvar)]
}

#----- eliminando features com menor importância
# run the RFE algorithm
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# eliminando das features a coluna ID
results <- rfe(trainDescr, trainClass, sizes=c(1:6), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

## Building and tuning models 
source("./R/f_apply_models.R")
models <- f_apply_models(trainClass, trainDescr)

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

# ANALISANDO OS MODELOS COM ROC CURVES
#--------- Realizando predições com os modelos com probabilidade de classes
source("./R/f_rank_fpRate.R")
source("./R/f_rank_best_bal.R")
source("./R/f_rank_cost_dflt.R")
source("./R/f_rank_cost_custm.R")
source("./R/f_rank_best_acc.R")

# MODELO SVM RADIAL
#----------------------------------------------------------------------

# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TESTE

######################################################################################
# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
######################################################################################
l_fpr <- f_rank_fpRate(models, "svmRadial", 0.1)
# Plot roc. object (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
plot(l_fpr[[2]])
abline(a=0, b= 1)
# lift curve (é o mesmo para todas as funções, portanto somente plota uma vez)
#-------------
#roc.perf = performance(pred, measure = "lift", x.measure = "rpp")
#plot(roc.perf)
#abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print (l_fpr[[1]]$table)
print (l_fpr[[1]]$byClass) # estatistics as a matrix
print (l_fpr[[1]]$overall) # acuracy as a numeric vector

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_fpr[[3]])

######################################################################################
# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
######################################################################################
l_bestBal <- f_rank_best_bal(models, "svmRadial")

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_bestBal[[3]])

######################################################################################
# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
######################################################################################
l_costDflt <- f_rank_cost_dflt(models, "svmRadial")

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costDflt[[3]])

######################################################################################
# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
######################################################################################
l_costCustm <- f_rank_cost_custm(models, "svmRadial")

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costCustm[[3]])

######################################################################################
# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição d apopulação não é confiável)
#####################################################################################

l_bestAcc <- suppressWarnings(f_rank_best_acc(models, "svmRadial"))

# Dataframe de probabilidades final rankeado
#----------------------------------------------------
print(l_bestAcc[[3]])

# plota acurácia x cutoff
#----------------------------------------------------
plot(l_bestAcc[[4]])

######################################################################################
# MODELO GBM
######################################################################################

# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TESTE

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
#-------------------------------------------------------------------------------------
l_fpr <- suppressWarnings(f_rank_fpRate(models, "gbm", 0.1))
# Plot roc. object (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
plot(l_fpr[[2]])
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print (l_fpr[[1]]$table)
print (l_fpr[[1]]$byClass) # estatistics as a matrix
print (l_fpr[[1]]$overall) # acuracy as a numeric vector

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_fpr[[3]])

# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
#-------------------------------------------------------------------------------------
l_bestBal <- suppressWarnings(f_rank_best_bal(models, "gbm"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_bestBal[[3]])

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
#-------------------------------------------------------------------------------------
l_costDflt <- suppressWarnings(f_rank_cost_dflt(models, "gbm"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costDflt[[3]])

# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
#-------------------------------------------------------------------------------------
l_costCustm <- suppressWarnings(f_rank_cost_custm(models, "gbm", 1, 10))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costCustm[[3]])


# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição da população não é confiável)
#----------------------------------------------------------------------------------------

l_bestAcc <- suppressWarnings(f_rank_best_acc(models, "gbm"))

# Dataframe de probabilidades final rankeado
#----------------------------------------------------
print(l_bestAcc[[3]])

# plota acurácia x cutoff
#----------------------------------------------------
plot(l_bestAcc[[4]])

######################################################################################
# MODELO TREE BAG
######################################################################################
# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TESTE

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
#----------------------------------------------------------------
l_fpr <- suppressWarnings(f_rank_fpRate(models, "treebag", 0.1))
# Plot roc. object (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
plot(l_fpr[[2]])
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print (l_fpr[[1]]$table)
print (l_fpr[[1]]$byClass) # estatistics as a matrix
print (l_fpr[[1]]$overall) # acuracy as a numeric vector

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_fpr[[3]])

# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
#--------------------------------------------------------------------------------------
l_bestBal <- suppressWarnings(f_rank_best_bal(models, "treebag"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_bestBal[[3]])

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
#--------------------------------------------------------------------------------------
l_costDflt <- suppressWarnings(f_rank_cost_dflt(models, "treebag"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costDflt[[3]])

# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
#--------------------------------------------------------------------------------------
l_costCustm <- suppressWarnings(f_rank_cost_custm(models, "treebag", 1, 10))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costCustm[[3]])

# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição d apopulação não é confiável)
#--------------------------------------------------------------------------------------

l_bestAcc <- suppressWarnings(f_rank_best_acc(models, "treebag"))

# Dataframe de probabilidades final rankeado
#----------------------------------------------------
print(l_bestAcc[[3]])

# plota acurácia x cutoff
#----------------------------------------------------
plot(l_bestAcc[[4]])


######################################################################################
# MODELO CTREE 2
######################################################################################
# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TESTE

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
#----------------------------------------------------------------
l_fpr <- suppressWarnings(f_rank_fpRate(models, "ctree2", 0.1))
# Plot roc. object (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
plot(l_fpr[[2]])
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print (l_fpr[[1]]$table)
print (l_fpr[[1]]$byClass) # estatistics as a matrix
print (l_fpr[[1]]$overall) # acuracy as a numeric vector

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_fpr[[3]])

# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
#--------------------------------------------------------------------------------------
l_bestBal <- suppressWarnings(f_rank_best_bal(models, "ctree2"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_bestBal[[3]])

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
#--------------------------------------------------------------------------------------
l_costDflt <- suppressWarnings(f_rank_cost_dflt(models, "ctree2"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costDflt[[3]])

# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
#--------------------------------------------------------------------------------------
l_costCustm <- suppressWarnings(f_rank_cost_custm(models, "ctree2", 1, 10))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costCustm[[3]])

# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição d apopulação não é confiável)
#--------------------------------------------------------------------------------------

l_bestAcc <- suppressWarnings(f_rank_best_acc(models, "ctree2"))

# Dataframe de probabilidades final rankeado
#----------------------------------------------------
print(l_bestAcc[[3]])

# plota acurácia x cutoff
#----------------------------------------------------
plot(l_bestAcc[[4]])


######################################################################################
# MODELO BAYES GLM
######################################################################################
# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TESTE

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
#----------------------------------------------------------------
l_fpr <- suppressWarnings(f_rank_fpRate(models, "bayesglm", 0.1))
# Plot roc. object (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
plot(l_fpr[[2]])
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print (l_fpr[[1]]$table)
print (l_fpr[[1]]$byClass) # estatistics as a matrix
print (l_fpr[[1]]$overall) # acuracy as a numeric vector

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_fpr[[3]])

# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
#--------------------------------------------------------------------------------------
l_bestBal <- suppressWarnings(f_rank_best_bal(models, "bayesglm"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_bestBal[[3]])

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
#--------------------------------------------------------------------------------------
l_costDflt <- suppressWarnings(f_rank_cost_dflt(models, "bayesglm"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costDflt[[3]])

# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
#--------------------------------------------------------------------------------------
l_costCustm <- suppressWarnings(f_rank_cost_custm(models, "bayesglm", 1, 10))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costCustm[[3]])

# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição d apopulação não é confiável)
#--------------------------------------------------------------------------------------

l_bestAcc <- suppressWarnings(f_rank_best_acc(models, "bayesglm"))

# Dataframe de probabilidades final rankeado
#----------------------------------------------------
print(l_bestAcc[[3]])

# plota acurácia x cutoff
#----------------------------------------------------
plot(l_bestAcc[[4]])

######################################################################################
# MODELO GLM
######################################################################################
# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TESTE

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
#----------------------------------------------------------------
l_fpr <- suppressWarnings(f_rank_fpRate(models, "glm", 0.1))
# Plot roc. object (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
plot(l_fpr[[2]])
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print (l_fpr[[1]]$table)
print (l_fpr[[1]]$byClass) # estatistics as a matrix
print (l_fpr[[1]]$overall) # acuracy as a numeric vector

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_fpr[[3]])

# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
#--------------------------------------------------------------------------------------
l_bestBal <- suppressWarnings(f_rank_best_bal(models, "glm"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_bestBal[[3]])

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
#--------------------------------------------------------------------------------------
l_costDflt <- suppressWarnings(f_rank_cost_dflt(models, "glm"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costDflt[[3]])

# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
#--------------------------------------------------------------------------------------
l_costCustm <- suppressWarnings(f_rank_cost_custm(models, "glm", 1, 10))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costCustm[[3]])

# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição d apopulação não é confiável)
#--------------------------------------------------------------------------------------

l_bestAcc <- suppressWarnings(f_rank_best_acc(models, "glm"))

# Dataframe de probabilidades final rankeado
#----------------------------------------------------
print(l_bestAcc[[3]])

# plota acurácia x cutoff
#----------------------------------------------------
plot(l_bestAcc[[4]])

######################################################################################
# MODELO LOGISTIC BOOST
######################################################################################
# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TESTE

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
#----------------------------------------------------------------
l_fpr <- suppressWarnings(f_rank_fpRate(models, "LogitBoost", 0.1))
# Plot roc. object (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
plot(l_fpr[[2]])
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print (l_fpr[[1]]$table)
print (l_fpr[[1]]$byClass) # estatistics as a matrix
print (l_fpr[[1]]$overall) # acuracy as a numeric vector

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_fpr[[3]])

# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
#--------------------------------------------------------------------------------------
l_bestBal <- suppressWarnings(f_rank_best_bal(models, "LogitBoost"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_bestBal[[3]])

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
#--------------------------------------------------------------------------------------
l_costDflt <- suppressWarnings(f_rank_cost_dflt(models, "LogitBoost"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costDflt[[3]])

# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
#--------------------------------------------------------------------------------------
l_costCustm <- suppressWarnings(f_rank_cost_custm(models, "LogitBoost", 1, 10))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costCustm[[3]])

# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição d apopulação não é confiável)
#--------------------------------------------------------------------------------------

l_bestAcc <- suppressWarnings(f_rank_best_acc(models, "LogitBoost"))

# Dataframe de probabilidades final rankeado
#----------------------------------------------------
print(l_bestAcc[[3]])

# plota acurácia x cutoff
#----------------------------------------------------
plot(l_bestAcc[[4]])

######################################################################################
# MODELO NAIVE BAYES
######################################################################################
# SELECIONANDO DIVERSAS SAÍDAS RANKEADAS DOS DADOS DE TESTE

# ABORDAGEM 1: para aceitar um falso positivo até um certo nível
# (ex.aceitar maior ou igual a 10% de falsos positivos)
#----------------------------------------------------------------
l_fpr <- suppressWarnings(f_rank_fpRate(models, "nb", 0.1))
# Plot roc. object (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
plot(l_fpr[[2]])
abline(a=0, b= 1)

# Confusion Matrix (é o mesmo para todas as funções, portanto somente plota uma vez)
#-----------------
print (l_fpr[[1]]$table)
print (l_fpr[[1]]$byClass) # estatistics as a matrix
print (l_fpr[[1]]$overall) # acuracy as a numeric vector

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_fpr[[3]])

# ABORDAGEM 2: getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
#--------------------------------------------------------------------------------------
l_bestBal <- suppressWarnings(f_rank_best_bal(models, "nb"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_bestBal[[3]])

# ABORDAGEM 3: usando custo que dá um resultado de cutoff 
# que minimiza custo (default: cost.fp = 1 e cost.fn = 1)
#--------------------------------------------------------------------------------------
l_costDflt <- suppressWarnings(f_rank_cost_dflt(models, "nb"))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costDflt[[3]])

# ABORDAGEM 4: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO cutoff que minimiza custo 
# (definindo relação cost.fp/cost.fn)
#--------------------------------------------------------------------------------------
l_costCustm <- suppressWarnings(f_rank_cost_custm(models, "nb", 1, 10))

# Dataframe de probabilidades final rankeado por FP Rate > que %cutoff
#----------------------------------------------------
print(l_costCustm[[3]])

# ABORDAGEM 5: DAQUI POSSO OBTER O INDICE DE CUTTOF EM RELAÇÃO ACURÁCIA MÁXIMA 
# accuracy vs cuttof (CUIDADO:  se existe skew na distribuição d apopulação não é confiável)
#--------------------------------------------------------------------------------------
l_bestAcc <- suppressWarnings(f_rank_best_acc(models, "nb"))

# Dataframe de probabilidades final rankeado
#----------------------------------------------------
print(l_bestAcc[[3]])

# plota acurácia x cutoff
#----------------------------------------------------
plot(l_bestAcc[[4]])

# ++++
# desenvolver abaixo a tree induction
# tree induction 
form <- as.formula(sexo ~ .)
tree.2 <- rpart(form,df_scores_hg)			# A more reasonable tree
prp(tree.2)   
# gerando probabilidades para rankear!
# A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle


# TESTE DE COST PARA FP e TP em ROC CURVE USING performence!!
# TESTAR ESTE MODELO COM DADOS DE BREAST C E MUSHROOMS

# AO FINAL CRIAR FUNÇÃO QUE RETORNA SOMENTE OS DADOS ATÉ O THRESHOLD ESCOLHIDO
# (PARA MALA DIRETA)
# OU RECEBE UM CANDIDATO E RETORNA SUA PROBABILIDADE DE TURNOVER

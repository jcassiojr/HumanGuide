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
require("dplyr")
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
isZV <- apply(trainDescr, 2, function(u) length(unique(u)) == 1)
trainDescr <- trainDescr[, !isZV]
testDescr  <-  testDescr[, !isZV]

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

# SVM MODEL
#---------------
set.seed(2)
svm_model <- train(
                trainDescr, trainClass,
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
# usando abaixo tunning mais sofisticado do grid
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
ggplot(ctree2_model) + theme(legend.position = "top")

# estimating feature importance
ctree2_importance <- varImp(ctree2_model, scale=FALSE)
# summarize importance
print(ctree2_importance)
# plot importance
plot(ctree2_importance)

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
ggplot(logb_model) + theme(legend.position = "top")

# estimating feature importance
logb_importance <- varImp(logb_model, scale=FALSE)
# summarize importance
print(logb_importance)
# plot importance
plot(logb_importance)

# NAIVE BAYES MODEL
#---------
nb_model <- train(trainDescr, trainClass, 
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

#--------- Realizando predições com os modelos com probabilidade de classes

# consolidando previsões de diversos modelos eu um output
models <- list(svm = svm_model,
               gbm = gbm_model,
               tbg = tbg_model,
               ctree2 = ctree2_model,
               bglm = bglm_model,
               glm = glm_model,
               logb = logb_model,
               nb = nb_model)
# 
testPred <- predict(models, newdata = testDescr, type = "prob")
predValues <- extractPrediction(
                                models,
                                testX = testDescr,
                                testY = testClass)
# obtém somente o subset de dados de teste
testValues <- subset(
                     predValues,
                     dataType == "Test")
head(testValues)
table(testValues$model)
nrow(testDescr)

# plota previsto x observado
plotObsVsPred(predValues)

# ----- extraindo probabilidades das classes
probValues <- extractProb(
                          models,
                          testX = testDescr,
                          testY = testClass)
# obtém somente o subset de dados de teste
testProbs <- subset(
                    probValues,
                    dataType == "Test")
str(testProbs)

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

# -------- avaliando modelos com ROC curve e definindo cuttof baseado em custo
# CONSIDERACOES GERAIS
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


# -----  somente para modelo Naive Bayes
nbProbs <- subset(testProbs,model == "nb")
head(cbind(nbProbs$m, nbProbs$obs), 5)
# making a prediction object
pred <- prediction(nbProbs$m, nbProbs$obs)
class(pred)
slotNames(pred)
sn = slotNames(pred)
sapply(sn, function(x) length(slot(pred, x)))
sapply(sn, function(x) class(slot(pred, x)))
# performance objects
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)
# para aceitar um falso positivo até um certo nível
# exemplo: aceitar menor ou igual a 10%
pROC = function(pred, fpr.stop){
    perf <- performance(pred,"tpr","fpr")
    for (iperf in seq_along(perf@x.values)){
        ind = which(perf@x.values[[iperf]] <= fpr.stop)
        perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
        perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
    }
    return(perf)
}
# o gráfico abaixo mostra até onde vai o limite que queremos
# neste caso: FPR 10% e TPR 50%
proc.perf = pROC(pred, fpr.stop=0.1)
plot(proc.perf)
abline(a=0, b= 1)
# getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))
        c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
          cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))
# ou usando custo que dá um resultado de cutoff acima
cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# different costs for TP and FN
# Target = turnover (S/N)
# TP: deixo de contratar e daria turnover
# FP: deixo de contratar e NÃO daria turnover
# TN: contrata e NÃO dá turnover
# FN: contrata e dá turnover
# ver planilha de Projeto Human Guide-V1.90.xlsx para os custos
# dá uma taxa de FN/TP cd 1/10 aproximadamente (pode ser refinada)

cost.perf = performance(pred, "cost", cost.fp = 1, cost.fn = 10)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# accuracy vs cuttof (cuidado se existe skew não é confiável)
acc.perf = performance(pred, measure = "acc")
plot(acc.perf)
# pegamos o máximo de acurácia do objeto performance
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

# ------ o mesmo acima para multiplos modelos
svmProbs <- subset(testProbs,model == "svmRadial")
gbmProbs <- subset(testProbs,model == "gbm")
tbgProbs <- subset(testProbs,model == "treebag")
ctree2Probs <- subset(testProbs,model == "ctree2")
bglmProbs <- subset(testProbs,model == "bayesglm")
glmProbs <- subset(testProbs,model == "glm")
logbProbs <- subset(testProbs,model == "LogitBoost")
nbProbs <- subset(testProbs,model == "nb")

# multiple set of predictions (aplicar depois para outros modelos!!!!)
# para lista de modelos (TESTAR E SUBSTITUIR NO SCRIPT PRINCIPAL)
#data(ROCR.hiv)
#manypred = prediction(ROCR.hiv$hiv.nn$predictions, ROCR.hiv$hiv.nn$labels)
#sapply(sn, function(x) length(slot(manypred, x)))
#sapply(sn, function(x) class(slot(manypred, x)))

# multiset predictions
#many.roc.perf = performance(manypred, measure = "tpr", x.measure = "fpr")
#plot(many.roc.perf, col=1:10)
#abline(a=0, b= 1)

# vale para multiplos predictions
#print(opt.cut(many.roc.perf, manypred))


# vale para multiplos predictions
#print(opt.cut(many.roc.perf, manypred))



# para multiplos modelos
#many.acc.perf = performance(manypred, measure = &quot;acc&quot;)
#sapply(manypred@labels, function(x) mean(x == 1))
#mapply(function(x, y){
#    ind = which.max( y )
#    acc = y[ind]
#    cutoff = x[ind]
#    return(c(accuracy= acc, cutoff = cutoff))
#}, slot(many.acc.perf, &quot;x.values&quot;), slot(many.acc.perf, &quot;y.values&quot;))


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

# AO FINAL CRIAR FUNÇÃO QUE RETORNA SOMENTE OS DADOS ATÉ O THRESHOLD ESCOLHIDO
# (PARA MALA DIRETA)
# OU RECEBE UM CANDIDATO E RETORNA SUA PROBABILIDADE DE TURNOVER

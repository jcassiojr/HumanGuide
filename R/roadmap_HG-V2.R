#---------------------------------------
# Roadmap de Análise Human Guide
#---------------------------------------

# 1. ESTABELECER CLARAMENTE O PROBLEMA DE NEGÓCIO A SOLUCIONAR
#----------------------------------------------------------
# PROBLEMA:
# ABORDAGEM:

# 2. PREPARACAO DOS DADOS
#----------------------------------------------------------
# Um modelo candidato para os dados raw é:
# 9 colunas para as features pessoais
#    nome, idade, sexo, escolaridade, formação, empresa, endereço, telefone e e-mail
# 72 colunas para cada afirmação HG, com valores:
#    p (positivo), n (negativo) ou i (indiferente)
# 1 coluna com valor latente, com valores: 
#    sensibilidade, força, qualidade, exposição, estrutura, imaginação, estabilidade, 
#    contatos
#-------------------------------------------------------------------------------
# 1. obter os dados de candidatos com o feature vector de suas pontuações 
#    no teste HG. 
# 2. associar ao dataset a target variable (turnover: sim/não) da base de funcionários
#    da empresa
# 3. separar em train data e houdout data usando a mesma distribuição das prior classes
#    da população (obter a distribuição real de turnover na empresa)

# ANÁLISE EXPLORATÓRIA
#---------------------------------------
# usar os dados sem preparaçõa adicional para criar os clusters
# criando dados para simular seguindo as proporções
# da amostra da tese

# dados pessoais de 815 amostras
# carrega dados simulados de Human Guide
df_hg <- f_simula_dados_HG()

# IDENTIFICAR ATRIBUTOS COM MAIOR INFORMATION GAIN PARA FEATURE SELECTION
#-------------------------------------------------
# CÁLCULO MANUAL
#--------------------
# chamar aqui minha função de cálculo de information gain categórica
Ep <- f_entropy(df_hg[,1])
# cálculo de information gain para feature
#----------------------------------------
v_nfeat <- c("f_11s", "f_12e", "f_13h", "f_14k", "f_15p", "f_16hy", "f_17d", 
             "f_18m", "f_21h", "f_22e", "f_23k", "f_24d", "f_25m", "f_26p",
             "f_27s", "f_28hy", "f_31h", "f_32e", "f_33hy", "f_34k", "f_35s", 
             "f_36p", "f_37d", "f_38m", "f_41hy", "f_42s", "f_43e", "f_44k", 
             "f_45h", "f_46m", "f_47d", "f_48p", "f_51e", "f_52s", "f_53hy", 
             "f_54k", "f_55d", "f_56h", "f_57p", "f_58m", "f_61m", "f_62s",
             "f_63e", "f_64hy", "f_65k", "f_66p", "f_67d", "f_68h", "f_71m",
             "f_72k", "f_73s", "f_74p", "f_75hy", "f_76h", "f_77e", "f_78d", 
             "f_81p", "f_82h", "f_83e", "f_84s", "f_85m", "f_86k", "f_87hy", 
             "f_88d", "f_91e", "f_92m", "f_93p", "f_94d", "f_95k", "f_96s", 
             "f_97h", "f_98hy")
IG <- vector()
for (i in 1:length(v_nfeat) ) {
    IG[i] <- f_ig_cat(df_hg, v_nfeat[i], "turnover")
}

# plota IG
df_ig <- data.frame(v_nfeat,IG)
# data.frame filtered: IG > 0.003 (for better viasualization)
df_ig_fltr <- 
    df_ig %>%
    filter(IG > 0.003)
# using ggplot
library(ggplot2)
ggplot(data=df_ig, 
       aes(x = reorder(df_ig[,1], IG), y=IG)) +
    geom_bar(stat="identity",color = "black",
             fill="#DD8888", 
             width=.8) +
    ggtitle("Information Gain for 72 features")
# using ggplot again for filtered data
library(ggplot2)
ggplot(data=df_ig_fltr, 
       aes(x = reorder(df_ig_fltr[,1], IG), y=IG)) +
    geom_bar(stat="identity",color = "black",
             fill="#DD8888", 
             width=.8) +
    ggtitle("Information Gain for 72 features")

# ou esta alternativa para selecionar as features via Information Gain
#data(iris)
#weights <- information.gain(Species~., iris)

# CÁLCULO USANDO pacote FSelector
#--------------------
library(FSelector)
weights <- information.gain(turnover~., df_hg)
print(weights)
# cria fórmula com os N features de maior information gain
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "turnover")
print(f)
# cria fórmula com os N features information gain acima de 75 %
subset <- cutoff.k.percent(weights, 0.75)
f <- as.simple.formula(subset, "turnover")
print(f)
# esta é a melhor opção (para pegar os atributos que mais se diferenciam dos demais)
subset <- cutoff.biggest.diff(weights)
f <- as.simple.formula(subset, "turnover")
print(f)

# USANDO CARET
#-------------------------

# AQUI CRIAR DADOS DE TEST E TRAIN 
#----------------------------------
# OBS: PRIMEIRO DEVO SEPARAR EM DOIS DATASETS???: 1 só com a coluna TARGET e outro com demais features
# VER SE É A MELHOR ABORDAGEM (é o que fqz em exp_predic_model_with_caret.R)
# ou deixa no meso datasets TARGET + FEATURES???!!!
require(dplyr)
# dataframe somente com coluna target variable
#df_hg_tgv <-
#    df_hg %>%
#    select(turnover)
# dataframe com colunas de features
#df_hg_feat <-
#    df_hg %>%
#    select(-(turnover))

require(caret)


set.seed(1)
# separa mantendo a mesma estratificação (prior class) dos originais!!
# colocando p % dos dados no training dataset
# PRIMEIRA OPÇÃO: criando datasets separados pra target e features
turnover <- df_hg[,1] # transformando em vetor de fatores
inTrain <- createDataPartition(turnover, p = 3/4, list = FALSE)
trainDescr <- df_hg[inTrain,]
testDescr  <- df_hg[-inTrain,]
trainClass <- turnover[inTrain]
testClass  <- turnover[-inTrain]

# SEGUNDA OPÇÃO: criando datasets com target e features
trainHG <- df_hg[inTrain,]
testHG  <- df_hg[-inTrain,]
# Prepare Data - listwise deletion of missing (should I standardize variables?)
#trainHG <- na.omit(trainHG) # listwise deletion of missing
#testHG <- na.omit(testHG) # listwise deletion of missing
# obtem a proporção de cada classe no outcome (target variable)
prop.table(table(turnover))
prop.table(table(trainClass))
# In cases where the outcome is numeric, the samples are split 
# into quartiles and the sampling is done within each quartile.

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


# --- ABAIXO SOMENTE PARA VARIÁVEIS NUMÉRICAS

# Also, some models are susceptible to multicollinearity (i.e., 
# high correlations between pre- dictors). Linear models, neural networks
# and other models can have poor performance in these situations or may
# generate unstable solutions. Other models, such as classification or
# regression trees, might be resistant to highly correlated predictors, 
# but multicollinearity may negatively a↵ect interpretability of the model.
# If there is a need to minimize the e↵ect of multicollinearity, there a
# re a few options. First, models that are resistant to large between-predictor
# correlations, such as partial least squares, can be used. Also, principal
# component analysis can be used to reduce the number of dimensions in a
# way that removes correlations (see below). Alternatively, we can 
# identify and remove predictors that contribute the most to the correlations.
# In linear models, the traditional method for reducing multicollinearity
# is to identify the of- fending predictors using the variable inflation 
# factor (VIF). For each variable, this statistic measures the increase 
# in the variation of the model parameter estimate in comparison to the 
# optimal situation (i.e., an orthogonal design).
# As an alternative, we can compute the correlation matrix of the predictors 
# and use an al- gorithm to remove the a subset of the problematic predictors
# such that all of the pairwise correlations are below a threshold

# For illustration, predictors that result in absolute pairwise correlations 
# greater than 0.90 can be removed using the findCorrelation function. 
# This function returns an index of column numbers for removal.
ncol(trainDescr)
# obs. exemplo abaixo não achou nenhuma correlação, apesar de no texto achar!
descrCorr <- cor(trainDescr)
highCorr <- findCorrelation(descrCorr, 0.90)
trainDescr <- trainDescr[, -highCorr]
testDescr  <-  testDescr[, -highCorr]
ncol(trainDescr)




# MODELING WITH THE TRAINING DATA AND CROSS VALIDATION
# WITH DIFERENT MODELS
#-------------------------------------------------
require(caret)
# load the iris dataset
#data(iris)


#----------------------------------------------------------
# PREPARACAO DOS DADOS
#----------------------------------------------------------


# criando bases simuladas para training and testing (com 2 classes)
#set.seed(2969)
#imbal_train <- twoClassSim(10000, intercept = -20, linearVars = 20)
#imbal_test  <- twoClassSim(10000, intercept = -20, linearVars = 20)
#table(imbal_train$Class)

# define training control (cross validation, 10 folds)
#train_control <- trainControl(method="cv", number=10)
#train_control <- trainControl(method = "repeatedcv", number=10, repeats = 5,
#                     classProbs = TRUE,
#                     summaryFunction = twoClassSummary
#                     )
# usar names(getModelInfo()) para ver todas as possibilidades
# ver doc completa em http://topepo.github.io/caret/bytag.html


#-------------------------------------------------
# REMOVENDO ATRIBUTOS PARA MELHORAR PERFORMANCE DO MODELO
#-------------------------------------------------


# Data can contain attributes that are highly correlated with each other. 
# Many methods perform better if highly correlated attributes are removed
# Generally, you want to remove attributes with an absolute correlation 
# of 0.75 or higher.

# load the library
#library(mlbench)
# calculate correlation matrix
#correlationMatrix <- cor(imbal_train[,1:25])
#correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
#print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
#print(highlyCorrelated)

# ranking features by importance
# The example below loads the Pima Indians Diabetes dataset and constructs an 
# Learning Vector Quantization (LVQ) model. The varImp is then used to estimate
# the variable importance, which is printed and plotted. 
#

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
#model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
lvq_model <- train(turnover~., data=trainHG, method="lvq", preProcess=c("scale","center"), trControl=control)
# estimate variable importance
importance <- varImp(lvq_model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# A popular automatic method for feature selection provided by the caret
# R package is called Recursive Feature Elimination or RFE.
# The example below provides an example of the RFE method on the Pima 
# Indians Diabetes dataset. A Random Forest algorithm is used on each 
# iteration to evaluate the model. The algorithm is configured to explore
# all possible subsets of the attributes. All 8 attributes are selected 
# in this example, although in the plot showing the accuracy of the 
# different attribute subset sizes, we can see that just 4 attributes 
# gives almost comparable results.
#data(PimaIndiansDiabetes)
# define the control using a random forest selection function
#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
#results <- rfe(imbal_train[,1:25], imbal_train[,26], sizes=c(1:25), rfeControl=control)
results <- rfe(trainDescr, trainClass, sizes=c(1:25), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))



# SOMENTE COM AS 72 FEATURES NÃO FICOU BOM O PLOT. TENTAR COMENTE COM CADASTRAIS
# PREPROCESSANDO PARA NORMALIZAR DADOS (Z-SCALE)
#-----------------------------------------------------
# MELHOR ABORDAGEM É USAR PARAMETRO DE PREDICT PARA ISSO
# ver webinar caret
# ex.
# svmTune <- train (churn ~ .,
#data = churnTrain,
#method = "svmRadial",
# this pre processing will be applied to
# these data and new samples too
#preProc = c("center", "scale"),
# tune of diferent values of cost
#tuneLength = 10,
#metric = "ROC",
#trControl = ctrl)

#preObj <- preProcess(df, method=c("center", "scale"))

# TRATANDO OVERFITING
#----------------------------------------------------------
# COMO TRATAR?
# EM KNN, usar o número de vizinhos
# EM TREES, limitar o número de nodes
# em geral, usando um test set verificamos se ocorre overfitting
# resampling o training set nos permite saber se a previsão é boa
# sem udo do test set (insere variações no training set para simlar futuras
# amostras)
# portanto, usando train de caret já estamos lidando com isso




# CRIANDO DIVERSOS TIPOS DE MODELOS
#----------------------------------------------------------
# CLASSIFICATION MODELS

# TREE BAG MODEL
#---------------
train_control <- trainControl(method = "repeatedcv", number=10, repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary
                     )
tbg_model <- train(turnover~., data=trainHG, 
               nbagg = 50,
               metric = "ROC",
               preProcess=c("center", "scale"),
               trControl=train_control, method="treebag")
                
# make predictions
#predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
predictions <- predict(tbg_model, trainHG[, -1])
#print(predictions)
# summarize results
#tb_cf <- confusionMatrix(predictions, imbal_train$Class)
tb_cf <- confusionMatrix(predictions, trainHG$turnover)
print (tb_cf$table) # confusion matrix as a table
print (tb_cf$byClass) # estatístics as a matrix
print (tb_cf$overall) # acuracy as a numeric vector




# CONDITIONAL INFERENCE TREE MODEL
ctree2_model <- train(turnover~., data=trainHG, 
                    #nbagg = 50,
                    metric = "ROC",
                    trControl=train_control, method="ctree2")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
#ctree2_predictions <- predict(ctree2_model, imbal_train[, -ncol(imbal_train)])
ctree2_predictions <- predict(ctree2_model, trainHG[, -1])
# summarize results
# ctree2_cf <- confusionMatrix(ctree2_predictions, imbal_train$Class)
ctree2_cf <- confusionMatrix(ctree2_predictions, trainHG$turnover)
print (ctree2_cf$table) # confusion matrix as a table
print (ctree2_cf$byClass) # estatístics as a matrix
print (ctree2_cf$overall) # acuracy as a numeric vector


# ESTE MODELO PRECISA DE FEATURES NUMÉRICAS (NÃO FUNCIONA COM DADSO CATEGÓRICOS)

# K NEIGHBORS MODEL
knn_model <- train(turnover~., data=trainHG, 
               k = 5,
               metric = "ROC",
               trControl=train_control, method="knn")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
knn_predictions <- predict(knn_model, imbal_train[, -ncol(imbal_train)])
# summarize results
knn_cf <- confusionMatrix(knn_predictions, imbal_train$Class)
print (knn_cf$table) # confusion matrix as a table
print (knn_cf$byClass) # estatístics as a matrix
print (knn_cf$overall) # acuracy as a numeric vector

# BAYESIAN GENERALIZING LINEAR MODEL
bglm_model <- train(turnover~., data=trainHG, 
                   #nbagg = 50,
                   metric = "ROC",
                   trControl=train_control, method="bayesglm")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
#bglm_predictions <- predict(bglm_model, imbal_train[, -ncol(imbal_train)])
bglm_predictions <- predict(bglm_model, trainHG[, -1])
# summarize results
#bglm_cf <- confusionMatrix(bglm_predictions, imbal_train$Class)
bglm_cf <- confusionMatrix(bglm_predictions, trainHG$turnover)
print (bglm_cf$table) # confusion matrix as a table
print (bglm_cf$byClass) # estatístics as a matrix
print (bglm_cf$overall) # acuracy as a numeric vector

# GENERALIZING LINEAR MODEL
glm_model <- train(turnover~., data=trainHG, 
                    #nbagg = 50,
                    metric = "ROC",
                    trControl=train_control, method="glm")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
#glm_predictions <- predict(glm_model, imbal_train[, -ncol(imbal_train)])
glm_predictions <- predict(glm_model, trainHG[, -1])
# summarize results
#glm_cf <- confusionMatrix(glm_predictions, imbal_train$Class)
glm_cf <- confusionMatrix(glm_predictions, trainHG$turnover)
print (glm_cf$table) # confusion matrix as a table
print (glm_cf$byClass) # estatístics as a matrix
print (glm_cf$overall) # acuracy as a numeric vector

# BOOSTED LOGISTIC REGRESSION MODEL
model <- train(turnover~., data=trainHG, 
                   #nbagg = 50,
                   metric = "ROC",
                   trControl=train_control, method="LogitBoost")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
# predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
predictions <- predict(model, trainHG[, -1])
# summarize results
#logit_cf <- confusionMatrix(predictions, imbal_train$Class)
logit_cf <- confusionMatrix(predictions, trainHG$turnover)
print (logit_cf$table) # confusion matrix as a table
print (logit_cf$byClass) # estatístics as a matrix
print (logit_cf$overall) # acuracy as a numeric vector

# SVM WITH FORWARD SELECTION MODEL
# ESTÁ DANDO ERRO ABAIXO SE USA metric = ROC. ???
#train_control <- trainControl(method = "repeatedcv", number=10, repeats = 5,
#                             classProbs = TRUE,
#                              summaryFunction = twoClassSummary
#)
train_control <- trainControl(method = "repeatedcv", number=10, repeats = 5)
model <- train(turnover~., data=trainHG, 
               #nbagg = 50,
               # metric = "ROC",
               trControl=train_control, method="svmRadialWeights")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
# predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
predictions <- predict(model, trainHG[, -1])
# summarize results
#svm_cf <- confusionMatrix(predictions, imbal_train$Class)
svm_cf <- confusionMatrix(predictions, trainHG$turnover)
print (svm_cf$table) # confusion matrix as a table
print (svm_cf$byClass) # estatístics as a matrix
print (svm_cf$overall) # acuracy as a numeric vector



# aqui prepara os dados pra trabalhar com REGRESSAO!! (TARGET NUMERICO)
# CLASSIFICATION MODELS

# LINEAR REGRESSION WITH FORWARD SELECTION MODEL
# testar com dados numericos somente!!!!
model <- train(Class~., data=imbal_train, 
               #nbagg = 50,
               metric = "ROC",
               trControl=train_control, method="lmStepAIC")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
# summarize results
logit_cf <- confusionMatrix(predictions, imbal_train$Class)
print (logit_cf$table) # confusion matrix as a table
print (logit_cf$byClass) # estatístics as a matrix
print (logit_cf$overall) # acuracy as a numeric vector

# AGRUPANDO OS MODELOS PARA ANALISAR a ROC Curve e definir o melhor
# obs. depois acrescentar o exemplo do livro, onde tem um budget
# fixo e precisa pegar número de dados que se encaixa no mesmo
# inserir também a lift curve aqui
#-------------------------------------------------

#  <COLOCAR AQUI SCRIPT DE exp_subsampling_technique.R>

#------------------------------------------------

# COMPARANDO OS MODELOS via ROC
# e SELECIONANDO O MELHOR Cut POINT do modelo escolhido
# Usando Expected value e pacote OptimalCutpoint para
# calcular melhor cutt point in ROC curve
# a partir da matriz de custo x benefício calcula o
# valor do parâmetro costs.ratio;
# CR = (Cfp - Ctn) / (Cfn - Ctp), valores obtidos
# da matriz de custoxbenefício para o problema
# de negócio.
# Calcular para todos os modelos e selecionar aquele que tem
# o maior valor, usando o cutpoint com maior custo x benefício
# Selecionado o modelo e o cutpoint, usa esgte modelo para
# predição




# USANDO O MODELO PARA PREVISÃO
#-------------------------------------------
#########################
# PAREI AQUI
#########################

# usando exemplo do knn
# K NEIGHBORS MODEL
set.seed(1)
#knnTune <- train (churn ~ .,
#                  data = churnTrain,
#                  method = "knn",
#                  #metric = "ROC",
#                  trControl = trainControl(method = "cv"))
knnTune <- train (turnover ~ .,
                  data = trainHG,
                  method = "knn",
                  #metric = "ROC",
                  trControl = trainControl(method = "cv"))
# obtendo a previsão somente para os primeiros exemplos da base de teste
# notar que não pego a coluna 20, onde está o target, ams não faz diferença
# na saída da função chamada!
# knnProbs <- predict(knnTune,churnTest[1:10,-20], type = "prob")
knnProbs <- predict(knnTune,testHG[,-1], type = "prob")
print (knnProbs)

# idem, abaixo tem a previsão pelo modelo das 10 primeiras ocorrências do arquivo
#knnPred <- predict(knnTune,newdata = churnTest[1:10,-20]) 
knnPred <- predict(knnTune,newdata = testHG[,-1]) 
print(knnPred) # imprime previsões do modelo para os 10 primeiros dados da amostra
#print(churnTest$churn[1:10]) # imprime os 10 primeiros targets reais da amostra
print(testHG$turnover) # imprime os 10 primeiros targets reais da amostra
#table(knnPred,churnTest$churn[1:10]) # tabela de confusão dos 10 primeiros dados
table(knnPred,testHG$turnover) # tabela de confusão dos 10 primeiros dados
#confusionMatrix(knnPred,churnTest$churn[1:10])$table # idem por caret
confusionMatrix(knnPred,testHG$turnover)$table # idem por caret

# O Exemplo abaixo de uso de extractPredictions funciona, mas gerou erros
# quando tentei com os dados de churn??!!
#knnFit <- train(Species ~ ., data = iris, method = "knn", 
#                trControl = trainControl(method = "cv"))

#rdaFit <- train(Species ~ ., data = iris, method = "rda", 
#                trControl = trainControl(method = "cv"))

allModels <- list(glm = glm_model, ctree = ctree2_model)
# ERRO: TALVEZ SÓ FUNCIONE COM FESTURES NUMÉRICAS
extractPrediction(allModels, unkX = testHG[1:10, -1])

# usando abaixo exemplos de exp_predic_model_with_caret.R
#--------------------------------------------------------
# Para o exemplo abaixo, trocar letras das 72 features por números e tirar as demais

bootControl <- trainControl(number = 10) # troquei o orignal 200. Demorou mais de hora e não terminou!
set.seed(2)
svmFit <- train(df_hg_feat, df_hg_tgv,
                method = "svmRadial", tuneLength = 5,
                trControl = bootControl, scaled = FALSE)

svmFit
# melhor modelo escolhido
svmFit$finalModel

# outro exemplo
gbmGrid <- expand.grid(.interaction.depth = seq(1,5, by = 2),
                       .n.trees = seq(100,1000, by = 50),
                       .shrinkage = c(0.01, 0.1),
                       .n.minobsinnode = 1)
bootControl <- trainControl(method = "repeatedcv", repeats = 5,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE)
#set.seed(2)
gbmFit <- train(df_hg[,-1], df_hg[,9:80],
                method = "gbm", trControl = bootControl, verbose = FALSE,
                bag.fraction = 0.5, tuneGrid = gbmGrid)
# In many cases, more control over the grid of tuning parameters 
# is needed. For example, for boosted trees using the gbm function 
# in the gbm package (Ridgeway 2007), we can tune over the number 
# of trees (i.e., boosting iterations), the complexity of the tree
# (indexed by interaction.depth) and the learning rate (also known
# as shrinkage). As an example, a user could specify a grid of 
# values to tune over using a data frame where the rows correspond 
# to tuning parameter combinations and the columns are the names 
# of the tuning variables (preceded by a dot). For our data, 
# we will generate a grid of 50 combinations and use the tuneGrid 
# argument to the train function to use these values

#gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,.n.trees = (1:10)*25, .shrinkage = .1)
gbmGrid <- expand.grid(.interaction.depth = seq(1,5, by = 2),
                       .n.trees = seq(100,1000, by = 50),
                       .shrinkage = c(0.01, 0.1),
                       .n.minobsinnode = 1)
bootControl <- trainControl(method = "repeatedcv", repeats = 5,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE)
#set.seed(2)
gbmFit <- train(trainDescr, trainClass,
                method = "gbm", trControl = bootControl, verbose = FALSE,
                bag.fraction = 0.5, tuneGrid = gbmGrid)
# A plot of the classification accuracy versus the tuning factors
# using plot(gbmFit)
# a plot of the Kappa statistic profiles
#plot(gbmFit, metric = "Kappa")
# A level plot of the accuracy values
plot(gbmFit, plotType = "level")
# Density plots of the 200 bootstrap estimates of accuracy and 
# Kappa for the final model
resampleHist(gbmFit)
#----------------------
# PARENTESIS: CREATING ROC Curves (from caret webinar on youtube)
#----------------------
library(pROC)
gbmPred <- predict(gbmFit, testDescr, testClass, type = "raw") # SE NÃO FUNCIONAR, ACHO QUE AQUI VOU PRECISAR CBIND testDescr + testClass
str(gbmPred)

gbmProbs <- predict(gbmFit, testDescr, testClass, type = "prob") # idem
str(gbmProbs)

confusionMatrix(gbmPred,testClass)
# caret webinar by Max kuhn 
# https://www.youtube.com/watch?v=7Jbb2ItbTC4
library (caret)
library (C50)
library(gbm)
library(pROC)
library(doMC)
registerDoMC(5)
data(churn)

# defining vector of predictors
predictors <- names(churnTrain)[names(churnTrain) != "churn"]
#set.seed(1)
#inTrainingSet <- createDataPartition(allData$churn,
#                                     p = .75, list = FALSE)
# preprocessing por coluna (escala, etc.)
#numerics <- c("account_length", "total_day_calls", "total_night_calls")
# determina médias e sd
#procValues <- preProcess(churnTrain[,numerics], method = c("center", "scale", "YeoJohnson"))
# usa os métodos para fazer os ajustes
#trainScaled <- predict(procValues, churnTrain[,numerics])
#testScaled <- predict(procValues, churnTest[,numerics])

# modeling bossted trees
# the final prediction is a weighted average of each tree's 
# prediction. The weights are based on quality of each tree

forGBM <- churnTrain
forGBM$churn <- ifelse(forGBM$churn == "yes", 1, 0)
gbmFit <- gbm(formula = churn ~ ., # use all predictors
              distribution = "bernoulli", # for classification (2 classes)
              data = forGBM,
              n.trees = 2000,   # 2000 boosting iterations
              interaction.depth = 7, # how many splits in each tree
              shrinkage = 0.01,   # learning rate
              verbose = FALSE)

# agora usando train para rodar o modelo acima automaticamente
# em diversas samples dos dados (ex. cross validation), com diferentes parâmetros do 
# modelo gbm (ex. n.trees). train permite selecionar o
# modelo com os parâmetros que tiveram a melhor performance
gbmTune <- train (x = churnTrain[,predictors],
                  y = churnTrain$churn,
                  method = "gbm")
# or using the formula interface
gbmTune <- train (churn ~ .,
                  data = churnTrain,
                  method = "gbm",
                  verbose = FALSE)
# mudando o resampling default de train (bootstrap)
# para cross validation
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
gbmTune <- train (churn ~ .,
                  data = churnTrain,
                  method = "gbm",
                  verbose = FALSE,
                  trControl = ctrl)
# default metrics to estimate best model is Kappa (great for
# unbalanced classes) e Accuracy

# se quisermos estimar sensitivity and specificity, ROC and AUC
# we need to tell train to produce class probability, estimate
# these statistics and to rank models by the ROC AUC
# twoClassSummary function calculates all of this
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
gbmTune <- train (churn ~ .,
                  data = churnTrain,
                  method = "gbm",
                  metric ="ROC",
                  verbose = FALSE,
                  trControl = ctrl)
 
# we may want to expand the scope of the search grid parameters
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
grid <- expand.grid (.interaction.depth = seq(1,7, by =2),
                     .n.trees = seq(100, 1000, by = 50),
                     .shrinkage = c(0.01, 0.1),
                     .n.minobsinnode = 10)
set.seed(1)
gbmTune <- train (churn ~ .,
                  data = churnTrain,
                  method = "gbm",
                  metric = "ROC",
                  tuneGrid = grid,
                  verbose = FALSE,
                  trControl = ctrl)
# plotando o resultado 
ggplot(gbmTune) + theme(legend.position = "top")
# previsão usando a base de teste com o melhor modelo
# obtido por train
gbmPred <- predict(gbmTune,churnTest)
str(gbmPred)
# gerando probabilidades previstas para o modelo,
# para cada exemplo, tanto de ser positivo 
# quanto de ser negativo
gbmProbs <- predict(gbmTune,churnTest, type = "prob") # idem
str(gbmProbs)
# criando confusion matrix
confusionMatrix(gbmPred,churnTest$churn)
# pobtendo e plotando ROC curve
rocCurve <-roc(response = churnTest$churn,
               predictor = gbmProbs[, "yes"],
               levels = rev(levels(churnTest$churn)))
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

# svm
set.seed(1)
# tell it to fit a SVM model and tuno over Cost
# and the RBF parameter
svmTune <- train (churn ~ .,
            data = churnTrain,
            method = "svmRadial",
            # this pre processing will be applied to
            # these data and new samples too
            preProc = c("center", "scale"),
            # tune of diferent values of cost
            tuneLength = 10,
            metric = "ROC",
            trControl = ctrl)

svmPred <- predict(svmTune,churnTest)
str(svmPred)
svmProbs <- predict(svmTune,churnTest, type = "prob") # idem
str(svmProbs)
confusionMatrix(svmPred,churnTest$churn)
# pobtendo e plotando ROC curve
rocCurve <-roc(response = churnTest$churn,
               predictor = svmProbs[, "yes"],
               levels = rev(levels(churnTest$churn)))
rocCurve
plot(rocCurve)
plot(rocCurve,
     print.thres = c(.5,.2),
     print.thres.pch = 16,
     print.thres.cex = 1,2,
     print.auc = TRUE
)


set.seed(1)
# now try a flexible discriminant model
# using MARS basis functions
fdaTune <- train (churn ~ .,
                  data = churnTrain,
                  method = "fda",
                  # tune of diferent values of cost
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = ctrl)

fdaPred <- predict(fdaTune,churnTest)
str(fdaPred)
fdaProbs <- predict(fdaTune,churnTest, type = "prob") # idem
str(fdaProbs)
confusionMatrix(fdaPred,churnTest$churn)
# pobtendo e plotando ROC curve
rocCurve <-roc(response = churnTest$churn,
               predictor = fdaProbs[, "yes"],
               levels = rev(levels(churnTest$churn)))
rocCurve
plot(rocCurve)
plot(rocCurve,
     print.thres = c(.5,.2),
     print.thres.pch = 16,
     print.thres.cex = 1,2,
     print.auc = TRUE
)

# daqui para a frente estou complementanto o webinar
#knn
# K NEIGHBORS MODEL
set.seed(1)
knnTune <- train (churn ~ .,
                  data = churnTrain,
                  method = "knn",
                  metric = "ROC",
                  trControl = ctrl)
knnPred <- predict(knnTune,churnTest)
#--------
allModels <- list(knn = knnTune)
extractPrediction(allModels, 
                  unkX = churnTest[1:10, -20])
#--------
str(knnPred)
knnProbs <- predict(knnTune,churnTest, type = "prob") # idem
str(knnProbs)
cf <- confusionMatrix(knnPred,churnTest$churn)
print (cf$table) # confusion matrix as a table
print (cf$byClass) # estatístics as a matrix
print (cf$overall) # acuracy as a numeric vector

# pobtendo e plotando ROC curve
rocCurve <-roc(response = churnTest$churn,
               predictor = knnProbs[, "yes"],
               levels = rev(levels(churnTest$churn)))
rocCurve
plot(rocCurve)
plot(rocCurve,
     print.thres = c(.5,.2),
     print.thres.pch = 16,
     print.thres.cex = 1,2,
     print.auc = TRUE
)

# using ramdon forest and 10-fold cross validation
set.seed(1)
rdfTune <- train (churn ~ .,
                  data = churnTrain,
                  method = "rf",
                  metric = "ROC",
                  tuneGrid = data.frame(mtry = 3),
                  ntree = 1000,
                  trControl = ctrl)

rdfPred <- predict(rdfTune,churnTest)
str(rdfPred)
rdfProbs <- predict(rdfTune,churnTest, type = "prob") # idem
str(rdfProbs)
cf <- confusionMatrix(rdfPred,churnTest$churn)
print (cf$table) # confusion matrix as a table
print (cf$byClass) # estatístics as a matrix
print (cf$overall) # acuracy as a numeric vector
# obtendo a performance do melhor modelo
getTrainPerf(rdfTune)


# obtendo e plotando ROC curve
rocCurve <-roc(response = churnTest$churn,
               predictor = rdfProbs[, "yes"],
               levels = rev(levels(churnTest$churn)))
rocCurve
plot(rocCurve)
plot(rocCurve,
     print.thres = c(.5,.2),
     print.thres.pch = 16,
     print.thres.cex = 1,2,
     print.auc = TRUE
)



# complementando o webinar comparando os modelos
# 1. colocando IG
# 2. colocando feature correlation and near zero deviation
trn_nzvar <- nearZeroVar(trainDescr, freqCut = 20, uniqueCut = 20)
tst_nzvar <- nearZeroVar(testDescr, freqCut = 20, uniqueCut = 20)
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
# 3. comparando modelos
# 4. Usando Expected value e pacote OptimalCutpointd para
# calcular melhor cutt point in ROC curve
# a partir da matriz de custo x benefício calcula o
# valor do parâmetro costs.ratio;
# CR = (Cfp - Ctn) / (Cfn - Ctp), valores obtidos
# da matriz de custoxbenefício para o problema
# de negócio


# usando testando lift charts para Evaluate Class Probability
# from Other Functions text form Caret homepage
# para identificar probabilility thresholds que podem capturar uma 
# certa percentagem de hits
# ou seja, para obter 60% de hits, preciso usar 30% da sample

## A summary of the resampling results:
getTrainPerf(gbmTune)
getTrainPerf(svmTune)
getTrainPerf(fdaTune)
getTrainPerf(knnTune)
getTrainPerf(rdfTune)
# From these models, we can predict the evaluation set and save
# the probabilities of being the first class:
evalResults <- data.frame(Class = churnTest$churn)
evalResults$GBM <- predict(gbmTune, churnTest, type = "prob")[,"yes"]
evalResults$SVM <- predict(svmTune, churnTest, type = "prob")[,"yes"]
evalResults$FDA <- predict(fdaTune, churnTest, type = "prob")[,"yes"]
evalResults$KNN <- predict(knnTune, churnTest, type = "prob")[,"yes"]
#evalResults$RDF <- predict(rdfTune, churnTest, type = "prob")[,"yes"]
head(evalResults)
# plotting
trellis.par.set(caretTheme())
liftData <- lift(Class ~ GBM + SVM + FDA + KNN, data = evalResults)
plot(liftData, values = 60, auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))
# From this we can see that, to find 60 percent of the hits, aproximately
# 10 percent of the data can be sampled for the GBM model (when ordered by 
# the probability predictions). The KNN model does worse 
# than the other models. The best ferformance is for GBM Model


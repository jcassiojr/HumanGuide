# teste de target sendo o sexo do respondente
# aplicação: de uma base de pessoas que responderam ao teste, 
# prever o sexo (não sei se é muito útil!!)
# mas útil para simular turnover para testes
library(dplyr)
require("caret")
require("corrplot")
require("ggplot2")
require("gridExtra")
require("pROC")
require("ROCR")
require("xlsx")
require("doMC")
registerDoMC(5) # parallel processing
source("./R/f_le_dados_HG.R")
df_hg <- f_le_dados_HG()
# ----- calcula scores a partir dos dados originais de Human Guide
source("./R/f_calc_scores_HG.R")
df_scores_hg <- f_calc_scores_HG(df_hg)
df_scores_hg <-
    df_scores_hg %>%
    select (sexo, f_power, f_quality,
            exposure, structure, imagination, stability, contacts) %>%
    mutate(sexo = ifelse(sexo == 1, "m", "f"))
class <- as.factor(df_scores_hg[,1]) # transformando em vetor de fatores de target
descr <- df_scores_hg[,-1] # transformando em vetor de fatores de features

set.seed(1)
inTrain <- createDataPartition(class, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]

trainClass <- class[inTrain]
testClass  <- class[-inTrain]

descrCorr <- cor(trainDescr)
corrplot.mixed(descrCorr, insig = "p-value",sig.level = -1)

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

# tentar criar dataset somente com os de maior importância: imagintion e stability

control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary # comentar para uso com iris
)

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

models <- list(nb = nb_model)
testPred <- predict(models, newdata = testDescr, type = "prob")
predValues <- extractPrediction(
    models,
    testX = testDescr,
    testY = testClass)
testValues <- subset(
    predValues,
    dataType == "Test")
head(testValues)
table(testValues$model)
nrow(testDescr)
# probabilitie score
probValues <- extractProb(
    models,
    testX = testDescr,
    testY = testClass)

testProbs <- subset(
    probValues,
    dataType == "Test")
str(testProbs)
# NAIVE BAYES
nbProbs <- subset(testProbs,model == "nb")
# aqui, sortear nbProbs decrescente de coluna "s" e usando threshold
# considerar positivo "s" acima do threshold nos dados a prever!!!
pred <- prediction(nbProbs$m, nbProbs$obs)
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
               predictor = nbProbs[, "m"],
               levels = rev(levels(testClass)),
               plot = TRUE, print.auc = TRUE, 
               print.thres = c(.5,.2),print.thres.pch = 16,
               print.thres.cex = 1,2)

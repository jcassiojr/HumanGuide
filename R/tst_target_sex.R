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
# tree induction 
form <- as.formula(sexo ~ .)
tree.2 <- rpart(form,df_scores_hg)			# A more reasonable tree
prp(tree.2)   
# gerando probabilidades para rankear!
# A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle
# TESTE DE COST PARA FP e TP em ROC CURVE USING performence!!
pred <- prediction(nbProbs$m, nbProbs$obs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

library(ROCR)
data(ROCR.simple)
head(cbind(ROCR.simple$predictions, ROCR.simple$labels), 5)
# making a prediction object
pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
class(pred)
slotNames(pred)
sn = slotNames(pred)
sapply(sn, function(x) length(slot(pred, x)))
sapply(sn, function(x) class(slot(pred, x)))
# multiple set of predictions
data(ROCR.hiv)
manypred = prediction(ROCR.hiv$hiv.nn$predictions, ROCR.hiv$hiv.nn$labels)
sapply(sn, function(x) length(slot(manypred, x)))
sapply(sn, function(x) class(slot(manypred, x)))
# performance objects
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)
# multiset predictions
many.roc.perf = performance(manypred, measure = "tpr", x.measure = "fpr")
plot(many.roc.perf, col=1:10)
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
# getting optimal cut-point
opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))
        c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
          cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))
# ou usando custo
cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# different costs for TP and FN
# supondo que falso positivo (tratar alguém que não tem a doença) 
# é 2 vezes mais custoso que falso negativo (deixar de tratar alguém que tem 
# a doença)
cost.perf = performance(pred, "cost", cost.fp = 2, cost.fn = 1)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# ou o contrário, supondo que falso positivo (tratar alguém que não tem a doença = R$100.000) 
# é 10 vezes mais barato que falso negativo (deixar de tratar alguém que tem 
# a doença = R$ 1.000.000)
cost.perf = performance(pred, "cost", cost.fp = 1, cost.fn = 10)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# vale para multiplos predictions
print(opt.cut(many.roc.perf, manypred))
# accuracy vs cuttof (cuidado se existe skew não é confiável)
acc.perf = performance(pred, measure = "acc")
plot(acc.perf)

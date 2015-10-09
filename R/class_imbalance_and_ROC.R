# class inbalance and ROC
library(caret)

set.seed(442)
trainingSet <- twoClassSim(n = 500, intercept = -16)
testingSet  <- twoClassSim(n = 500, intercept = -16)

## Class frequencies (9:1 imbalance here)
table(trainingSet$Class)
# using ramdon forest and 10-fold cross validation
set.seed(949)
mod0 <- train(Class ~ ., data = trainingSet,
              method = "rf",
              metric = "ROC",
              tuneGrid = data.frame(mtry = 3),
              ntree = 1000,
              trControl = trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       classProbs = TRUE,
                                       summaryFunction = twoClassSummary))
getTrainPerf(mod0)

## Get the ROC curve
roc0 <- roc(testingSet$Class,
            predict(mod0, testingSet, type = "prob")[,1],
            levels = rev(levels(testingSet$Class)))
roc0

#-------------------------------------------
# alguns testes meus
#############
# criando a tabela de probabilidades
mod0Pred <- predict(mod0, testingSet, type = "prob")

df_1 <- cbind (mod0Pred,testingSet$Class)
names(df_1) <- c("Class1", "Class2", "actual.class")
library(dplyr)
df_2 <-
    df_1 %>%
    mutate (confmatr = ifelse(Class1 > 0.5 & actual.class == "Class1","TP",
                        ifelse(Class1 > 0.5 & actual.class == "Class2","FP",
                        ifelse(Class1 <= 0.5 & actual.class == "Class1","FN",
                        ifelse(Class1 <= 0.5 & actual.class == "Class2","TN",NA)))))
# entendendo ROC curve
# cada curva representa um modelo
# quanto maior a área sob a curva, maior a separação entre as classes
# positiva e negativa que seu modelo consegue (mais discriminante)
# Cada ponto na curva representa um threshold, ou seja, considerando
# a probabilidade que o modelo detecta um hit (true Positive) crescente
# Neste threshold, calcula-se nos dados de teste as taxas de TP e FP,
# gerando um ponto no gráfico. Fazemos este cálculo percorrendo as 
# probabilidades da menor para a maior, gerando a curva ROC para
# cada novo cálculo das taxas (cada cálculo, um ponto na linha
# definir um threshold em um modelo significa escolher uma probabilidade
# de corte para os true positives, onde acima dela vale a pena usar o modelo.
# Se escolhemos um threshold de 80% significa que selecionaremos de nossos
# dados somente aqueles onde o modelo previu mais de 80% de probabilidade de
# escolha de true positive
 
# aqui eu vou movendo o threshold para cima e para o que está acima calculo
# a taxa de TP/(Total actual P na coluna actual.class) e
# a taxa de FP/(total actual N na coluna actual.class)
#----------------------------------
## Now plot
plot(roc0, print.thres = c(.5), 
     type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)

## Get the model code for the original random forest method:
# AQUI ELE ESTÁ CRIANDO UM MODELO NOVO BASEADO NO MÉTODO RANDOM FOREST!!
thresh_code <- getModelInfo("rf", regex = FALSE)[[1]]
thresh_code$type <- c("Classification")
## Add the threshold as another tuning parameter
thresh_code$parameters <- data.frame(parameter = c("mtry", "threshold"),
                                     class = c("numeric", "numeric"),
                                     label = c("#Randomly Selected Predictors",
                                               "Probability Cutoff"))
## The default tuning grid code:
thresh_code$grid <- function(x, y, len = NULL) {
    p <- ncol(x)
    expand.grid(mtry = floor(sqrt(p)),
                threshold = seq(.01, .99, length = len))
}

## Here we fit a single random forest model (with a fixed mtry)
## and loop over the threshold values to get predictions from the same
## randomForest model.
thresh_code$loop = function(grid) {
    library(plyr)
    loop <- ddply(grid, c("mtry"),
                  function(x) c(threshold = max(x$threshold)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for(i in seq(along = loop$threshold)) {
        index <- which(grid$mtry == loop$mtry[i])
        cuts <- grid[index, "threshold"]
        submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
    }
    list(loop = loop, submodels = submodels)
}

## Fit the model independent of the threshold parameter
thresh_code$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if(length(levels(y)) != 2)
        stop("This works only for 2-class problems")
    randomForest(x, y, mtry = param$mtry, ...)
}

## Now get a probability prediction and use different thresholds to
## get the predicted class
thresh_code$predict = function(modelFit, newdata, submodels = NULL) {
    class1Prob <- predict(modelFit,
                          newdata,
                          type = "prob")[, modelFit$obsLevels[1]]
    ## Raise the threshold for class #1 and a higher level of
    ## evidence is needed to call it class 1 so it should 
    ## decrease sensitivity and increase specificity
    out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                  modelFit$obsLevels[1],
                  modelFit$obsLevels[2])
    if(!is.null(submodels)) {
        tmp2 <- out
        out <- vector(mode = "list", length = length(submodels$threshold))
        out[[1]] <- tmp2
        for(i in seq(along = submodels$threshold)) {
            out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                                 modelFit$obsLevels[1],
                                 modelFit$obsLevels[2])
        }
    }
    out
}

## The probabilities are always the same but we have to create
## mulitple versions of the probs to evaluate the data across
## thresholds
thresh_code$prob = function(modelFit, newdata, submodels = NULL) {
    out <- as.data.frame(predict(modelFit, newdata, type = "prob"))
    if(!is.null(submodels)) {
        probs <- out
        out <- vector(mode = "list", length = length(submodels$threshold)+1)
        out <- lapply(out, function(x) probs)
    }
    out
}
# How do we optimize this model? Normally we might look at the area under 
# the ROC curve as a metric to choose our final values. In this case the 
# ROC curve is independent of the probability threshold so we have to use 
# something else. A common technique to evaluate a candidate threshold is
# see how close it is to the perfect model where sensitivity and 
# specificity are one. Our code will use the distance between the current
# model's performance and the best possible performance and then have 
# train minimize this distance when choosing it's parameters. Here is the
# code that we use to calculate this:
fourStats <- function (data, lev = levels(data$obs), model = NULL) {
    ## This code will get use the area under the ROC curve and the
    ## sensitivity and specificity values using the current candidate
    ## value of the probability threshold.
    out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
    
    ## The best possible model has sensitivity of 1 and specificity of 1. 
    ## How far are we from that value?
    coords <- matrix(c(1, 1, out["Spec"], out["Sens"]),
                     ncol = 2,
                     byrow = TRUE)
    colnames(coords) <- c("Spec", "Sens")
    rownames(coords) <- c("Best", "Current")
    c(out, Dist = dist(coords)[1])
}

set.seed(949)
mod1 <- train(Class ~ ., data = trainingSet,
              method = thresh_code,
              ## Minimize the distance to the perfect model
              metric = "Dist",
              maximize = FALSE,
              tuneLength = 20,
              ntree = 1000,
              trControl = trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       classProbs = TRUE,
                                       summaryFunction = fourStats))

mod1
# salvando resultados em dataframe
df_res <- mod1$results
df_thres <- mod1$bestTune
# Using ggplot(mod1) will show the performance profile. Instead
# here is a plot of the sensitivity, specificity, and distance 
# to the perfect model:
# You can see that as we increase the probability cut off for
# the first class it takes more and more evidence for a sample
# to be predicted as the first class. As a result the sensitivity
# goes down when the threshold becomes very large. The upside is
# that we can increase specificity in the same way. The blue 
# curve shows the distance to the perfect model. The value of
# 0.89 was found to be optimal.
library(reshape2)
metrics <- mod1$results[, c(2, 4:6)]
metrics <- melt(metrics, id.vars = "threshold",
                variable.name = "Resampled",
                value.name = "Data")

ggplot(metrics, aes(x = threshold, y = Data, color = Resampled)) +
    geom_line() +
    ylab("") + xlab("Probability Cutoff") +
    theme(legend.position = "top")

# ploting the ROC Curve with the best threshold
## Now plot
plot(roc0, print.thres = c(.8868421), 
     type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)


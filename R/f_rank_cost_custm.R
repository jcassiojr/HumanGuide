#' função que recebe a lista de modelos e o tipo a tratar e retorna lista com:
#' 1. ROC object
#' 2. confusion matrix
#' 3. dataframe rankeado por melhor custo, segundo relação cost.fp/cost.fn
#' 
#' 
# different costs for TP and FN
# Target = turnover (S/N)
# TP: deixo de contratar e daria turnover
# FP: deixo de contratar e NÃO daria turnover
# TN: contrata e NÃO dá turnover
# FN: contrata e dá turnover
# ver planilha de Projeto Human Guide-V1.90.xlsx para os custos
# dá uma taxa de FN/TP cd 1/10 aproximadamente (pode ser refinada)

require("caret")
require("pROC")
f_rank_cost_custm <- function(models, tipo_mod) {
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        models,
        testX = testDescr,
        testY = testClass)
    # obtém somente o subset de dados de teste
    testProbs <- subset(
        probValues,
        dataType == "Test")
    # tipo_mod = "svmRadial" 
    svmProbs <- subset(testProbs, model == tipo_mod)
    
    # Confusion Matrix
    #-----------------
    svm_cf <- confusionMatrix(svmProbs$pred, svmProbs$obs)
    
    # a ROC curve creating performance object
    #------------------------------------------
    # making a prediction object
    pred <- prediction(svmProbs$m, svmProbs$obs)
    # obtendo objeto roc
    roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
    # melhor cuttof para relação de custo de relação cost.fp/cost.fn
    cost.perf = performance(pred, "cost", cost.fp = 1, cost.fn = 10)
    pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
    
    # Criar um data.frame com as probabilidades até o índice obtido no objeto performance
    ######################################################################################
    # obtenho o valor de cutoff obtido da função acima
    valor_cutoff <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
    # crio data frame com as probabilidades do preditor (já ordenado)
    df_cost_custm <- data.frame (my_pred = roc.perf@alpha.values[[1]])
    # obtém dataframe final apenas com as probabilidades acima do cutoff escolhido
    df_cost_custm <-
        df_cost_custm %>%
        filter (df_cost_custm >= valor_cutoff)
    
    # retorna a lista
    return(list(svm_cf, cost.perf, df_cost_custm))
}
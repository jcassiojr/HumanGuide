#' função que, a partir do modelo treinado, obtem a confusion matrix, a previsao e performance
#' segundo métrica ROC e lista de probabilidades rankeadas até um cutpoint definido
#' por valor de FP rate máximo definido
#' Pode ser usado para comparação de modelos, quando passados dados de teste ou
#' para uso em dados a prever sem classe definida, quando passados estes dados
#' Parâmetros: recebe a lista de modelos, o tipo de modelo a tratar dentro da lista,
#' as features de treino, as classes de treino
#' Retorno:  lista com:
#' 1. ROC object
#' 2. confusion matrix
#' 3. dataframe rankeado por cutoof de fp rate
#'  

require("caret")
require("pROC")

f_rank_custmCost_models <- function(models, testClass, testDescr, cost_fp, cost_fn) {
    
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        models,
        testX = testDescr,
        testY = testClass)
    # getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
    #--------------------------------------------------------------------------
    
    # trabalha os dados para cada modelo da lista
    l_cf <- list() # cria lista de confusion matrix
    l_rocPerf <- list() # cria lista de objetos de performance ROC
    l_valor_cutoff <- list() # cria lista de valores de cutoff
    #l_probModel <- list()
    for (i in unique(probValues$model)) {
        #print(i)
        df_testValues <- subset(
            probValues,
            model == i & dataType == "Test" 
        )
        # Confusion Matrix
        #-----------------
        l_cf[[i]] <- confusionMatrix(df_testValues$pred, df_testValues$obs)
        
        # a ROC curve creating performance object
        #------------------------------------------
        # making a prediction object
        pred <- prediction(df_testValues$m, df_testValues$obs)
        
        # obtendo o objeto roc
        l_rocPerf[[i]] = performance(pred, measure = "tpr", x.measure = "fpr")
    
        # obtendo o valor de cutoff
        costPerf = performance(pred, "cost", cost.fp = cost_fp, cost.fn = cost_fn)
        pred@cutoffs[[1]][which.min(costPerf@y.values[[1]])]
        l_valor_cutoff[[i]] <- pred@cutoffs[[1]][which.min(costPerf@y.values[[1]])]
    }
    
    # retorna a lista de lista por modelo
    return(list(rocPerf = l_rocPerf, cf = l_cf, cutoff = l_valor_cutoff))
}
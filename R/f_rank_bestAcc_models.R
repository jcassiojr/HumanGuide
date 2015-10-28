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

f_rank_bestAcc_models <- function(i_models, i_testClass, i_testDescr) {
    
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        i_models,
        testX = i_testDescr,
        testY = i_testClass)
    # getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
    #--------------------------------------------------------------------------
    
    # trabalha os dados para cada modelo da lista
    l_cf <- list() # cria lista de confusion matrix
    l_rocPerf <- list() # cria lista de objetos de performance ROC
    l_valor_cutoff <- list() # cria lista de valores de cutoff
    l_best_acc <- list() # cria lista de valores de acurácia x cutoff
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
        # melhor cuttof para relação de custo de relação cost.fp/cost.fn
        accPerf = performance(pred, measure = "acc")
        
        # pegamos o máximo de acurácia do objeto performance
        ind = which.max( slot(accPerf, "y.values")[[1]] )
        acc = slot(accPerf, "y.values")[[1]][ind]
        cutoff = slot(accPerf, "x.values")[[1]][ind]
       
        # obtenho o valor de cutoff obtido da função acima
        l_valor_cutoff[[i]] <- slot(accPerf, "x.values")[[1]][ind]
        
        # cria dataframe de acurácias para permitir plotar acc x cutoff
        #l_best_acc[[i]] <- data.frame (my_pred = l_rocPerf[[i]]@alpha.values[[1]])
    }
    
    # retorna a lista de lista por modelo
    return(list(rocPerf = l_rocPerf, cf = l_cf, cutoff = l_valor_cutoff))
}
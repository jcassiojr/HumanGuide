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

f_rank_bestBal_models <- function(i_models, i_testClass, i_testDescr) {
    
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        i_models,
        testX = i_testDescr,
        testY = i_testClass)
    # getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
    #--------------------------------------------------------------------------
    opt.cut = function(perf, pred){
        cut.ind = mapply(FUN=function(x, y, p){
            d = (x - 0)^2 + (y-1)^2
            ind = which(d == min(d))
            c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
              cutoff = p[[ind]])
        }, perf@x.values, perf@y.values, pred@cutoffs)
    }
    
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
        l_rocPerf [[i]] = performance(pred, measure = "tpr", x.measure = "fpr")
        
        # obtendo o valor de cutoff
        l_valor_cutoff[[i]] <- opt.cut(l_rocPerf[[i]], pred)[3]
    }
    
    # retorna a lista de lista por modelo
    return(list(rocPerf = l_rocPerf, cf = l_cf, cutoff = l_valor_cutoff))
}
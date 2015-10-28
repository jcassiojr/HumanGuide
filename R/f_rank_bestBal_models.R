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

f_rank_bestBal_models <- function(models, testClass, testDescr) {
    
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        models,
        testX = testDescr,
        testY = testClass)
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
    
    # ++ +TESTE
    # carrega lista com um data.frame de probabilidades de classe para cada modelo
    #for (i in 1:length(i_models)) {
    #print (i_models[[i]])
    #    df_probs <- predict(i_models[[i]], i_useDescr[,-1], type = "prob") 
    # monta lista de data.frames com ID e probabilidade positiva (classe = "m")
    #    l_id_probs[[i]] <- data.frame(ID = v_id, probClass = df_probs$m)
    # ordenando em ordem decrescente as probabilidades
    # e aplicando cutoff ao dataframe
    #    l_id_probs[[i]] <-
    #        l_id_probs[[i]] %>%
    #        arrange(desc(probClass)) %>%
    #        filter(probClass >= i_valor_cutoff) # filtra dados até valor de cutoff
    #}
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
        #proc.perf = pROC(pred, fpr.stop=cutoff)
        #print(opt.cut(l_roc.perf[[i]], pred))
        # obtenho o índice de cutoff obtido da função acima
        l_valor_cutoff[[i]] <- opt.cut(l_rocPerf[[i]], pred)[3]
        #indice_cutoff <- length(proc.perf@x.values[[1]])
        
        # crio data frame com as probabilidades do preditor (já ordenado)
        #df_max_fpr <- data.frame (my_pred = proc.perf@alpha.values[[1]])
        #df_otpm <- data.frame (my_pred = l_rocPerf[[i]]@alpha.values[[1]])
        
        # salva o valor da probabilidade de cutoff para posterior identificação
        # da probabilidade de corte quando chamar a função de previsão 
        # f_prev_FPrate()
        #l_valor_cutoff[[i]] <- df_otpm[indice_cutoff,]
        # obtém dataframe final apenas com as probabilidades acima do cutoff escolhido
        #df_max_fpr <- head(df_max_fpr,indice_cutoff)
    }
    
    # retorna a lista de lista por modelo
    return(list(rocPerf = l_rocPerf, cf = l_cf, cutoff = l_valor_cutoff))
}
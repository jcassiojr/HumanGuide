#' função que recebe a lista de modelos e o tipo a tratar e retorna lista com:
#' 1. ROC object
#' 2. confusion matrix
#' 3. dataframe rankeado por cutoof de fp rate
#' 

require("caret")
require("pROC")
f_rank_optmRate <- function(models, tipo_mod) {
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
    
    roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
    
    # criando o dataframe rankeado
    #-----------------------------
    pROC = function(pred, fpr.stop){
        perf <- performance(pred,"tpr","fpr")
        for (iperf in seq_along(perf@x.values)){
            ind = which(perf@x.values[[iperf]] <= fpr.stop)
            perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
            perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
        }
        return(perf)
    }
    proc.perf = pROC(pred, fpr.stop=cutoff)
    # plot(proc.perf)
    # abline(a=0, b= 1)
    
    # obtenho o índice de cutoff obtido da função acima
    indice_cutoff <- length(proc.perf@x.values[[1]])
    
    # crio data frame com as probabilidades do preditor (já ordenado)
    df_final <- data.frame (my_pred = proc.perf@alpha.values[[1]])
    
    # obtém dataframe final apenas com as probabilidades acima do cutoff escolhido
    df_final <- head(df_final,indice_cutoff)
    
    # retorna a lista
    return(list(svm_cf, roc.perf, df_final))
}
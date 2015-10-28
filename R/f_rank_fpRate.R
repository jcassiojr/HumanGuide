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

# CONSIDERACOES GERAIS SOBRE ROC CURVES
# These characteristics of ROC graphs have become increasingly important
# as research continues into the areas of cost-sensitive learning and 
# learning in the presence of unbalanced classes.
# um ponto no espaço ROC representa um modleo que retorna true/false para
# a pergunta se a instância pertence ou não á classe alvo
# um ponto no espaço ROC representa uma única confusion matrix
# (uma decision tree gera, portanto, um ponto no espaço ROC)
# o lado direito superior do espaço ROC é considerado mais liberal
# o lado esquerdo é considerado conservador
# probabilistic classifiers:
# Naive Bayes ou Neural Network classifier gera uma probabilidade ou score de cada instância
# (grau no qual a instância pertence à classe), gerando uma curva ROC para as instâncias
# consideradas
# este tipo de classifier permite criar um threshold para criar um classifier binário,
# ou seja, acima dele considera-se positivo para a classe e abaixo dele, negativo
# baixando o threshold vou movendo da área mais conservativa para a área mais liberal.
# Assim, seleciona-se o threshold com melhor custo x benefício ou acurácia, ou seja,
# acima dele o modelo retorna Y e abaix N!!!
# A classifier need not produce accurate, calibrated probability estimates; 
# it need only produce relative accurate scores that serve to discriminate positive 
# and negative instances
# ROC curves have an attractive property: they are insensitive to changes in class
# distribution.


# IMPORTANTE: Os valores de alpha.value do objeto performance são os mesmos dos valores
# das probabilidades da coluna "m" de svmProbs ordenada em ordem decrescente!!! 
# que, por sua vez são os mesmos valores de pred@predictions[[1]]

require("caret")
require("pROC")

f_rank_fpRate <- function(models, testClass, testDescr, tipo_mod, cutoff) {
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
    
    roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
    
    # criando o dataframe rankeado por cutoff em FP Rate
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
    df_max_fpr <- data.frame (my_pred = proc.perf@alpha.values[[1]])
    
    # salva o valor da probabilidade de cutoff para posterior identificação
    # da probabilidade de corte quando chamar a função de previsão 
    # f_prev_FPrate()
    valor_cutoff <- df_max_fpr[indice_cutoff,]
    # obtém dataframe final apenas com as probabilidades acima do cutoff escolhido
    df_max_fpr <- head(df_max_fpr,indice_cutoff)
    
    # retorna a lista
    return(list(svm_cf, roc.perf, df_max_fpr, valor_cutoff))
}
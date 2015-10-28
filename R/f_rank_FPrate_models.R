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

f_rank_fpRate_models <- function(i_models, i_testClass, i_testDescr, cutoff) {
    
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        i_models,
        testX = i_testDescr,
        testY = i_testClass)
    
    # funcao criando o dataframe rankeado por cutoff em FP Rate
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

    # trabalha os dados para cada modelo da lista
    l_cf <- list() # cria lista de confusion matrix
    l_rocPerf <- list() # cria lista de objetos de performance ROC
    l_valor_cutoff <- list() # cria lista de valores de cutoff
    #l_probModel <- list()
    for (i in unique(probValues$model)) {
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
        proc.perf = pROC(pred, fpr.stop=cutoff)
        
        # crio data frame com as probabilidades do preditor (já ordenado)
        df_max_fpr <- data.frame (my_pred = proc.perf@alpha.values[[1]])
        
        # obtenho o índice de cutoff obtido da função acima
        indice_cutoff <- length(proc.perf@x.values[[1]])
        
        # f_prev_FPrate()
        l_valor_cutoff[[i]] <- df_max_fpr[indice_cutoff,]
    }
    
    # retorna a lista de lista por modelo
    return(list(rocPerf = l_rocPerf, cf = l_cf, cutoff = l_valor_cutoff))
}
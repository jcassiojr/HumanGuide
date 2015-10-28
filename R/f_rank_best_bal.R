#' função que recebe a lista de modelos e o tipo a tratar e retorna lista com:
#' 1. ROC object
#' 2. confusion matrix
#' 3. dataframe rankeado por cutoof de fp rate
#' 
#' # CONSIDERACOES GERAIS SOBRE ROC CURVES
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
f_rank_best_bal <- function(models, tipo_mod) {
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
    
    
    # getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
    
    opt.cut = function(perf, pred){
        cut.ind = mapply(FUN=function(x, y, p){
            d = (x - 0)^2 + (y-1)^2
            ind = which(d == min(d))
            c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
              cutoff = p[[ind]])
        }, perf@x.values, perf@y.values, pred@cutoffs)
    }
    print(opt.cut(roc.perf, pred))
    
    # Criar um data.frame com as probabilidades até o índice obtido no objeto performance
    ######################################################################################
    # obtenho o valor de cutoff obtido da função acima
    valor_cutoff <- opt.cut(roc.perf, pred)[3]
    # crio data frame com as probabilidades do preditor (já ordenado)
    df_otpm <- data.frame (my_pred = roc.perf@alpha.values[[1]])
    # obtém dataframe final apenas com as probabilidades acima do cutoff escolhido
    df_otpm <-
        df_otpm %>%
        filter (df_otpm >= valor_cutoff)
    
    # retorna a lista
    return(list(svm_cf, roc.perf, df_otpm, valor_cutoff))
}
#' função que recebe a lista de modelos e o tipo a tratar e retorna lista com:
#' 1. ROC object
#' 2. confusion matrix
#' 3. dataframe rankeado por melhor custo, segundo custo default TPR = 1 FNR = 1
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
f_rank_cost_dflt <- function(models, tipo_mod) {
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
    # melhor cuttof para relação de custo padrão de FPR = 1 e NPR = 1
    cost.perf = performance(pred, "cost")
    
    # Criar um data.frame com as probabilidades até o índice obtido no objeto performance
    ######################################################################################
    # obtenho o valor de cutoff obtido da função acima
    valor_cutoff <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
    # crio data frame com as probabilidades do preditor (já ordenado)
    df_cost_dftl <- data.frame (my_pred = roc.perf@alpha.values[[1]])
    # obtém dataframe final apenas com as probabilidades acima do cutoff escolhido
    df_cost_dftl <-
        df_cost_dftl %>%
        filter (df_cost_dftl >= valor_cutoff)
    
    # retorna a lista
    return(list(svm_cf, cost.perf, df_cost_dftl, valor_cutoff))
}
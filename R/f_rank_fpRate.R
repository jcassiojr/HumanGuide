#' função que recebe a lista de modelos e o tipo a tratar e retorna lista com:
#' 1. plot ROC curve
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

f_rank_fpRate <- function(models, tipo_mod) {
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
    # making a prediction object
    pred <- prediction(svmProbs$m, svmProbs$obs)
    
    # Confusion Matrix
    #-----------------
    svm_cf <- confusionMatrix(svmPred$pred, svmPred$obs)
    svmPred <- subset(testProbs, model == "svmRadial")
    svmProbs <- subset(testProbs,model == "svmRadial")
    return(list(svm_cf))
}
#'função de aplicação dos modelos

require(caret)
f_apply_models <- function(trainClass, trainDescr){
    ## Building and tuning models (métodos para regression (ou dual) in caret)
    # definindo os parâmetros de controle para uso nos modelos
    # default metrics to estimate best model is Kappa (great for
    # unbalanced classes) e Accuracy
    # se quisermos estimar sensitivity and specificity, ROC and AUC
    # we need to tell train to produce class probability, estimate
    # these statistics and to rank models by the ROC AUC
    # twoClassSummary function calculates all of this
    control <- trainControl(method="repeatedcv", number=10, repeats=3,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary # comentar para uso com iris
    )
    
    # SVM MODEL
    #---------------
    set.seed(2)
    svm_model <- train(
        trainDescr, trainClass,
        method = "svmRadial",
        tuneLength = 5,
        metric = "ROC",
        preProcess = c("scale", "center"),
        trControl = control,
        scaled = FALSE)
    
  
#    svm_model
#    svm_model$finalModel
#    ggplot(svm_model) + theme(legend.position = "top")
    
    # estimating feature importance
#    svm_importance <- varImp(svm_model, scale=FALSE)
    # summarize importance
#    print(svm_importance)
    # plot importance
#    plot(svm_importance)
    
    # STOCHASTIC GRADIENT BOOST MODEL (GBM)
    #---------------
    # At each step of the GBM algorithm, a new decision tree is constructed. 
    # The question when growing a decision tree is 'when to stop?'. The 
    # furthest you can go is to split each node until there is only 1 
    # observation in each terminal node. This would correspond to 
    # n.minobsinnode=1. Alternatively, the splitting of nodes can cease 
    # when a certain number of observations are in each node. The default 
    # for the R GBM package is 10.
    # usando abaixo tunning mais sofisticado do grid
    gbmGrid <- expand.grid(
        .interaction.depth = (1:5) * 2,
        .n.trees = (1:10)*25,
        .shrinkage = .1,
        .n.minobsinnode = 1)
    set.seed(2)
    gbm_model <- train(
        trainDescr, trainClass,
        method = "gbm",
        trControl = control,
        verbose = FALSE,
        metric = "ROC",
        preProcess=c("center", "scale"),
        bag.fraction = 0.5,                
        tuneGrid = gbmGrid)
    
#    gbm_model
#    gbm_model$finalModel
##    ggplot(gbm_model) + theme(legend.position = "top")
    
    # estimating feature importance
#    gbm_importance <- varImp(gbm_model, scale=FALSE)
    # summarize importance
#    print(gbm_importance)
    # plot importance
#    plot(gbm_importance)
    
    # TREE BAG MODEL
    #---------------
    tbg_model <- train(trainDescr, trainClass, 
                       nbagg = 50,
                       metric = "ROC",
                       preProcess=c("center", "scale"),
                       trControl=control, 
                       method="treebag")
    
#    tbg_model
##    tbg_model$finalModel
    
    # estimating feature importance
#    tbg_importance <- varImp(tbg_model, scale=FALSE)
    # summarize importance
#    print(tbg_importance)
#    plot(tbg_importance)
    
    # CONDITIONAL INFERENCE TREE MODEL
    #-------
    ctree2_model <- train(trainDescr, trainClass, 
                          metric = "ROC",
                          preProcess=c("center", "scale"),
                          trControl=control, 
                          method="ctree2")
#    ctree2_model
#    ctree2_model$finalModel
#    ggplot(ctree2_model) + theme(legend.position = "top")
    
    # estimating feature importance
#    ctree2_importance <- varImp(ctree2_model, scale=FALSE)
    # summarize importance
#    print(ctree2_importance)
    # plot importance
#    plot(ctree2_importance)
    
    # BAYESIAN GENERALIZING LINEAR MODEL
    #----------
    bglm_model <- train(trainDescr, trainClass, 
                        metric = "ROC",
                        preProcess=c("center", "scale"),
                        trControl=control,
                        method="bayesglm")
#    bglm_model
#    bglm_model$finalModel
    
    # estimating feature importance
#    bglm_importance <- varImp(bglm_model, scale=FALSE)
    # summarize importance
#    print(bglm_importance)
    # plot importance
#    plot(bglm_importance)
    
    # GENERALIZING LINEAR MODEL
    #----------
    glm_model <- train(trainDescr, trainClass, 
                       metric = "ROC",
                       preProcess=c("center", "scale"),
                       trControl=control, 
                       method="glm")
#    glm_model
#    glm_model$finalModel
    
    # estimating feature importance
#    glm_importance <- varImp(glm_model, scale=FALSE)
    # summarize importance
#    print(glm_importance)
    # plot importance
#    plot(glm_importance)
    
    # BOOSTED LOGISTIC REGRESSION MODEL
    #---------
    logb_model <- train(trainDescr, trainClass, 
                        #nbagg = 50,
                        metric = "ROC",
                        preProcess=c("center", "scale"),
                        trControl=control,  
                        method="LogitBoost")
#    logb_model
#    logb_model$finalModel
#    ggplot(logb_model) + theme(legend.position = "top")
    
    # estimating feature importance
#    logb_importance <- varImp(logb_model, scale=FALSE)
    # summarize importance
#    print(logb_importance)
    # plot importance
#    plot(logb_importance)
    
    # NAIVE BAYES MODEL
    #---------
    nb_model <- train(trainDescr, trainClass, 
                      #nbagg = 50,
                      metric = "ROC",
                      preProcess=c("center", "scale"),
                      trControl=control,  
                      method="nb")
    
#    nb_model
#    nb_model$finalModel
#    ggplot(nb_model) + theme(legend.position = "top")
    
    # estimating feature importance
#    nb_importance <- varImp(nb_model, scale=FALSE)
    # summarize importance
#    print(nb_importance)
    # plot importance
#    plot(nb_importance)
    
    # consolidando previsões de diversos modelos eu um output
    models <- list(svm = svm_model,
                   gbm = gbm_model,
                   tbg = tbg_model,
                   ctree2 = ctree2_model,
                   bglm = bglm_model,
                   glm = glm_model,
                   logb = logb_model,
                   nb = nb_model)
    return(models)
}
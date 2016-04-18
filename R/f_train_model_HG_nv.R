#'função que treina modelo de data.frame passado
#' campo_in -> numero do código de atuação (1 a 10)
#' df_tidy_in -> dataframe a partir de mt.score.total com colunas: ID, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7

require("caret")
require("MASS")
require("ROCR")
#library("lubridate")
#require("doMC")
f_train_model_HG_nv <- function(df_tidy_in, campo_in) {
    
    #registerDoMC(8) # parallel processing
    #preparando modelo para target campo = CFM
    #df_tidy_in <- my.scores.total
    #df_tidy_in <- df_change
    #df_tidy_in <-
    #    df_tidy_in %>%
    #    mutate(class.carr = ifelse(class.carr == campo_in, campo_in, "outros"))
    df_tidy_in <-
        df_tidy_in %>%
        mutate(target = ifelse(target == campo_in,"T","F"))
    
    #selecionando as features e target em arquivos diferentes
    class <- as.factor(df_tidy_in[,"target"]) # transformando em vetor de fatores de target
    class <- factor(class, levels = c("T","F")) # ordenando levels para "S" ser o primeiro
    # modelo sem precisão suficiente. Tentar tirando as duplicidades
    #df_tidy_in <-
    #    df_tidy_in %>%
    #    distinct(ID)
    
    # ABAIXO USANDO VALOR DEVIDO SEM AGRUPAR E USANDO SOMENTE FEATURES DO ARQUIVO AVON PARA PODER PREVER DEPOIS
    descr <- df_tidy_in[,c(3:9)] # Obs: depois trocar Valor Devido por Faixa para ver se melhora o modelo!!
   
    set.seed(1)
    inTrain <- createDataPartition(class, p = 3/4, list = FALSE)
    
    trainDescr <- descr[inTrain,]
    testDescr  <- descr[-inTrain,]
    
    trainClass <- class[inTrain]
    testClass  <- class[-inTrain]
    
    
    # REMOVENDO NEAR ZERO VARIANCE AND CORRELATIONS 
    # (FOR CORRELATION, NUMERIC FEATURES ONLY)
    ######################################################
    trn_nzvar <- nearZeroVar(trainDescr, freqCut = 20, uniqueCut = 20)
    tst_nzvar <- nearZeroVar(testDescr, freqCut = 20, uniqueCut = 20)
    # removendo colunas que não passaram no teste
    if(length(trn_nzvar) != 0  || length(tst_nzvar) != 0) {
        trainDescr <- trainDescr[,-(trn_nzvar)]
        testDescr <- testDescr[,-(tst_nzvar)]
    }

    # eliminando features com menor importância 
    ###################################################
    #trainTotal <- cbind(sexo = trainClass,trainDescr)
    initial <- glm(class.carr ~ ., data = cbind(class.carr = trainClass,trainDescr), family = "binomial")
    aic_o <- stepAIC(initial, direction = "both", trace = FALSE)
    
    #######################################
    ## BUILDING AND TUNING MODELS
    # O NAIVE BAYES PARECE Se MOSTROU MELHOR COMPARADO COM OS TESTADOS ABAIXO
    # GBM, TBG, CTREE2, GLM
    #######################################
    control <- trainControl(method="repeatedcv", number=10, repeats=3,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary # comentar para uso com iris
    )
    
    # BOOSTED LOGISTIC REGRESSION MODEL
    #---------
    logb_model <- train(trainDescr, trainClass, 
                        #nbagg = 50,
                        metric = "ROC",
                        preProcess=c("center", "scale"),
                        trControl=control,  
                        method="LogitBoost")
    #nb_model <- train(trainDescr, trainClass, 
    #                  #nbagg = 50,
    #                  metric = "ROC",
    #                  preProcess=c("center", "scale"),
    #                  trControl=control,
    #                  na.action=na.omit,
    #                  method="nb")
    
    models_o <- list(logb = logb_model)
    
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        models_o,
        testX = testDescr,
        testY = testClass)
    
    # pegar subset somente com dados de teste para validar o modelo
    #testProbs <- subset(
    #    probValues,
    #    dataType == "Test")
    
    # confusion matrix
    cf_o <- confusionMatrix(probValues$pred, probValues$obs)

    # making a prediction object
    pred_o <- prediction(probValues$T, probValues$obs)
    
    # ROC curve
    roc.perf_o = performance(pred_o, measure = "tpr", x.measure = "fpr")
    
    
    # RETORNAR AUC
    roc.auc_o = performance(pred_o, measure = "auc")
    auc <- roc.auc_o@y.values
   
    # crio data frame com as probabilidades do preditor (já ordenado)
    #df_rank_o <- data.frame (my_pred = roc.perf_o@alpha.values[[1]])

    # retorna lista para validação do modelo
    l_o <- list(models_o,aic_o,cf_o, roc.perf_o, roc.auc_o, pred_o)
    return(l_o)
    
    
    
    # ABAIXO COMENTADOS TESTE DE OUTROS MODELOS 
    #--------------------------------
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
    #gbmGrid <- expand.grid(
    #    .interaction.depth = (1:5) * 2,
    #    .n.trees = (1:10)*25,
    #    .shrinkage = .1,
    #    .n.minobsinnode = 1)
    #set.seed(2)
    #gbm_model <- train(
    #    trainDescr, trainClass,
    #    method = "gbm",
    #    trControl = control,
    ##    verbose = FALSE,
     #   metric = "ROC",
    #    preProcess=c("center", "scale"),
    #    bag.fraction = 0.5,                
    #    tuneGrid = gbmGrid)
    
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
    #tbg_model <- train(trainDescr, trainClass, 
    #                   nbagg = 50,
    #                   metric = "ROC",
    #                   preProcess=c("center", "scale"),
    #                   trControl=control, 
    #                   method="treebag")
    
    #    tbg_model
    ##    tbg_model$finalModel
    
    # estimating feature importance
    #    tbg_importance <- varImp(tbg_model, scale=FALSE)
    # summarize importance
    #    print(tbg_importance)
    #    plot(tbg_importance)
    
    # CONDITIONAL INFERENCE TREE MODEL
    #-------
    #ctree2_model <- train(trainDescr, trainClass, 
    #                      metric = "ROC",
    #                      preProcess=c("center", "scale"),
    #                      trControl=control, 
    #                      method="ctree2")
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
    #bglm_model <- train(trainDescr, trainClass, 
    #                    metric = "ROC",
    #                    preProcess=c("center", "scale"),
    #                    trControl=control,
    #                    method="bayesglm")
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
    #glm_model <- train(trainDescr, trainClass, 
    #                   metric = "ROC",
    #                   preProcess=c("center", "scale"),
    #                   trControl=control, 
    #                   method="glm")
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
    #logb_model <- train(trainDescr, trainClass, 
    #                    #nbagg = 50,
    #                    metric = "ROC",
    ##                    preProcess=c("center", "scale"),
    #                    trControl=control,  
    #                    method="LogitBoost")
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
    #nb_model <- train(trainDescr, trainClass, 
    #                  #nbagg = 50,
    #                  metric = "ROC",
    #                  preProcess=c("center", "scale"),
    #                  trControl=control,  
    #                  method="nb")
    
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
    #models <- list(
    #                #svm = svm_model,
    #               gbm = gbm_model,#
    #               tbg = tbg_model,#
    #               ctree2 = ctree2_model,#
    #               #bglm = bglm_model,
    #               glm = glm_model,#
    #               #logb = logb_model,
    #               nb = nb_model) #
    
    
    #probValues <- extractProb(
    #    models,
    #    testX = testDescr,
    #    testY = testClass)
    # obtém somente o subset de dados de teste
    #testProbs <- subset(
    #    probValues,
    #    dataType == "Test")
    #table(testProbs$model)
    # ESTE PARECE SER O MELHOR RESULTADO
    #tipo_mod = "gbm"
    #gbmProbs <- subset(testProbs, model == tipo_mod)
    #gbm_cf <- confusionMatrix(gbmProbs$pred, gbmProbs$obs)
    #gbm_pred <- prediction(gbmProbs$S, gbmProbs$obs)
    ##roc.perf = performance(gbm_pred, measure = "tpr", x.measure = "fpr")
#    plot(roc.perf)
    #abline(a=0, b= 1)
    
    #tipo_mod = "ctree2"
    #ctree2Probs <- subset(testProbs, model == tipo_mod)
    #ctree2_cf <- confusionMatrix(ctree2Probs$pred, ctree2Probs$obs)
    #ctree2_pred <- prediction(ctree2Probs$S, ctree2Probs$obs)
    #roc.perf = performance(ctree2_pred, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf)
    #abline(a=0, b= 1)
    
    #tipo_mod = "glm"
    ##glmProbs <- subset(testProbs, model == tipo_mod)
    #glm_cf <- confusionMatrix(glmProbs$pred, glmProbs$obs)
    #glm_pred <- prediction(glmProbs$S, glmProbs$obs)
    #roc.perf = performance(glm_pred, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf)
    #abline(a=0, b= 1)
    
    #tipo_mod = "nb"
    #nbProbs <- subset(testProbs, model == tipo_mod)
    #nb_cf <- confusionMatrix(nbProbs$pred, nbProbs$obs)
    #nb_pred <- prediction(nbProbs$S, nbProbs$obs)
    #roc.perf = performance(nb_pred, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf)
    #abline(a=0, b= 1)
    
    #tipo_mod = "treebag"
    #treebagProbs <- subset(testProbs, model == tipo_mod)
    #treebag_cf <- confusionMatrix(treebagProbs$pred, treebagProbs$obs)
    ##treebag_pred <- prediction(treebagProbs$S, treebagProbs$obs)
    #roc.perf = performance(treebag_pred, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf)
    #abline(a=0, b= 1)
  
}
#'função que usa o  modelo de data.frame passado
#' campo_in -> numero do código de atuação (1 a 10)
#' df_tidy_in -> dataframe a partir de mt.score.total com colunas: ID, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7
#' df_test_in -> dataframe a partir de outros respondentes, com mesmo formato acima

#require("caret")
#require("MASS")
#require("ROCR")

f_usa_model_HG_form <- function(df_train_in, df_test_in, campo_in) {
    
    #preparando modelo para target campo = CFM
    #df_train_in <- my.train.atua
    #df_test_in <- my.usa.atua
    #campo_in <- 28
    # 86(49LG, 56 NB) 13(0.4983965, 0,57NB) 76(0.5335106, 0.65NB) 73(0.5302266, .68NB ) 71(0.5392586, .66NB)  1(0.4996424, .57NB) 70(0.592213, 0.69nb) 46(0.6112179)  3(0.548364)
    # 12(0.4832399,0.52nb) 45(0.70NB) 79(0.64nb) 35(.47nb) 81(.65nb)  8(.61nb)  4(0.58nb)  6(.58nb) 17(.58nb) 10(.66nb) 15(.5nb) 39(.64nb) 
    # 23(.6nb) 38(.58nb) 49(.61nb) 33(.59nb) 74(.67nb)   42(.89nb) 11(.53nb) 78(.40nb) 28(.67) 16(.60) 37(.57) 20(.55) 47(.58) 36(.66) 52 19 31 29  7 24
    # 50 22 84 83 87 75  9 25 80 26 21 82 41 97 91 89 27 40 43 18 88 48 96 72 90  
    # 5 94 44 30 34 93 32 85 14 92 51 95
    #campo_in <- "administração e negócio"
    #campo_in <- "saúde"
    #campo_in <- "artes e design"
    #campo_in <- "ciências exatas e informática"
    #campo_in <- "ciências humanas e sociais",
    #campo_in <- "comunicação e informação",
    #campo_in <- "engenharia",
    #campo_in <- "meio ambiente e ciências agrárias",

    df_train_in <-
        df_train_in %>%
        mutate(target = ifelse(target == campo_in,"T","F"))
    
    trainClass <- as.factor(df_train_in[,"target"]) # transformando em vetor de fatores de target
    trainClass <- factor(trainClass, levels = c("T","F")) # ordenando levels para "T" ser o primeiro
    trainDescr <- df_train_in[,c(1, 3:9)] 
    
    df_test_in <-
        df_test_in %>%
        mutate(target = ifelse(target == campo_in,"T","F"))
    testClass <- as.factor(df_test_in[,"target"]) # transformando em vetor de fatores de target
    testClass <- factor(testClass, levels = c("T","F")) # ordenando levels para "T" ser o primeiro
    testDescr <- df_test_in[,c(1, 3:9)]
    #table(trainClass)
    #table(testClass)
    # REMOVENDO NEAR ZERO VARIANCE AND CORRELATIONS 
    # (FOR CORRELATION, NUMERIC FEATURES ONLY)
    ######################################################
    #trn_nzvar <- nearZeroVar(trainDescr, freqCut = 20, uniqueCut = 20)
    #tst_nzvar <- nearZeroVar(testDescr, freqCut = 20, uniqueCut = 20)
    # removendo colunas que não passaram no teste
    #if(length(trn_nzvar) != 0  || length(tst_nzvar) != 0) {
    #    trainDescr <- trainDescr[,-(trn_nzvar)]
    #    testDescr <- testDescr[,-(tst_nzvar)]
    #}

    # eliminando features com menor importância 
    ###################################################
    #trainTotal <- cbind(sexo = trainClass,trainDescr)
    #initial <- glm(target ~ ., data = cbind(target = trainClass,trainDescr[,-1]), family = "binomial")
    #aic_o <- stepAIC(initial, direction = "both", trace = FALSE)
    
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
    #logb_model <- train(trainDescr[,-1], trainClass, 
    #                    #nbagg = 50,
    #                    metric = "ROC",
    #                    preProcess=c("center", "scale"),
    #                    trControl=control,  
    #                    method="LogitBoost")
    nb_model <- train(trainDescr[,-1], trainClass, 
                      #nbagg = 50,
                      metric = "ROC",
                      preProcess=c("center", "scale"),
                      trControl=control,
                      na.action=na.omit,
                      method="nb")
    
    #models_o <- list(logb = logb_model, nb = nb_model)
    models_o <- list(logb = nb_model)
    
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        models_o,
        testX = testDescr[,-1],
        testY = testClass)
    
    # pegar subset somente com dados de teste para validar o modelo
    testProbs <- subset(
        probValues,
        dataType == "Test")
    #testProbs <- subset(testProbs, model == tipo_mod)
    
    #+++++++++++
    #tipo_mod = "nb"
    #nbProbs <- subset(testProbs, model == tipo_mod)
    #nb_cf <- confusionMatrix(nbProbs$pred, nbProbs$obs)
    #nb_pred <- prediction(nbProbs$F, nbProbs$obs)
    #roc.perf = performance(nb_pred, measure = "tpr", x.measure = "fpr")
    #    plot(roc.perf)
    #abline(a=0, b= 1)
    
    #+++++++++++
    
    
    
    
    
    
    
    # confusion matrix
    #cf_o <- confusionMatrix(probValues$pred, probValues$obs)
    #cf_o <- confusionMatrix(testProbs$pred, testProbs$obs)

    # making a prediction object
    #pred_o <- prediction(probValues$T, probValues$obs)
    #pred_o <- prediction(testProbs$T, testProbs$obs)
    #pred_o <- prediction(testProbs$T, testProbs$obs) # USEI FALSE POR TER ALTA ESPECIFICIDADE! POR ELIMINACAO
    
    # ROC curve
    #roc.perf_o = performance(pred_o, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf_o)
    #abline(a=0, b= 1)
    
    # RETORNAR AUC
    #roc.auc_o = performance(pred_o, measure = "auc")
    #auc <- roc.auc_o@y.values
    #(auc)
    # crio data frame com as probabilidades do preditor (já ordenado)
    #df_rank_o <- data.frame (my_pred = roc.perf_o@alpha.values[[1]])

    # retorna lista para validação do modelo
    #l_o <- list(models_o,aic_o,cf_o, roc.perf_o, roc.auc_o, pred_o)
    #l_o <- list(models_o,cf_o, roc.perf_o, roc.auc_o, pred_o)
    #return(l_o)
    return(testProbs)
    
    
    
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
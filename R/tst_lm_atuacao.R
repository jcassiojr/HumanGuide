# teste de predição por regressão linear simples
# a partir de my.scores.total com area de atuação (ver Analise-Preditiva-Acuracia-Atuacao.Rmr)
require("xlsx")
require("dplyr")
require("caret")
require("MASS")
require("ROCR")
source("./R/f_acentos.R") 
source("./R/f_le_raw_HG.R") 
source("./R/f_tidy_scores_HG_area.R") 
registerDoMC(8) # parallel processing
# OBTEM ASSINATURAS DE BASELINE PARA CAMPO PROFISSIONAIS
#########################################################

#df_raw_hg <- f_le_raw_HG() # lê toda a amostra de dados HG
df_raw_hg_nv <- read.xlsx2("./data/pp_humanguide_20160307-1349.xlsx",1)

# changing factor to numeric
unfactorize<-c(4:7,12:83)
df_raw_hg_nv[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df_raw_hg_nv[,x])))

# mudança de descrição para código no arquivo recebido
#df_raw_hg <-
#    df_raw_hg %>%
#    mutate(atua.na.area.1 = ifelse(atua.na.area.1 == "Gerência / Gestão / Administração",1,
#                            ifelse(atua.na.area.1 == "Finanças / Manutenção / Operacional",2,
#                            ifelse(atua.na.area.1 == "Contencioso / Compras",3,
#                            ifelse(atua.na.area.1 == "Contato / Vendas",4,
#                            ifelse(atua.na.area.1 == "Pesquisa  / Novos Produtos",5,
#                            ifelse(atua.na.area.1 == "Suporte / Atendimento",6,
#                            ifelse(atua.na.area.1 == "Educacional / Treinamento / RH / Saúde",7,
#                            ifelse(atua.na.area.1 == "Análise / Controle / Auditoria",8,
##                            ifelse(atua.na.area.1 == "Empreendedor / Autonomo",9,
#                            ifelse(atua.na.area.1 == "Marketing / Artes Visuais / Publicidade",10,
#                            atua.na.area.1))))))))))) %>%
#    rename(atua.em.1 = atua.na.area.1)

#df_tidy_hg <- f_tidy_scores_HG_area(df_raw_hg) # calcula os fatores de acordo com a pontuação
 
    # primeiro somente selecionando as colunas necessárias para cálculo de ranking
df_raw_hg_nv <-
    df_raw_hg_nv %>%
    #mutate(p48 = as.numeric(as.vector(p48)),
    #       hy98 = as.numeric(as.vector(hy98))) %>%
    select(nomerespondente, idade, formacao, atua.em.1,
           s11, h21, h31, hy41, e51, m61, m71, p81, e91,
           e12, e22, e32, s42, s52, s62, k72, h82, m92,
           h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
           k14, d24, k34, k44, k54, hy64, p74, s84, d94,
           p15, m25, s35, h45, d55, k65, hy75, m85, k95,
           hy16, p26, p36, m46, h56, p66, h76, k86, s96,
           d17, s27, d37, d47, p57, d67, e77, hy87, h97,
           m18, hy28, m38, p48, m58, h68, d78, d88, hy98)

# colocando NA como indefinido em formação e ocupação
#df_in <-
#    df_in %>%
#    mutate(profissao.na.area.de = ifelse(is.na(profissao.na.area.de), "INDEFINIDO", as.character(profissao.na.area.de)),
#           formacao.em = ifelse(is.na(formacao.em), "INDEFINIDO", as.character(formacao.em)))

# agora eliminando linhas com NAs
df_raw_hg_nv <- na.omit(df_raw_hg_nv) # listwise deletion of missing

# seleciona apenas as colunas necessárias
df_tidy_hg_nv <-
    df_raw_hg_nv %>%
    mutate(sensibility = h13 + h21 + h31 + h45 + h56 + h68 + h76 + h82 + h97,
           power = s11 + s27 + s35 + s42 + s52 + s62 + s73 + s84 + s96,
           quality = e12 + e22 + e32 + e43 + e51 + e63 + e77 + e83 + e91,
           exposure = hy16 + hy28 + hy33 + hy41 + hy53 + hy64 + hy75 + hy87 + hy98,
           structure = k14 + k23 + k34 + k44 + k54 + k65 + k72 + k86 + k95,
           imagination = p15 + p26 + p36 + p48 + p57 + p66 + p74 + p81 + p93,
           stability = d17 + d24 + d37 + d47 + d55 + d67 + d78 + d88 + d94,
           contacts = m18 + m25 + m38 + m46 + m58 + m61 + m71 + m85 + m92) %>%
    select(nomerespondente, idade, formacao, atua.em.1, sensibility, power, quality,
           exposure, structure, imagination, stability, contacts) 
# restaura fatores como numericos
#df_tidy_hg <-
#    df_tidy_hg %>%
#    mutate(atua.em.1 = as.numeric(as.vector(atua.em.1)),
#           sensibility = as.numeric(as.vector(sensibility)),
#           power = as.numeric(as.vector(power)),
##           quality = as.numeric(as.vector(quality)),
#           exposure = as.numeric(as.vector(exposure)),
#           structure = as.numeric(as.vector(structure)),
#           imagination = as.numeric(as.vector(imagination)),
#           stability = as.numeric(as.vector(stability)),
#           contacts = as.numeric(as.vector(contacts)))

# OBTENÇÃO DOS SCORES A PARTIR DE TODA A AMOSTRA
#-----------------------------------------------------------------------------------
# obtendo os scores previstos de acordo com a análise de componentes principais
pca1 = prcomp(df_tidy_hg_nv[,5:12], scale. = TRUE, center = TRUE)
# scores obtidos
scores.total <- as.data.frame(pca1$x)
my.scores.total <- as.data.frame(cbind(nomerespondente = df_tidy_hg_nv$nomerespondente, 
                                       idade = df_tidy_hg_nv$idade,
                                       formacao = df_tidy_hg_nv$formacao,
                                       atua.em.1 = df_tidy_hg_nv$atua.em.1,
                                       scores.total))

# eliminando PC8
my.scores.total <-
    my.scores.total %>%
    select (-PC8)


# AREA DE ATUACAO 1
##########################

# IMPORTANTE: no nosso caso, baixo R2 e baixo p-value mostra que os componentes são significantes para explicar a área de 
# atuação (baixo p-value), mas explicam pouco da variabilidade (baixo R2).
# Os resultados preditivos portanto serão baixos
# Tb, R2 grande não quer dizer que o modelo linear explique bem os dados (reta x curva suave de dados dá um bom R2!!)
# ABAIXO: NÃO É CORRETO, pois atuação não é escala crescente, usar classificação ao invés

#fit.atuacao <- lm(atua.em.1 ~ formacao + idade + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7,my.scores.total)
#summary(fit.atuacao)
#write.xlsx2(summary(fit.atuacao)[4], "./data/RegrLinearAreaAtuacao.xlsx")
# CLASSIFICACAO
# atua.em.1 = 1 mudar para T e os demais para F
my.scores.class <-
    my.scores.total %>%
    mutate(atua.em.1 = ifelse(atua.em.1 == 1,"T","F"))


#selecionando as features e target em arquivos diferentes
class <- as.factor(my.scores.class[,"atua.em.1"]) # transformando em vetor de fatores de target
class <- factor(class, levels = c("T","F")) # ordenando levels para "S" ser o primeiro
# prop.table(table(class))


# ABAIXO USANDO VALOR DEVIDO SEM AGRUPAR E USANDO SOMENTE FEATURES DO ARQUIVO AVON PARA PODER PREVER DEPOIS
#descr <- df_tidy_in[,c(3:9)] # Obs: depois trocar Valor Devido por Faixa para ver se melhora o modelo!!
descr <- my.scores.class[,c(5:11)] # uso com df_change

set.seed(101)
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
#trainDescr <- as.data.frame(trainDescr[,c(2,3)])
#testDescr <- as.data.frame(testDescr[,c(2,3)])
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

models_o <- list(nb = logb_model)

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
plot(roc.auc_o)
abline(a=0, b= 1)







# CORRELAÇÃO ENTRE OS COMPONENTES
###################################
# correlação entee os PCs pode mostrar que são independentes
# o que faz estes preditores bons candidatos a previsão de resposta
#require("corrplot", quietly = TRUE, warn.conflicts = FALSE)
my.cor <- cor(my.scores.total[,3:7])
#corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)
(my.cor)

# OBS. TENTAR USAR FORMAÇÃO tb como preditor para AREA DE ATUACAO




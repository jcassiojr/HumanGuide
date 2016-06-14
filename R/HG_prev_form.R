# teste de predição por regressão linear simples
# a partir de my.scores.total com area de atuação (ver Analise-Preditiva-Acuracia-Atuacao.Rmr)
require("xlsx")
require("caret")
require("MASS")
require("ROCR")
require("doMC")
require("dplyr")
source("./R/f_acentos.R") 
source("./R/f_train_model_HG_form.R") 
source("./R/f_assinatura_abril.R")
#source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
registerDoMC(8) # parallel processing
options(scipen = 999) # removendo notação científica das saídas
# OBTEM DADOS PARA TREINO DE ENERGIA SUSTENTAVEL
#########################################################

# lendo dados de rh99 com codigos ao invés de descrições
df_raw_hg_nv <- read.csv2("./data/new/rh99_20160425_2158.csv", encoding = "UTF-8", 
                     sep = "\t", header = TRUE)
df_raw_hg_nv <- f_acentos(df_raw_hg_nv)

# filtrando para tirar colunas deslocadas e removendo coluna X
df_raw_hg_form <-
    df_raw_hg_nv %>%
    filter(!grepl("df|sp",TIPOUSER)) %>%
    select(-X)

df_raw_hg_form <- 
    df_raw_hg_form %>%
    mutate(area.form = as.factor(ifelse(formacao.em %in% c("1", "3", "6", "8", "9", "75", "81", 
                                             "52", "96", "76", "12", "75"), "administração e negócio",
                       ifelse(formacao.em %in% c("4", "28", "42"), "artes e design",
                       ifelse(formacao.em %in% c("11", "27", "30", "38", "47", "76"), "ciências exatas e informática",
                       ifelse(formacao.em %in% c("10", "13", "14", "33", "35", "37",
                                              "15", "84", "49", "9", "74", "83"), "ciências humanas e sociais",
                       ifelse(formacao.em %in% c("88", "36", "46", "48", "52", "72"), "comunicação e informação",
                       ifelse(formacao.em %in% c("20", "89", "78", "21", "87", "23",
                                              "79", "24", "80", "25", "86", "26",
                                              "82", "39", "50", "17"), "engenharia",
                       ifelse(formacao.em %in% c("90", "7", "91", "19", "22", "85",
                                              "92", "34", "41", "95", "93"), "meio ambiente e ciências agrárias",
                       ifelse(formacao.em %in% c("5", "16", "18", "29", "31", "32",
                                              "40", "43", "44", "45", "51", "94"), "saúde",
                              NA
                              ))))))))))
    

# changing factor to numeric
unfactorize<-c(15:86)
df_raw_hg_form[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df_raw_hg_form[,x])))

# primeiro somente selecionando as colunas necessárias para cálculo de ranking
df_raw_hg_form <-
    df_raw_hg_form %>%
    select(nomerespondente, idade, formacao.em, area.form,
           s11, h21, h31, hy41, e51, m61, m71, p81, e91,
           e12, e22, e32, s42, s52, s62, k72, h82, m92,
           h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
           k14, d24, k34, k44, k54, hy64, p74, s84, d94,
           p15, m25, s35, h45, d55, k65, hy75, m85, k95,
           hy16, p26, p36, m46, h56, p66, h76, k86, s96,
           d17, s27, d37, d47, p57, d67, e77, hy87, h97,
           m18, hy28, m38, p48, m58, h68, d78, d88, hy98)

# seleciona apenas as colunas necessárias
df_tidy_hg_form <-
    df_raw_hg_form %>%
    mutate(sensibility = h13 + h21 + h31 + h45 + h56 + h68 + h76 + h82 + h97,
           power = s11 + s27 + s35 + s42 + s52 + s62 + s73 + s84 + s96,
           quality = e12 + e22 + e32 + e43 + e51 + e63 + e77 + e83 + e91,
           exposure = hy16 + hy28 + hy33 + hy41 + hy53 + hy64 + hy75 + hy87 + hy98,
           structure = k14 + k23 + k34 + k44 + k54 + k65 + k72 + k86 + k95,
           imagination = p15 + p26 + p36 + p48 + p57 + p66 + p74 + p81 + p93,
           stability = d17 + d24 + d37 + d47 + d55 + d67 + d78 + d88 + d94,
           contacts = m18 + m25 + m38 + m46 + m58 + m61 + m71 + m85 + m92) %>%
    select(nomerespondente, idade, formacao.em, area.form, sensibility, power, quality,
           exposure, structure, imagination, stability, contacts) 



# OBTENÇÃO DOS SCORES A PARTIR DE TODA A AMOSTRA
#-----------------------------------------------------------------------------------
# obtendo os scores previstos de acordo com a análise de componentes principais
pca1.train = prcomp(df_tidy_hg_form[,5:12], scale. = TRUE, center = TRUE)
# scores obtidos
scores.total <- as.data.frame(pca1.train$x)
my.scores.total <- as.data.frame(cbind(nomerespondente = df_tidy_hg_form$nomerespondente, 
                                       idade = df_tidy_hg_form$idade,
                                       formacao.em = df_tidy_hg_form$formacao,
                                       area.form = df_tidy_hg_form$area.form,
                                       scores.total))

# eliminando PC8
my.scores.total <-
    my.scores.total %>%
    select (-PC8)
# colocando formacao.em como target
my.scores.form <-
    my.scores.total %>%
    rename(target = formacao.em) %>%
    select(nomerespondente, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7)

class <- as.factor(my.scores.form[,"target"]) # transformando em vetor de fatores de target
inTrain <- createDataPartition(class, p = 3/4, list = FALSE)
descr <- my.scores.form[,c(1,3:9)] 

#set.seed(1)
#inTrain <- createDataPartition(class, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]

trainClass <- class[inTrain]
testClass  <- class[-inTrain]


my.test.atua <- cbind(target = testClass, testDescr)
# reordenando as colunas
my.test.atua <-
    my.test.atua %>%
    select(nomerespondente, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7)

my.train.atua <- cbind(target = trainClass, trainDescr)
my.train.atua <-
    my.train.atua %>%
    select(nomerespondente, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7)

# RODANDO UM POR UM
#l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 28)
#sprintf("28: auc: %.4f", l_mod[[4]]@y.values)
#l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 1)
#sprintf("1: auc: %.4f", l_mod[[4]]@y.values)    
l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 52)
sprintf("52: auc: %.4f", l_mod[[4]]@y.values)    


    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 16)
    sprintf("16: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 37)
    sprintf("37: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 20)
    sprintf("20: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 47)
    sprintf("47: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 36)
    sprintf("36: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 52)
    sprintf("52: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 19)
    sprintf("19: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 31)
    sprintf("31: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 29)
    sprintf("29: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 7)
    sprintf("7: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 24)
    sprintf("24: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 50)
    sprintf("50: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 22)
    sprintf("22: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 84)
    sprintf("84: auc: %.4f", l_mod[[4]]@y.values)
    l_mod <- f_train_model_HG_form(my.train.atua, my.test.atua, 83)
    sprintf("83: auc: %.4f", l_mod[[4]]@y.values)
#my.atua.target <- 8 # código da atuação a prever
my.pred <- list()
my.auc <- numeric()

# treina o modelo para cada uma das 10 areas de atuação, gerando dataframe com os dados
# retornando o modelo para ser usado em cada previsão

# salva vetor com nomes dos respondentes
v_nomes_test <- my.test.atua[,1]
#colocar loop para obter percentuais para todos os areas forms Abril
#area.cod.descr <- c("administração e negócio",
#                    "saúde",
#                    "artes e design",
#                    "ciências exatas e informática",
#                    "ciências humanas e sociais",
#                    "comunicação e informação",
#                    "engenharia",
#                    "meio ambiente e ciências agrárias")
df_cod.form <- my.scores.form %>% mutate(target = as.numeric(as.character(target))) # transformando target de factor em numerico
df_cod.form <- df_cod.form %>% distinct(target)
cods.form <- df_cod.form[,"target"]
for (i in 1:length(cods.form)) {
    l_models <- try(f_train_model_HG_form(my.train.atua, my.test.atua, cods.form[i]))
    if("try-error" %in% class(l_models)) {
        print(paste0("Error in ", as.character(cods.form[i])))
    } else {
        pred <-  l_models[[6]] # valor de cutoff calculado (best balance)
        auc <-  l_models[[5]] # valor de cutoff calculado (best balance)
        my.pred[i] <- as.data.frame(pred@predictions)
        my.auc[i] <- (auc@y.values)
    }
}

# imprime acurácia do modelo
for (i in 1:length(cods.form)) {
    print(1 - as.numeric(my.auc[i])) # uso o complemento pois no treino faço a predição usando o falso. Depois mudar!!!
}

#    plot(roc.perf)
#abline(a=0, b= 1)

# transformando a lista em dataframe
my.df_prev <- data.frame(matrix(unlist(my.pred), nrow=8, byrow=T),stringsAsFactors=FALSE)
my.df_prev.t <- as.data.frame(t(my.df_prev))
# restaura nomes dos respondentes no data.frame
my.df_prev.t$nomerespondente <- v_nomes_test
names(my.df_prev.t) <- c("administração.e.negócio",
                         "saúde",
                         "artes.e.design",
                         "ciências.exatas.e.informática",
                         "ciências.humanas.e.sociais",
                         "comunicação.e.informação",
                         "engenharia",
                         "meio.ambiente.e.ciências.agrárias","nomerespondente")
# considerando TRUE somente probabilidade > 50%
my.df_prev.final <- 
    my.df_prev.t %>%
    mutate(administração.e.negócio = ifelse(administração.e.negócio > .5,"T", "F"),
           saúde = ifelse(saúde > .5,"T", "F"),
           artes.e.design = ifelse(artes.e.design > .5,"T", "F"),
           ciências.exatas.e.informática = ifelse(ciências.exatas.e.informática > .5,"T", "F"),
           ciências.humanas.e.sociais = ifelse(ciências.humanas.e.sociais > .5,"T", "F"),
           comunicação.e.informação = ifelse(comunicação.e.informação > .5,"T", "F"),
           engenharia = ifelse(engenharia > .5,"T", "F"),
           meio.ambiente.e.ciências.agrárias = ifelse(meio.ambiente.e.ciências.agrárias > .5,"T", "F")
    )

# plotanto os percentuais para atuacoes de dado respondente escolhido dos dados de uso
#respondente <- sample_n(my.df_prev.t,1) # amostra aleatória
# alternativa: buscar na base d euso por nome
# FALTA: colocar na funcao abaixo: criar coluna T (<50%) e F(>50%). No plo pintar de vermelho F r verde T 
my.respondente <- "mariana morales furlan"
respondente <-
    my.df_prev.t %>%
    filter(grepl(my.respondente, my.df_prev.t$nomerespondente))
# chama funcao que gera plot de assinatura
my.pl_atuacao <- f_assinatura_abril(respondente)
(my.pl_atuacao)

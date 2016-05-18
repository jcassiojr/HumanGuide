# teste de predição por regressão linear simples
# a partir de my.scores.total com area de atuação (ver Analise-Preditiva-Acuracia-Atuacao.Rmr)
require("xlsx")
require("caret")
require("MASS")
require("ROCR")
require("doMC")
require("dplyr")
require("reshape2")
source("./R/f_acentos.R") 
source("./R/f_usa_model_HG_form.R") 
#source("./R/f_assinatura_abril.R")
#source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
registerDoMC(8) # parallel processing
options(scipen = 999) # removendo notação científica das saídas
# OBTEM DADOS PARA TREINO DE ENERGIA SUSTENTAVEL
#########################################################

#df_raw_hg_form <- read.csv2("./data/new/HG_export_1461708842-TALENT.csv", encoding = "UTF-8", 
#                         sep = "\t", header = TRUE)
# lendo somente os dados de ultimo envio
#df_raw_hg_form <- read.xlsx2("./data/pp_humanguide_20160307-1349.xlsx",1)
# lendo dados de rh99 com codigos ao invés de descrições
df_raw_hg_nv <- read.csv2("./data/new/rh99_20160425_2158.csv", encoding = "UTF-8", 
                     sep = "\t", header = TRUE)
df_raw_hg_nv <- f_acentos(df_raw_hg_nv)
# filtando somente AMBEV e removendo coluna X
##df_raw_hg_form <-
##    df_raw_hg_nv %>%
#    filter(TIPOUSER == "ambev") %>%
#    select(-X)
# filtrando para tirar colunas deslocadas e removendo coluna X
df_raw_hg_form <-
    df_raw_hg_nv %>%
    #filter(!grepl("ambev|func publico|df|sp",TIPOUSER)) %>%
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
    
#df_raw_hg <- f_le_raw_HG() # lê toda a amostra de dados HG

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

# removendo NAs (ensino médio, ensino fundamental e ensino técnico)
#df_tidy_hg_form <- na.omit(df_tidy_hg_form)


# OBTENÇÃO DOS SCORES A PARTIR DE TODA A AMOSTRA

# obs: FALTA: pegar prior de acordo com area.form nos dados de treino e teste!!!

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
# separando dados de uso
uso <-
    my.scores.form %>%
    filter(grepl("alex welter|giselle welter|laura welter|beatriz welter|jose cassio dos santos",nomerespondente))


#set.seed(1)
#inTrain <- createDataPartition(class, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]
usaDescr  <- uso[,c(1,3:9)]

trainClass <- class[inTrain]
testClass  <- class[-inTrain]
usaClass <- uso[,2]
# TESTE
my.test.atua <- cbind(target = testClass, testDescr) # teste
# reordenando as colunas
my.test.atua <-
    my.test.atua %>%
    select(nomerespondente, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7)

# USO
my.usa.atua <- cbind(target = usaClass, usaDescr) # uso
my.usa.atua <-
    my.usa.atua %>%
    select(nomerespondente, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7)

#TREINO
my.train.atua <- cbind(target = trainClass, trainDescr)
my.train.atua <-
    my.train.atua %>%
    select(nomerespondente, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7)

# RODANDO UM POR UM
    # 28
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 28)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.28 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.28 <- as.data.frame(temp.t[1,])
    
    # 86
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 86)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.86 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.86 <- as.data.frame(temp.t[1,])
    
    # 13
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 13)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.13 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.13 <- as.data.frame(temp.t[1,])
    
    # 76
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 76)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.76 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.76 <- as.data.frame(temp.t[1,])
    
    # 73
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 73)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.73 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.73 <- as.data.frame(temp.t[1,])
    
    # 71
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 71)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.71 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.71 <- as.data.frame(temp.t[1,])
    
    # 1
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 1)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.1 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.1 <- as.data.frame(temp.t[1,])
    
    # 70
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 70)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.70 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.70 <- as.data.frame(temp.t[1,])
    
    # 46
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 46)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.46 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.46 <- as.data.frame(temp.t[1,])
    # 3
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 3)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.3 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.3 <- as.data.frame(temp.t[1,])
    # 12
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 12)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.12 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.12 <- as.data.frame(temp.t[1,])
    # 45
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 45)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.45 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.45 <- as.data.frame(temp.t[1,])
    
    # 79
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 79)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.79 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.79 <- as.data.frame(temp.t[1,])
    # 35
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 35)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.35 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.35 <- as.data.frame(temp.t[1,])
    # 81
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 81)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.81 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.81 <- as.data.frame(temp.t[1,])
    # 8
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 8)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.8 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.8 <- as.data.frame(temp.t[1,])
    # 4
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 4)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(4 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    4 <- as.data.frame(temp.t[1,])
    # 6
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 6)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.6 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.6 <- as.data.frame(temp.t[1,])
    # 17
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 17)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.17 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.17 <- as.data.frame(temp.t[1,])
    # 10
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 10)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.10 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.10 <- as.data.frame(temp.t[1,])
    # 15
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 15)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.15 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.15 <- as.data.frame(temp.t[1,])
    # 39
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 39)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.39 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.39 <- as.data.frame(temp.t[1,])
    # 23
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 23)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.23 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.23 <- as.data.frame(temp.t[1,])
    # 38
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 38)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.38 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.38 <- as.data.frame(temp.t[1,])
    # 49
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 49)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.49 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.49 <- as.data.frame(temp.t[1,])
    # 33
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 33)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.33 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.33 <- as.data.frame(temp.t[1,])
    # 74
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 74)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.74 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.74 <- as.data.frame(temp.t[1,])
    # 42
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 42)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.42 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.42 <- as.data.frame(temp.t[1,])
    # 11
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 11)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.11 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.11 <- as.data.frame(temp.t[1,])
    # 78
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 78)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.78 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.78 <- as.data.frame(temp.t[1,])
    # 28
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 28)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.28 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.28 <- as.data.frame(temp.t[1,])
    # 16
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 16)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.16 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.16 <- as.data.frame(temp.t[1,])
    # 37
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 37)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.37 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.37 <- as.data.frame(temp.t[1,])
    # 20
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 20)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.20 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.20 <- as.data.frame(temp.t[1,])
    # 47
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 47)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.47 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.47 <- as.data.frame(temp.t[1,])
    # 36
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 36)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.36 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.36 <- as.data.frame(temp.t[1,])
    # 52 PAROU AQUI
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(52)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.52 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.52 <- as.data.frame(temp.t[1,])
    # 19
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 19)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.19 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.19 <- as.data.frame(temp.t[1,])
    # 31
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 31)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.31 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.31 <- as.data.frame(temp.t[1,])
    # 29
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 29)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.29 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.29 <- as.data.frame(temp.t[1,])
    # 7
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 7)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.7 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.7 <- as.data.frame(temp.t[1,])
    # 24
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 24)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.24 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.24 <- as.data.frame(temp.t[1,])
    # 50
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 50)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.50 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.50 <- as.data.frame(temp.t[1,])
    # 22
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 22)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.22 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.22 <- as.data.frame(temp.t[1,])
    # 84
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 84)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.84 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.84 <- as.data.frame(temp.t[1,])
    # 83
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 83)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.83 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.83 <- as.data.frame(temp.t[1,])
    # 87
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 87)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.87 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.87 <- as.data.frame(temp.t[1,])
    # 75
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 75)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.75 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.75 <- as.data.frame(temp.t[1,])
    # 9
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 9)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.9 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.9 <- as.data.frame(temp.t[1,])
    # 25
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 25)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.25 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.25 <- as.data.frame(temp.t[1,])
    # 80
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 80)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.80 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.80 <- as.data.frame(temp.t[1,])
    # 26
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 26)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.26 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.26 <- as.data.frame(temp.t[1,])
    # 21
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 21)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.21 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.21 <- as.data.frame(temp.t[1,])
    # 82
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 82)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.82 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.82 <- as.data.frame(temp.t[1,])
    # 41
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 41)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.41 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.41 <- as.data.frame(temp.t[1,])
    # 97
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 97)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.97 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.97 <- as.data.frame(temp.t[1,])
    # 91
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 91)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.91 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.91 <- as.data.frame(temp.t[1,])
    # 89
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 89)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.89 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.89 <- as.data.frame(temp.t[1,])
    # 27
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 27)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.27 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.27 <- as.data.frame(temp.t[1,])
    # 40
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 40)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.40 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.40 <- as.data.frame(temp.t[1,])
    # 43
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 43)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.43 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.43 <- as.data.frame(temp.t[1,])
    # 18
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 18)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.18 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.18 <- as.data.frame(temp.t[1,])
    # 88
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 88)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.88 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.88 <- as.data.frame(temp.t[1,])
    # 48
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 48)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.48 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.48 <- as.data.frame(temp.t[1,])
    # 96
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 96)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.96 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.96 <- as.data.frame(temp.t[1,])
    # 72
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 72)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.72 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.72 <- as.data.frame(temp.t[1,])
    # 90
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 90)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.90 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.90 <- as.data.frame(temp.t[1,])
    # 5
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 5)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.5 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.5 <- as.data.frame(temp.t[1,])
    # 94
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 94)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.94 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.94 <- as.data.frame(temp.t[1,])
    # 44
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 44)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.44 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.44 <- as.data.frame(temp.t[1,])
    # 30
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 30)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.30 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.30 <- as.data.frame(temp.t[1,])
    # 34
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 34)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.34 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.34 <- as.data.frame(temp.t[1,])
    # 93
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 93)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.93 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.93 <- as.data.frame(temp.t[1,])
    # 32
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 32)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.32 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.32 <- as.data.frame(temp.t[1,])
    # 85
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 85)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.85 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.85 <- as.data.frame(temp.t[1,])
    # 14
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 14)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.14 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.14 <- as.data.frame(temp.t[1,])
    # 92
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 92)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.92 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.92 <- as.data.frame(temp.t[1,])
    # 51
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 51)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.51 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.51 <- as.data.frame(temp.t[1,])
    # 95
    v_nomes_uso <- my.usa.atua[,1]
    prob <- f_usa_model_HG_form(my.train.atua, my.usa.atua, 95)
    prob$nomerespondente <- v_nomes_uso
    temp <- prob %>% rename(form.95 = T)
    temp.t <- as.data.frame(t(temp))
    temp.t <- temp.t %>% add_rownames("VALUE")
    my.nomerespondente <- as.data.frame(temp.t[8,])
    form.95 <- as.data.frame(temp.t[1,])
    
    prev.forms <- rbind(my.nomerespondente, form.95, form.51, form.92, form.14, form.85, form.32, form.93, form.34, 
                        form.30, form.44, form.94, form.5, form.90, form.72, form.96, form.48, form.88, form.18,
                        form.43, form.40, form.27, form.89, form.91, form.97,form.41, form.82, form.21, form.26,
                        form.80, form.25, form.9, form.75, form.87, form.83, form.84, form.22, form.50, form.24,
                        form.7, form.29, form.31, form.19, form.52, form.36, form.47, form.20, form.37, form.16,
                        form.28, form.78, form.11, form.42, form.74, form.33, form.49, form.38, form.23, form.39,
                        form.15, form.10, form.17, form.6, form.8, form.81, form.35, form.79, form.45, form.12,
                        form.3, form.46,form.70, form.1, form.71, form.73, form.76, form.13, form.86)
    
    x <- rbind(my.nomerespondente, form.1, form.6, form.8, form.10, form.11, form.12, form.13, form.15, form.16, form.17, 
                        form.42, form.45, form.46, form.47, form.49, form.70, form.71, form.73, form.74, form.76,
                        form.78, form.79, form.81, form.86)
    x %>% write.csv("./data/prev_form-V1.csv")
    # ler formaco_ref.csv e merge com dataframe acima

    
    

# teste de predição por regressão linear simples
# a partir de my.scores.total com area de atuação (ver Analise-Preditiva-Acuracia-Atuacao.Rmr)

# mudar para treinar o modelo com dados Sustentavel, mas testar com dados Kroton

require("xlsx")
require("dplyr")
require("caret")
require("MASS")
require("ROCR")
require("doMC")
source("./R/f_acentos.R") 
source("./R/f_train_model_HG_nv.R") 
#source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
registerDoMC(8) # parallel processing
# lendo somente os dados de EnergiaSustantavel
#df_raw_hg_nv <- read.xlsx2("./data/pp_humanguide_20160307-1349.xlsx",1)
# lendo todos os dados
#df_raw_hg_nv <- f_le_raw_HG() # lê toda a amostra de dados HG
#df_raw_hg_nv <- read.csv2("./data/goe_20151121_0154.csv", encoding = "UTF-8", 
#                    sep = "\t", header = TRUE) # le somente goe
# OBS: KROTON ABAIXO NAO TEM FORMACAO< PROFISSAO NEM AREAS DE ATUACAO!!!!
#df_raw_hg_nv <- read.csv2("./data/kroton_20151121_0125.csv", encoding = "UTF-8", 
#                       sep = "\t", header = TRUE) # KROTON

# eliminando linhas erradas, com TIPOUSER = "SP" ou "DF"
#df_raw_hg_nv <- read.csv2("./data/rh99_20151121_0002.csv", encoding = "UTF-8", 
#                     sep = "\t", header = TRUE)
# elimina linhas com dados que estavam na coluna errada
#df_raw_hg_nv <-
#    df_raw_hg_nv %>%
#    filter(TIPOUSER != "SP" & TIPOUSER != "DF")

#names(df_raw_hg_nv) <- gsub("ç","c",names(df_raw_hg_nv))
#names(df_raw_hg_nv) <- gsub("ã","a",names(df_raw_hg_nv))
#names(df_raw_hg_nv) <- gsub("õ","o",names(df_raw_hg_nv))
#names(df_raw_hg_nv) <- gsub("á","a",names(df_raw_hg_nv))
# changing factor to numeric
#unfactorize<-c(4:7,12:83)
#df_raw_hg_nv[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df_raw_hg_nv[,x])))

    # primeiro somente selecionando as colunas necessárias para cálculo de ranking
df_raw_hg_nv <-
    df_raw_hg_nv %>%
    select(nomerespondente, idade, formacao.em, profissao.na.area.de, atua.na.area.1,
           s11, h21, h31, hy41, e51, m61, m71, p81, e91,
           e12, e22, e32, s42, s52, s62, k72, h82, m92,
           h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
           k14, d24, k34, k44, k54, hy64, p74, s84, d94,
           p15, m25, s35, h45, d55, k65, hy75, m85, k95,
           hy16, p26, p36, m46, h56, p66, h76, k86, s96,
           d17, s27, d37, d47, p57, d67, e77, hy87, h97,
           m18, hy28, m38, p48, m58, h68, d78, d88, hy98)

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
    select(nomerespondente, idade, formacao.em, atua.na.area.1, sensibility, power, quality,
           exposure, structure, imagination, stability, contacts) 

# muda nome da area para codigo
df_tidy_hg_nv <-
    df_tidy_hg_nv %>%
    mutate(atua.na.area.1 = ifelse(atua.na.area.1  == "Gerência / Gestão / Administração",1,
                            ifelse(atua.na.area.1  == "Finanças / Manutenção / Operacional",2,
                            ifelse(atua.na.area.1  == "Contencioso / Compras", 3,
                            ifelse(atua.na.area.1 == "Contato / Vendas", 4,
                            ifelse(atua.na.area.1 == "Pesquisa  / Novos Produtos", 5,
                            ifelse(atua.na.area.1 == "Suporte / Atendimento", 6,
                            ifelse(atua.na.area.1 == "Educacional / Treinamento / RH / Saúde", 7,
                            ifelse(atua.na.area.1  == "Análise / Controle / Auditoria", 8,
                            ifelse(atua.na.area.1 == "Empreendedor / Autonomo", 9,
                            10))))))))))
# OBTENÇÃO DOS SCORES A PARTIR DE TODA A AMOSTRA
#-----------------------------------------------------------------------------------
# obtendo os scores previstos de acordo com a análise de componentes principais
pca1 = prcomp(df_tidy_hg_nv[,5:12], scale. = TRUE, center = TRUE)
# scores obtidos
scores.total <- as.data.frame(pca1$x)
my.scores.total <- as.data.frame(cbind(nomerespondente = df_tidy_hg_nv$nomerespondente, 
                                       idade = df_tidy_hg_nv$idade,
                                       formacao = df_tidy_hg_nv$formacao.em,
                                       atua.em.1 = df_tidy_hg_nv$atua.na.area.1,
                                       scores.total))

# eliminando PC8
my.scores.total <-
    my.scores.total %>%
    select (-PC8)

my.atua.class <-
    my.scores.total %>%
    rename(target = atua.em.1) %>%
    select(nomerespondente, target, PC1, PC2, PC3, PC4, PC5, PC6, PC7)


# CORRELAÇÃO ENTRE OS COMPONENTES
###################################
# correlação entee os PCs pode mostrar que são independentes
# o que faz estes preditores bons candidatos a previsão de resposta
#require("corrplot", quietly = TRUE, warn.conflicts = FALSE)
#my.cor <- cor(my.scores.total[,3:7])
#corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)
#(my.cor)

# OBS. TENTAR USAR FORMAÇÃO tb como preditor para AREA DE ATUACAO

# AREA DE ATUACAO 1 - CLASSIFICAÇÃO
##########################
my.atua.target <- 2 # código da atuação a prever
l_models <- f_train_model_HG_nv(my.atua.class, my.atua.target)

models <-  l_models[[1]] # modelo trans de caret
aic <-  l_models[[2]] # Valor de AIC do modelo
cf <-  l_models[[3]] # objeto confusion matrix
roc.perf <-  l_models[[4]] # objeto performance de caret
roc.auc <-  l_models[[5]] # valor de AUC de curva ROC
pred <-  l_models[[6]] # valor de cutoff calculado (best balance)
#df_rank <-  l_models[[7]] # dataframe com pobabilidades de teste rankeadas

resampleHist(models$logb)
# objeto confusion Matrix (devo diminuir false positive)
print(cf$table)
print(cf$byClass)
print(cf$overall)
# Plot roc. objects (para cada modelo)
plot(roc.perf)
abline(a=0, b= 1)
# lift plot
roc.perf.lift = performance(pred, measure = "lift", x.measure = "rpp")
plot(roc.perf.lift)

# prints
sprintf(paste0("AIC Área de Atuação ", my.atua.target,": %.2f"),aic$aic)
sprintf(paste0("Área de Atuação ", my.atua.target," - %s : %.4f"),roc.auc@y.name,roc.auc@y.values)

# AREA DE ATUACAO 2 - CLASSIFICAÇÃO
##########################
my.atua.target <- 10 # código da atuação a prever
l_models <- f_train_model_HG_nv(my.atua.class, my.atua.target)

models <-  l_models[[1]] # modelo trans de caret
aic <-  l_models[[2]] # Valor de AIC do modelo
cf <-  l_models[[3]] # objeto confusion matrix
roc.perf <-  l_models[[4]] # objeto performance de caret
roc.auc <-  l_models[[5]] # valor de AUC de curva ROC
pred <-  l_models[[6]] # valor de cutoff calculado (best balance)
#df_rank <-  l_models[[7]] # dataframe com pobabilidades de teste rankeadas

resampleHist(models$logb)
# objeto confusion Matrix (devo diminuir false positive)
print(cf$table)
print(cf$byClass)
print(cf$overall)
# Plot roc. objects (para cada modelo)
plot(roc.perf)
abline(a=0, b= 1)
# lift plot
roc.perf.lift = performance(pred, measure = "lift", x.measure = "rpp")
plot(roc.perf.lift)

# prints
sprintf(paste0("AIC Área de Atuação ", my.atua.target,": %.2f"),aic$aic)
sprintf(paste0("Área de Atuação ", my.atua.target," - %s : %.4f"),roc.auc@y.name,roc.auc@y.values)

#tree.2 <- rpart(target ~ PC1 + PC2,my.atua.class[,c(2:9)])			# A more reasonable tree
#prp(tree.2)                                     # A fast plot													
#fancyRpartPlot(tree.2)				# A fancy plot from rattle

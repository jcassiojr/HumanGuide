# exemplo de feature selection using Infornmation Gain
#---------------------------------------------------
# com calculo de entropia do parent
# três exemplos de seleçao de features (gill-color, spore-print-color e odor)
# calculando entropia para cada um 
# plotando entropia usando porporcao em relação aos dados total como peso
# e calculando information gain para cad afeature para ver qual é melhor
# neste exemplo, se tivessemos que escolher uma unica feature, seria ODOR
# por apresentar o maior IG

source("./R/f_ig_num.R")
source("./R/f_ig_cat.R")
source("./R/f_entropy.R")
##############################################################
#
# FEATURE SELECTION PARA FEATURE CATEGÓRICA
#
##############################################################
#----------------------------------
# carregando dados
#----------------------------------
df_mush <- read.csv("./data/agaricus-lepiota.data.txt", header = FALSE)
# carregando nomes dos dados
df_nmmush <- read.csv("./data/agaricus-lepiota.nomes.csv", header = FALSE)
# colocando nomes nos títulos das colunas
# transformando dataframe em vetor de nomes
v_nmmush <- df_nmmush[['V1']]
# função para retornar o dataframe com os nomes
get.names<- function(x,y) {
    names(x)<-y
    x
}
# dataframe com os nomes das colunas
df_mush <- get.names(df_mush,v_nmmush)
# trocando inderscore por ponto nos nomeses das colunas
#gsub("-",".",names(df_mush),)
df_mush <- get.names(df_mush,gsub("-",".",names(df_mush),))
# como está distribuída a variável alvo no parent (%)
#x <- 'edible' # exemplo de uso de parametro em dplyr
#df_mush %>%
#    group_by_(x) %>%
#    summarize(target = n())
#----------------------
# entropia do parent
#---------------------
# chamada da função de cálculo de entropia do parent
Ep <- f_entropy(df_mush[,1])

# cálculo de information gain para feature
#----------------------------------------
v_nfeat <- c("gill.color","spore.print.color","odor")

IG <- vector()
IG[1] <- f_ig_cat(df_mush, "gill.color", "edible")
# cálculo de information gain para feature: spore.print.color
IG[2] <- f_ig_cat(df_mush, "spore.print.color", "edible")
# cálculo de information gain para feature: odor
IG[3] <- f_ig_cat(df_mush, "odor", "edible")
# plota IG
df_ig <- data.frame(v_nfeat,IG)
# using ggplot
library(ggplot2)
ggplot(data=df_ig, 
       aes(x = reorder(df_ig[,1], IG), y=IG)) +
    geom_bar(stat="identity",color = "black",
             fill="#DD8888", 
             width=.8) +
    ggtitle("Information Gain for 3 features")

##############################################################
#
# FEATURE SELECTION PARA FEATURE NUMÉRICA (iris dataset)
#
##############################################################
# chama função que calcula IG (bins = 4 por default)
IG <- vector()
# cálculo de information gain para feature: Sepal.Length
IG[1] <- f_ig_num(iris, "Sepal.Length", "Species")
# cálculo de information gain para feature: Sepal.Width
IG[2] <- f_ig_num(iris, "Sepal.Width", "Species")
# cálculo de information gain para feature: Petal.Length
IG[3] <- f_ig_num(iris, "Petall.Length", "Species")
# cálculo de information gain para feature: Petal.Width
IG[4] <- f_ig_num(iris, "Petal.Width", "Species")
# plota IG
df_ig <- data.frame(v_nfeat,IG)
# using ggplot
IG

# multivariate supervised segmentation using classification tree (tree induction) 
# segmentando os dados por múltiplos atributos
library(dplyr)
#
# ---- PASSO 1 Carregar dados de training
#
# carregando dados de cogumelos
df_mush <- read.csv("./data/agaricus-lepiota.data.txt", header = FALSE)
# crregando nomes dos dados
df_nmmush <- read.csv("./data/agaricus-lepiota.nomes.csv", header = FALSE)
# colocando nomes nos títulos das colunas
# transformando dataframe em vetor de nomes
v_nmmush <- df_nmmush[['V1']]
#class(v_nmmush)

# função para retornar o dataframe com os nomes
get.names<- function(x,y) {
    names(x)<-y
    x
}
# dataframe com os nomes das colunas
df_mush <- get.names(df_mush,v_nmmush)

# montando a decision tree
# parametros: target variable, dataset

#Train the decision tree
#treemodel <- rpart(edible~., data=df_mush)
#plot(treemodel,compress = TRUE)
#text(treemodel, use.n=T)
#Predict using the decision tree
#prediction <- predict(treemodel, newdata=iristest, type=’class’) > #Use contingency table to see how accurate it is
#    table(prediction, iristest$Species)
#names(nnet_iristrain)[8] <- ‘virginica’

# Plotting Classification Trees with the plot.rpart and rattle pckages

library(rpart)				    # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script

#data(segmentationData)				# Get some data
#data <- segmentationData[,-c(1,2)]
#
# ---- PASSO 2 aplicar o modelo de regressão
#
# Make big tree
form <- as.formula(edible ~ .)

#-------------------------------------------------------------------
#
# ---- PASSO 3 gerar o plot da árvore de decisão
#
tree.2 <- rpart(form,df_mush)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle
#
# ---- PASSO 4 aplicar a correção de Laplace
# Laplace correction manually
# olhando para o gráfico e gerando dataframe manualmente

node <- c('N0','N2','N3', 'N4', 'N5')
perc_e <- c(.52, .97, .0, .99, .0)
perc_p <- c(.48, .3, 1.0, .01, 1.0)
df_laplace_corr <- data.frame(node, perc_e, perc_p)

# inserindo coluna com correção de Laplace nas probabilidades
# obtendo o total de registros
tt_mush <- nrow(df_mush)
# aplicando a correção de Laplace
df_laplace_corr <-
    df_laplace_corr %>%
    mutate (laplace_corr_e = ((perc_e * tt_mush) + 1) / (tt_mush + 2),
            laplace_corr_p = ((perc_p * tt_mush) + 1) / (tt_mush + 2))

# exemplo de interpretação
# se a amostra tem odor = n e spore-print-color = b, 
# então tem 99% de chance de ser comestível (edible = e)



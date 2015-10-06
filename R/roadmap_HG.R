#---------------------------------------
# Roadmap de Análise Human Guide
#---------------------------------------

# ESTABELECER CLARAMENTO O PROBLEMA D ENEGÓCIO A SOLUCIONAR
#----------------------------------------------------------

# PREPARACAO DOS DADOS
#----------------------------------------------------------

# 1. obter os dados de candidatos com o feature vector de suas pontuações 
#    no teste HG. 
# 2. associar ao dataset a target variable (turnover: sim/não) da base de funcionários
#    da empresa
# 3. separar em train data e houdout data
# 4. obter a distribuição real de turnover na empresa (prior class)

# ANÁLISE EXPLORATÓRIA
#---------------------------------------

# 1. análise de cluster entre target variable e fetaures, usando shiny
# executar ui.R ou server.R, após mudar f_cluster_df para retornar o data.frame
# a ser analisado (obs. variáveis devem ser numéricas)

# Prepare Data
data("filmData")
mydata <- filmData[,-4]
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

# hierarquical clustering

# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

# model based clustering
# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model

# plot cluster solutions

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)

# IDENTIFICAR ATRIBUTOS COM MAIOR INFORMATION GAIN
#-------------------------------------------------

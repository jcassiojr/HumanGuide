#---------------------------------------
# Roadmap de Análise Human Guide
#---------------------------------------

# ESTABELECER CLARAMENTO O PROBLEMA DE NEGÓCIO A SOLUCIONAR
#----------------------------------------------------------
# PROBLEMA:
# ABORDAGEM:

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

data(iris)
weights <- information.gain(Species~., iris)
print(weights)
subset <- cutoff.k(weights, 1)
f <- as.simple.formula(subset, "Species")
print(f)
subset <- cutoff.k.percent(weights, 0.75)
f <- as.simple.formula(subset, "Species")
print(f)
# esta é a melhor opção (para pegar os atributos que mais se diferenciam dos demais)
subset <- cutoff.biggest.diff(weights)
f <- as.simple.formula(subset, "Species")
print(f)
#print(f)
#weights <- gain.ratio(Species~., iris)
#print(weights)
#subset <- cutoff.k(weights, 2)
#f <- as.simple.formula(subset, "Species")
#print(f)
#weights <- symmetrical.uncertainty(Species~., iris)
#print(weights)
#subset <- cutoff.biggest.diff(weights)
#f <- as.simple.formula(subset, "Species")
#print(f)

# outra possibilidade
#library(mlbench)
#data(HouseVotes84)
# selecionafeature com mais information gain
#weights <- chi.squared(Class~., HouseVotes84)
#print(weights)
# seleciona os 5 maiores
#subset <- cutoff.k(weights, 5)
# mota a fórmula com os 5 maiores
#f <- as.simple.formula(subset, "Class")
#print(f)

#subset <- consistency(Class~., HouseVotes84)
#f <- as.simple.formula(subset, "Class")
#print(f)

# AVALIANDO USANDO CROSS VALIDATION (MANUALMENTE)
#---------------------------------------
#library(rpart)
#data(iris)
#evaluator <- function(subset) {
    #k-fold cross validation
#    k <- 5
    # gera vetor randômico para seleção dos dados
#    splits <- runif(nrow(iris))
    # obtém error rate para cada feature
#    results = sapply(1:k, function(i) {
#        test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
#        train.idx <- !test.idx
#        test <- iris[test.idx, , drop=FALSE]
#        train <- iris[train.idx, , drop=FALSE]
#        tree <- rpart(as.simple.formula(subset, "Species"), train)
#        error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
#        return(1 - error.rate)
#    })
#    print(subset)
#    print(mean(results))
#    return(mean(results))
#}
#subset <- exhaustive.search(names(iris)[-5], evaluator)
#f <- as.simple.formula(subset, "Species")
#print(f)

# IDENTIFICAR ATRIBUTOS (ALTERNATIVA USANDO SFS e MEDINDO AIC)
#-------------------------------------------------
#library(dplyr)
#df_iristrain <-
#    iris %>%
#    mutate(newcol = ifelse(Species == 'setosa',1,
#                           ifelse(Species == 'virginica',2,
#                                  ifelse(Species == 'versicolor',3,0))))

#step(glm(newcol ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, data=df_iristrain), direction = "forward")
# or
#mod <- glm(newcol ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, data=df_iristrain)

#AIC(mod)


# começar o ROADMAP A PARTIR DAQUI!!!! USANDO CARET


# MODELING WITH THE TRAINING DATA AND CROSS VALIDATION
# WITH DIFERENT MODELS
#-------------------------------------------------
require(caret)
# load the iris dataset
#data(iris)
# PREPARACAO DOS DADOS
#----------------------------------------------------------
# criando bases simuladas para training and testing (com 2 classes)
set.seed(2969)
imbal_train <- twoClassSim(10000, intercept = -20, linearVars = 20)
imbal_test  <- twoClassSim(10000, intercept = -20, linearVars = 20)
table(imbal_train$Class)

# define training control (cross validation, 10 folds)
#train_control <- trainControl(method="cv", number=10)
train_control <- trainControl(method = "repeatedcv", number=10, repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary
                     )
# usar names(getModelInfo()) para ver todas as possibilidades
# ver doc completa em http://topepo.github.io/caret/bytag.html

# REMOVENDO ATRIBUTOS PARA MELHORAR PERFORMANCE DO MODELO
#-------------------------------------------------
# Data can contain attributes that are highly correlated with each other. 
# Many methods perform better if highly correlated attributes are removed
# Generally, you want to remove attributes with an absolute correlation 
# of 0.75 or higher.

# load the library
library(mlbench)
# calculate correlation matrix
correlationMatrix <- cor(imbal_train[,1:25])
#correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# ranking features by importance
# The example below loads the Pima Indians Diabetes dataset and constructs an 
# Learning Vector Quantization (LVQ) model. The varImp is then used to estimate
# the variable importance, which is printed and plotted. 

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
#model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
model <- train(Class~., data=imbal_train, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# A popular automatic method for feature selection provided by the caret
# R package is called Recursive Feature Elimination or RFE.
# The example below provides an example of the RFE method on the Pima 
# Indians Diabetes dataset. A Random Forest algorithm is used on each 
# iteration to evaluate the model. The algorithm is configured to explore
# all possible subsets of the attributes. All 8 attributes are selected 
# in this example, although in the plot showing the accuracy of the 
# different attribute subset sizes, we can see that just 4 attributes 
# gives almost comparable results.
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
results <- rfe(imbal_train[,1:25], imbal_train[,26], sizes=c(1:25), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))






# CRIANDO DIVERSOS TIPOS DE MODELOS
#----------------------------------------------------------
# CLASSIFICATION MODELS

# TREE BAG MODEL
#---------------
model <- train(Class~., data=imbal_train, 
               nbagg = 50,
               metric = "ROC",
               trControl=train_control, method="treebag")
                
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
#print(predictions)
# summarize results
tb_cf <- confusionMatrix(predictions, imbal_train$Class)
print (tb_cf$table) # confusion matrix as a table
print (tb_cf$byClass) # estatístics as a matrix
print (tb_cf$overall) # acuracy as a numeric vector

# CONDITIONAL INFERENCE TREE MODEL
ctree2_model <- train(Class~., data=imbal_train, 
                    #nbagg = 50,
                    metric = "ROC",
                    trControl=train_control, method="ctree2")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
ctree2_predictions <- predict(ctree2_model, imbal_train[, -ncol(imbal_train)])
# summarize results
ctree2_cf <- confusionMatrix(ctree2_predictions, imbal_train$Class)
print (ctree2_cf$table) # confusion matrix as a table
print (ctree2_cf$byClass) # estatístics as a matrix
print (ctree2_cf$overall) # acuracy as a numeric vector


# K NEIGHBORS MODEL
knn_model <- train(Class~., data=imbal_train, 
               k = 5,
               metric = "ROC",
               trControl=train_control, method="knn")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
knn_predictions <- predict(knn_model, imbal_train[, -ncol(imbal_train)])
# summarize results
knn_cf <- confusionMatrix(knn_predictions, imbal_train$Class)
print (knn_cf$table) # confusion matrix as a table
print (knn_cf$byClass) # estatístics as a matrix
print (knn_cf$overall) # acuracy as a numeric vector

# BAYESIAN GENERALIZING LINEAR MODEL
bglm_model <- train(Class~., data=imbal_train, 
                   #nbagg = 50,
                   metric = "ROC",
                   trControl=train_control, method="bayesglm")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
bglm_predictions <- predict(bglm_model, imbal_train[, -ncol(imbal_train)])
# summarize results
bglm_cf <- confusionMatrix(bglm_predictions, imbal_train$Class)
print (bglm_cf$table) # confusion matrix as a table
print (bglm_cf$byClass) # estatístics as a matrix
print (bglm_cf$overall) # acuracy as a numeric vector

# GENERALIZING LINEAR MODEL
glm_model <- train(Class~., data=imbal_train, 
                    #nbagg = 50,
                    metric = "ROC",
                    trControl=train_control, method="glm")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
glm_predictions <- predict(glm_model, imbal_train[, -ncol(imbal_train)])
# summarize results
glm_cf <- confusionMatrix(glm_predictions, imbal_train$Class)
print (glm_cf$table) # confusion matrix as a table
print (glm_cf$byClass) # estatístics as a matrix
print (glm_cf$overall) # acuracy as a numeric vector

# BOOSTED LOGISTIC REGRESSION MODEL
model <- train(Class~., data=imbal_train, 
                   #nbagg = 50,
                   metric = "ROC",
                   trControl=train_control, method="LogitBoost")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
# summarize results
logit_cf <- confusionMatrix(predictions, imbal_train$Class)
print (logit_cf$table) # confusion matrix as a table
print (logit_cf$byClass) # estatístics as a matrix
print (logit_cf$overall) # acuracy as a numeric vector

# SVM WITH FORWARD SELECTION MODEL
# PAREI AQUI
model <- train(Class~., data=imbal_train, 
               #nbagg = 50,
               metric = "ROC",
               trControl=train_control, method="svmRadialWeights")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
# summarize results
svm_cf <- confusionMatrix(predictions, imbal_train$Class)
print (svm_cf$table) # confusion matrix as a table
print (svm_cf$byClass) # estatístics as a matrix
print (svm_cf$overall) # acuracy as a numeric vector



# aqui prepara os dados pra trabalhar com REGRESSAO!! (TARGET NUMERICO)
# CLASSIFICATION MODELS

# LINEAR REGRESSION WITH FORWARD SELECTION MODEL
# testar com dados numericos somente!!!!
model <- train(Class~., data=imbal_train, 
               #nbagg = 50,
               metric = "ROC",
               trControl=train_control, method="lmStepAIC")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
# summarize results
logit_cf <- confusionMatrix(predictions, imbal_train$Class)
print (logit_cf$table) # confusion matrix as a table
print (logit_cf$byClass) # estatístics as a matrix
print (logit_cf$overall) # acuracy as a numeric vector

# AGRUPANDO OS MODELOS PARA ANALISAR a ROC Curve e definir o melhor
# obs. depois acrescentar o exemplo do livro, onde tem um budget
# fixo e precisa pegar número de dados que se encaixa no mesmo
# inserir também a lift curve aqui
#-------------------------------------------------

#  <COLOCAR AQUI SCRIPT DE exp_subsampling_technique.R>

#------------------------------------------------

# COMPARANDO OS MODELOS via ROC
# e SELECIONANDO O MELHOR Cut POINT do modelo escolhido
# Usando Expected value e pacote OptimalCutpoint para
# calcular melhor cutt point in ROC curve
# a partir da matriz de custo x benefício calcula o
# valor do parâmetro costs.ratio;
# CR = (Cfp - Ctn) / (Cfn - Ctp), valores obtidos
# da matriz de custoxbenefício para o problema
# de negócio.
# Calcular para todos os modelos e selecionar aquele que tem
# o maior valor, usando o cutpoint com maior custo x benefício
# Selecionado o modelo e o cutpoint, usa esgte modelo para
# predição




# USANDO O MODELO PARA PREVISÃO
# acrescentar aqui script para aplicar o modelo a dados reais que queremos
# prever a classe
# como fazer isso usando caret????
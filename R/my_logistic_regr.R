# logistic regression - binary classification using iris data
# incluindo coluna com valor binario para classificação
# trata a probabilidade de instância pertencer a uma classe 
# definida por algum target binário

# preparing training and testing data (using iris as an example)
# obtém os índices múltiplos de 5 do dataset
testidx <- which(1:length(iris[,1]) %% 5 == 0)
# cria dados para treino a partir dos índices que não são múltiplos de 5
iristrain <- iris[-testidx,]
# cria dados para teste a partir dos índices que são múltiplos de 5
iristest <- iris[testidx,]

# cria data.frame com uma coluna TRUE/FALSE para Setosa
newcol <- data.frame(isSetosa=(iristrain$Species == 'setosa')) 
# mescla esta coluna no dataframe de treino
traindata <- cbind(iristrain, newcol)
# mostra o inicio do dataset
head(traindata)
# cria fórmula de regressão (símbolo ~) para este trget variable
formula <- isSetosa ~ .
# PODERIA SER ASSIM TB formula <- isSetosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
# aplica o modelo logistico
logisticModel <- glm(formula, data=traindata, family='binomial') 
# Predict the probability for test data
prob <- predict(logisticModel, newdata=iristest, type= 'response') 
round(prob, 3)

######################################
# another example 
######################################
library("MASS")
data(menarche)
str(menarche)
summary(menarche)
plot(Menarche/Total ~ Age, data=menarche)
glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age,
             family=binomial(logit), data=menarche)

plot(Menarche/Total ~ Age, data=menarche)
lines(menarche$Age, glm.out$fitted, type="l", col="red")
title(main="Menarche Data with Fitted Logistic Regression Line")


# another yet
library (ISLR)
attach(Smarket)
?Smarket
summary(Smarket)
cor(Smarket[,-9])
pairs(Smarket[,-9])
# step 2 - split the data into training and test datasets
training <- (Year < 2005) # já entendeu a variable por causa do attach acima???
training_data <- Smarket[training,]
table (training)
testing <- !training
testing_data <- Smarket[testing,]
Direction_testing <- Direction[testing]

# Step 3 - fit a logistict regression model using our training data
# binomial porque traget é binaria
stock_model <- glm(Direction~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                   data = training_data, family = binomial )
summary(stock_model)

# Step 4: using the fitted model to do predictions for the test data
model_pred_probs <- predict(stock_model, testing_data, type = "response")
# mudando a probabilidade retornada para classe (Up or Down)
model_pred_directions <- rep("Down", 252)
model_pred_directions[model_pred_probs > 0.5] <- "Up"

# Step 5: create the confusion matrix and compute the misclassification rate
table(model_pred_directions, Direction_testing)
# media da misclassification (alta!!)
mean(model_pred_directions != Direction_testing)
# conclusão: jogar uma moeda tem o mesmo efeito de usr este modelo para previr market stock!!


#######################################
# book DS for business example
######################################
#----------------------------------
# carregando dados
#----------------------------------
df_wdbc <- read.csv("./data/wdbc.data.txt", header = FALSE)
# carregando nomes dos dados
df_nmwdbc <- read.csv("./data/wdbc.nomes.csv", header = FALSE)
# transformando dataframe em vetor de nomes
v_nmwdbc <- df_nmwdbc[['V1']]
# função para retornar o dataframe com os nomes
get.names<- function(x,y) {
    names(x)<-y
    x
}
# dataframe com os nomes das colunas
df_wdbc <- get.names(df_wdbc,v_nmwdbc)
# cria data.frame com uma coluna TRUE/FALSE para Setosa
newcol <- data.frame(isM=(df_wdbc$Diagnosis == 'M')) 
# mescla esta coluna no dataframe 
df_wdbc <- cbind(df_wdbc, newcol)

# cria dados para treino e teste
# obtém os índices múltiplos de 5 do dataset
testidx <- which(1:length(df_wdbc[,1]) %% 5 == 0)
# cria dados para treino a partir dos índices que não são múltiplos de 5
wdbctrain <- df_wdbc[-testidx,]
# cria dados para teste a partir dos índices que são múltiplos de 5
wdbctest <- df_wdbc[testidx,]
# tirando coluna ID e Diagnosis
wdbctrain <- wdbctrain[,-c(1,2)]

formula <- isM ~ .
logisticModel <- glm(formula, data=wdbctrain, family='binomial') 
# Predict the probability for model
prob <- predict(logisticModel, newdata=wdbctest, type= 'response') 
round(prob, 3)
# medindo a matriz de confusão (mostra boa performance pois a previsão a partir dos dados
# de teste está muito boa em relação aos dados reais presentes nos testes)
# mas como usar para prever uma instância??? 
table(round(prob),wdbctest$isM)
# qual a performance? 4 erros no data set inteiro de 113
performance <- 1-4/113

# gráfico de overffiting
# 1. rodar logistic regression para todas as variáveis
# 2. depois ir tirando as que apresentaram menos influência
# 3. para cada execução, obter a acurácia
# 4. repetir o procedimento acima para holdaut e training data
# 5. plotar o gráfico de fitting para holdout e treining data

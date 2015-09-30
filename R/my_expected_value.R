# expected value 
# usar para contratar o candidato desde que a probabilidade de not turnover é maior que X%
# precisa definir claramento o que é "not turnover" (funcionário que fica mais que 
# quantos meses?)

# ou para ver que funcionários novos devo tomar ação para manter, de acordo com HG test
# 0. criamos modelo que dá probabilidade de turnover de funcionário, baseado no 
#    resultado do HGTest
# 1. Pergunta: que funcionário que está a pouco tempo (menos do tempo de turnover escolhido)
#    vale a pena fazer trabalho de retenção?
# 2. obter a estimativa da probabilidade de turnover de funcionário, a partir do modelo
# 3. definir o valor de benefício se funcionário não sair
# 4. definir o valor do custo de funcionário sair
# 5. aplicando expected value obtemos qual a probabilidade de turnover mínima
#    onde vale a pena aplicarmos ação para manter funcionário

library(caret)
# load the iris dataset
data(iris)
# define training control (cross validation, 10 folds)
train_control <- trainControl(method="cv", number=10)
# train the model 
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# make predictions
predictions <- predict(model, iris[,1:4])
# summarize results
result <- confusionMatrix(predictions, iris$Species)
# transforming into probabilities
mt_result <- as.matrix(result)
mt_result_p <- mt_result/sum(mt_result)
# adding cost/benefit (apenas exemplo, deve ser criada de acordo com experts de negócio)
# 45, 46 e 47 são os benefícios dos acertos. Os demais são custos dos erros
mt_cost_benef = matrix( c(45, -12, -13, -14, 46, -15, -16, -17, 47), nrow=3, ncol=3) 
# obtendo a matriz de valor esperado
mt_expect_val <- mt_result_p * mt_cost_benef

############################################
# usando exemplo mais próximo com resultado do Brest C como se fosse modelo HG
############################################
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

#result <- confusionMatrix(predictions, iris$Species)

round(prob, 3)
# medindo a matriz de confusão (mostra boa performance pois a previsão a partir dos dados
# de teste está muito boa em relação aos dados reais presentes nos testes)
# mas como usar para prever uma instância??? 
tb_result <- table(round(prob),wdbctest$isM)
# transforming into probability matrix
tb_result_p <- tb_result/sum(tb_result)
# adding cost/benefit (apenas exemplo, deve ser criada de acordo com experts de negócio)
# para contratação de candidato
# objetivo: minimizar custos
#custos envolvidos: investimento no candidato (treinamento). Supondo que selecão
# só será feita seleção após candidato responder online o HGtest
# false positive: classificamos candidato como candidato a turnover e ele não sairia
#   custo: não foi para seleção (ex. redução de custo = 0). custo = 0
# false negative: classificamos candidato como NÃO candidato a turnover e ele sairia
#   custo: custo da seleção + custo do treinamento
#   (ex. redução de custo = 0). custo = 0 (CUIDADO PARA NÃO CONSIDERER -700 E DUPLICAR O VALOR)
# true positive: classificamos candidato como candidato a turnover e ele sairia
#   benefício: valor evitado para seleção e treinamento (redução de custo: R$ 200 + R$ 500) = 700
# true negative: classificamos candidato como NÃO candidato a turnover e ele NÃO sairia
#   benefício: não tivemos gasto desnecessário nem benefício (ex. redução de custo = 0) = 0

tb_cost_benef = matrix( c(700, 0, 0, 0), nrow=2, ncol=2)
# obtendo a matriz de valor esperado
tb_expect_val <- tb_result_p * tb_cost_benef
# expected profit (para usar comparativamente com confusion matrix de outros modelos)
Ep <- sum(tb_expect_val)
# usando prior class
# total de ocorrências (fonte: capitulo sobre expected value em DS for business book)
T <- sum(tb_result)
# total positive
P <- sum(tb_result[,1])
# class prior(P)
Pr_P <- P/T
N <- sum(tb_result[,2])
# class prior(N)
Pr_N <- N/T
# rates (true pos, true neg, etc)

tp_rate <- tb_result[1,1]/P
fp_rate <- tb_result[1,2]/N
fn_rate <- tb_result[2,1]/P
tn_rate <- tb_result[2,2]/N
tb_rate <- matrix( c(tp_rate, fn_rate, fp_rate, tn_rate), nrow=2, ncol=2)

# expected profit levand em conta o peso percentual de cada clsse (prior class)
Ep_prior <- Pr_P * (tb_expect_val[1,1]* tp_rate + tb_expect_val[2,1]* fn_rate) +
            Pr_N * (tb_expect_val[2,2]* tn_rate + tb_expect_val[1,2]* fp_rate)

# expected profit is $ 268.56
# quer dizer que se aplicamos o modelo em população de candidatos e contratamos
# aqueles que classificamos como não gerando turnover, temos uma redução de custo média de
# $ 268.56 por funcionário contratado
        
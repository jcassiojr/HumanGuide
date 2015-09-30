# explo de baselines
# para clasificação, um bom é majority classifier
# para regressão, temos o valor médio sobre a população (usualment média e mediana)
# decision Tree
# decision tree
library(rpart)

#fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
# maxdepth = 0 para criar decision stump
fit_stump <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, maxdepth = 1)
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(fit_stump)
text(fit_stump, use.n = TRUE)
#fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
#              parms = list(prior = c(.65,.35), split = "information"))
#fit3 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
#              control = rpart.control(cp = 0.05))

#plot(fit)
#text(fit, use.n = TRUE)
#plot(fit2)
#text(fit2, use.n = TRUE)

#######################################
# outro exemplo de baseline com iris
#######################################
# decision tree
library(rpart)
# obtém os índices múltiplos de 5 do dataset
testidx <- which(1:length(iris[,1]) %% 5 == 0)
# cria dados para treino a partir dos índices que não são múltiplos de 5
iristrain <- iris[-testidx,]
# cria dados para teste a partir dos índices que são múltiplos de 5
iristest <- iris[testidx,]
#Train the decision tree
treemodel <- rpart(Species~., data=iristrain)
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(treemodel)
text(treemodel, use.n=T)
# desision stump
fit_stump <- rpart(Species~., data=iristrain, maxdepth = 1)
plot(fit_stump)
text(fit_stump, use.n=T)

#############################################################
# com regression
#############################################################

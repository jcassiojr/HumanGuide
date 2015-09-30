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
#Predict using the decision tree
prediction <- predict(treemodel, newdata=iristest, type='class') > #Use contingency table to see how accurate it is
table(prediction, iristest$Species)
names(nnet_iristrain)[8] <- 'virginica'

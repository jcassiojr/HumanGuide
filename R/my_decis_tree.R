# decision tree
library(rpart)
#Train the decision tree
treemodel <- rpart(Species~., data=iristrain)
plot(treemodel)
text(treemodel, use.n=T)
#Predict using the decision tree
prediction <- predict(treemodel, newdata=iristest, type='class') > #Use contingency table to see how accurate it is
table(prediction, iristest$Species)
names(nnet_iristrain)[8] <- 'virginica'
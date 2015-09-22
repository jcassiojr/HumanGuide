# logistic regression - binary classification using iris data
# incluindo coluna com valor binario para classificação
newcol = data.frame(isSetosa=(iristrain$Species == 'setosa')) 
traindata <- cbind(iristrain, newcol)
head(traindata)
formula <- isSetosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
logisticModel <- glm(formula, data=traindata, family='binomial') 
# Predict the probability for test data
prob <- predict(logisticModel, newdata=iristest, type= 'response') 
round(prob, 3)

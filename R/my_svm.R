# support vector machine
library(e1071)
tune <- tune.svm(Species~., data=iristrain, gamma=10^(-6:-1), cost=10^(1:4))
summary(tune)
model <- svm(Species~., data=iristrain, method="C-classification", kernel="radial", 
             probability=T, gamma=0.001, cost=10000)
prediction <- predict(model, iristest, probability=T)
table(iristest$Species, prediction)
prediction

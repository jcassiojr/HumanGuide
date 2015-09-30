# accuracy calculation
# accuracy = numero de decisões corretas feitas / número total de decisões tomadas
# error rate = 1 - accuracy
# ideia: criar matriz a partir da matriz de confusão
# estimating models- k-fold cross validation
# load the library
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
confusionMatrix(predictions, iris$Species)


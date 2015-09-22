# tree ensembles (BAGGING METHOD)
library(randomForest)
#Train 100 trees, random selected attributes
model <- randomForest(Species~., data=iristrain, nTree=500) 
#Predict using the forest
prediction <- predict(model, newdata=iristest, type='class')
table(prediction, iristest$Species)
importance(model)
#######################################
# tree ensembles (BOOSTING METHOD)
library(gbm)
iris2 <- iris
newcol = data.frame(isVersicolor=(iris2$Species=='versicolor'))
iris2 <- cbind(iris2, newcol)
iris2[45:55,]
formula <- isVersicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
model <- gbm(formula, data=iris2, n.trees=1000, interaction.depth=2,
             distribution="bernoulli")
prediction <- predict.gbm(model, iris2[45:55,], type="response", n.trees=1000)
round(prediction, 3)
summary(model)

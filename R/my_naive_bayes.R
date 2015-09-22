# bayesian netwrks and naive bayes
library(e1071)
# Can handle both categorical and numeric input variables, but output must be categorical
model <- naiveBayes(Species~., data=iristrain)
prediction <- predict(model, iristest[,-5])
table(prediction, iristest[,5])

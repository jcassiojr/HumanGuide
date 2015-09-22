# regression with regularization
# incluindo coluna com valor binario para classificação
library(glmnet)
# realiza k-fold cross validation
cv.fit <- cv.glmnet(as.matrix(prestige_train[,c(-4,-6)]), 
                    as.vector(prestige_train[,4]), nlambda=100, alpha = 0.7, family = 'gaussian')
plot(cv.fit)
coef(cv.fit)
prediction <- predict (cv.fit, newx = as.matrix(prestige_test[,c(-4,-6)]))
cor(prediction, as.vector(prestige_test[,4]))

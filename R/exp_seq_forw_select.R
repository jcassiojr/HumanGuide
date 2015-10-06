# sequential forward selection (AIC = An Information Criterion)
# comparando o AIC entre modelos, escolhendo o menor é melhor para evitar overfitting

#data("EuStockMarkets")
# testar com iris dataset
# Só funciona com números, portanto, devo criar coluna binaria
# cria data.frame com uma coluna numerica para Species
library(dplyr)
df_iristrain <-
    iris %>%
    mutate(newcol = ifelse(Species == 'setosa',1,
                           ifelse(Species == 'virginica',2,
                                  ifelse(Species == 'versicolor',3,0))))
           
#newcol <- data.frame(num_Species=(iris$Species == 'setosa') | ) 
# mescla esta coluna no dataframe de treino
#traindata <- cbind(iris, newcol)
#step(glm(DAX ~ SMI + CAC + FTSE, data=EuStockMarkets), direction = "backward")
step(glm(newcol ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, data=df_iristrain), direction = "backward")
x <- glm(newcol ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, data=df_iristrain)
AIC(x)
drop1(x) # tira um preditor por vez e mede AIC
# uso manual de AIC
model1 <- glm(newcol ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, data=df_iristrain)
model2 <- glm(newcol ~ Petal.Length + Petal.Width + Sepal.Length, data=df_iristrain)
model3 <- glm(newcol ~ Petal.Length + Petal.Width, data=df_iristrain)
model4 <- glm(newcol ~ Petal.Length, data=df_iristrain)
AIC(model1,model2,model3,model4)
# plot de ROC de cada model

# or forward
step(glm(newcol ~ 1, data=df_iristrain), direction = "forward", scope = ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width)
# or both
step(glm(newcol ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, data=df_iristrain), direction = "both")

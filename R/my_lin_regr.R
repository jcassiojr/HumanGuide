# linear regression example
# estimanda a formula linear para dar prestige como combinação linear das demais features:
# (education, income, women, census, typeprof, typewc) para os dados de treino
model <- lm(prestige~., data=prestige_train)
# neste caso, se tem ao menos um asterísco a correlação é significativa
summary(model)
# Use the model to predict the output of test data
# predict aplica a fórmula gerada por model para obter a previsao de prestige a partir dos dados de teste
# prediction recebe um valor previsto para a feature prestige (Y) de cada linha a partir dos dados de teste
prediction <- predict(model, newdata=prestige_test) 
# Check for the correlation with actual result
cor(prediction, prestige_test$prestige)


# preparing training and testing data (using iris as an example)
# obtém os índices múltiplos de 5 do dataset
testidx <- which(1:length(iris[,1]) %% 5 == 0)
# cria dados para treino a partir dos índices que não são múltiplos de 5
iristrain <- iris[-testidx,]
# cria dados para teste a partir dos índices que são múltiplos de 5
iristest <- iris[testidx,]

# preparing training and testing data (using Prestige as an example)
library(car)
summary(Prestige)
# obtém os índices múltiplos de 5 do dataset
test2idx <- which(1:nrow(Prestige) %% 4 == 0)
# cria dados para treino a partir dos índices que não são múltiplos de 5
prestige_train <- Prestige[-test2idx,]
# cria dados para teste a partir dos índices que são múltiplos de 5
prestige_test <- Prestige[test2idx,]

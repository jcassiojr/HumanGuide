# dicas gerais R
# filtrando dataframe por row.name
names.to.keep <- c("nR04")
rows.to.keep<-which(rownames(x) %in% names.to.keep) # x sendo data.frame

# acessando atributos de objetos diversos
# exemplo: objeto performance do package ROCR
perf <- performance(pred,"tpr","fpr")
perf@x.name

# uso de predição com dados de teste sem target
data <- matrix(rnorm(200*10),200,10)
tr <- data[1:150,]
ts <- data[151:200,]

mod <- ksvm(x = tr[,-1],
            y = tr[,1],
            kernel = "rbfdot", type = 'nu-svr',
            kpar = "automatic", C = 60, cross = 3)

pred <- predict(mod, 
                ts[,-1]
)

# disparar stop condicional dentro de funcão
stopifnot("age" %in% names(newdata))


# para não iniciar automaticamente o X11 quando carrega library ggplot2
library(devtools)
install_version("colorspace", "1.2-4")
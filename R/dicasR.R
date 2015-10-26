# dicas gerais R
# filtrando dataframe por row.name
names.to.keep <- c("nR04")
rows.to.keep<-which(rownames(x) %in% names.to.keep) # x sendo data.frame

# acessando atributos de objetos diversos
# exemplo: objeto performance do package ROCR
perf <- performance(pred,"tpr","fpr")
perf@x.name
# ESTÁ FUNCIONANDO MAS NÃO CONSIGO GENERALIZAR USANDO DPLYR!!!!!
# PARA RECEBER COMO PARAMETRO O NOME DA COLUNA
# exemplo de entropia e information gain
# fonte: https://github.com/philjette/InformationGain
library(dplyr)
#compute Shannon entropy
# recebe coluna do target no dataframe
entropy <- function(target) {
    freq <- table(target)/length(target)
    # vectorize
    vec <- as.data.frame(freq)[,2]
    #drop 0 to avoid NaN resulting from log2
    vec<-vec[vec>0]
    #compute entropy
    -sum(vec * log2(vec))
}

#returns IG for numerical variables.
# parametros
#   data: parent dataframe
#   feature: column name (as in "Species") of the feature selected
#   target: column name (as in "Species") of the target selected
#   bins: number of segments to split to
IG_numeric<-function(data, feature, target, bins=4) {
    # geting column number from column name

    feature_ncol <- match(feature,names(data))
    target_ncol <- match(target,names(data))
    
    #Strip out rows where feature is NA
    data<-data[!is.na(data[,feature_ncol]),]
    #compute entropy for the parent
    e0<-entropy(data[,target_ncol])
    # divide os dados em N (bins) segmentos e cria nova coluna em dados
    # para identificar cada segmento
    data$cat<-cut(data[,feature_ncol], breaks=bins, labels=c(1:bins))
    # divide o data frame pela coluna "cat", aplica funcao
    #dd_data<-ddply(data, "cat", here(summarise), 
    #               e=entropy(get(target)), 
    #               N=length(get(target)),
    #               min=min(get(feature)),
    #               max=max(get(feature))
    #)
    # falta generalizar abaixo pois não passei como parametro
    dd_data <-
        data %>%
        group_by(cat) %>%
        summarize(e=entropy(Species),
                  N=length(Species),
                  min=min(Sepal.Length),
                  max=max(Sepal.Length)
        )
    # abaixo nao funciona recebendo string do nome
    #dd_data <-
    #    data %>%
    #    group_by(cat) %>%
    #    summarize(e=entropy(target),
    #              N=length(target),
    #              min=min(feature),
    #              max=max(feature)
    #              )
    #calculate p for each value of feature
    dd_data$p<-dd_data$N/nrow(data)
    #compute IG
    IG<-e0-sum(dd_data$p*dd_data$e)
    
    return(IG)
}

#returns IG for categorical variables.
IG_cat<-function(data,feature,target){
    #Strip out rows where feature is NA
    data<-data[!is.na(data[,feature]),] 
    #use ddply to compute e and p for each value of the feature
    dd_data<-ddply(data, feature, here(summarise), e=entropy(get(target)), N=length(get(target)))
    dd_data <-
        data %>%
        group_by(feature) %>%
        summarize(e=entropy(Species),
                  N=length(Species),
                  min=min(Sepal.Length),
                  max=max(Sepal.Length)
        )
    
    # teste
    #d <- data %>%
    #    filter (cat == 4)
    #compute entropy for the parent
    e0<-entropy(data[,target])
    #calculate p for each value of feature
    dd_data$p<-dd_data$N/nrow(data)
    #compute IG
    IG<-e0-sum(dd_data$p*dd_data$e)
    
    return(IG)
}

# chamada para iris dataset

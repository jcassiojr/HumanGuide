#' function: f_ig_num
#' para tratar feature numérica
#'
require(plyr, warn.conflicts = TRUE)
require(dplyr)
require(ggplot2)
f_ig_num <- function(i_df,feature, target, bins = 4) {
    
    
    # geting column number from column name
    feature_ncol <- match(feature,names(i_df))
    target_ncol <- match(target,names(i_df))
    
    # strip out rows where feature is NA
    i_df<-i_df[!is.na(i_df[,feature_ncol]),]
    
    #compute entropy for the parent
    e0<-f_entropy(i_df[,target_ncol])
    
    # divide os dados em N (bins) segmentos e cria nova coluna em dados
    # para identificar cada segmento
    i_df$cat<-cut(i_df[,feature_ncol], breaks=bins, labels=c(1:bins))
    
    # sumariza pela coluna cat para os cáclulos abaixo
    # abaixo funciona mas não quando passado como parametro da funcao
    #dd_data <-
    #    i_df %>%
    #    group_by(cat) %>%
    #    summarize(
    #              e=f_entropy(Species),
    #              N=length(Species),
    #              min=min(Sepal.Length),
    #              max=max(Sepal.Length)
    #   )
    
    # sumariza pela coluna cat para os cálculos abaixo
    dd_data<-ddply(i_df, "cat", here(summarise),
                   e=f_entropy(get(target)), 
                   N=length(get(target)),
                   min=min(get(feature)),
                   max=max(get(feature))
    )
    
    #calculate p for each value of feature
    dd_data <-
        dd_data %>%
        mutate(p = N/nrow(i_df) )
    
    
    # plot feature entropy for category
    ggplot(data=dd_data, 
           aes(x = reorder(dd_data[,1], e), y=e)) +
        geom_bar(stat="identity",color = "black",
                 fill="#DD8888", 
                 width=dd_data$p)
    
    ########################################
    # cálculo de IG
    ########################################
    Ef <- as.numeric(summarize (dd_data,ef = sum(p * e)))
    IG <- e0 - Ef
    
    return(IG)
}
#library(FSelector)
#data(iris)
#result <- cfs(Species ~ ., iris)
#f <- as.simple.formula(result, "Species")

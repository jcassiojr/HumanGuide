#' function: f_ig
#' cuidado: se library "plyr" estiver carregada além da "dplyr"
#' gera msg: Error in n() : This function should not be called directly
#' solução: unload da library "plyr"

require(plyr, warn.conflicts = TRUE)
require(dplyr, warn.conflicts = TRUE)
f_ig_cat <- function(i_df,feature, target) {
    df_1 <-
        i_df %>%
        group_by_(feature) %>%
        summarise(ct = n()) %>%
        arrange_(feature)
    
    # sumarize by gill-color and edible
    df_feature <-
        i_df %>%
        group_by_(feature, target) %>%
        summarize(ct = n()) %>%
        mutate(ttreg = nrow(i_df)) %>%
        arrange_(feature)
    
    # merge both to prepare for calculating entropy
    df_ig <- merge(df_feature,df_1,by = c(feature))
    
    # calculate probability and log2 probability
    df_ig <-
        df_ig %>%
        mutate(p = ct.x/ct.y,
               p_log_p = p*log2(p),
               proportion = ct.y/ttreg) %>%
        arrange_(feature)
    
    # calculate entropy and proportion (weigth)
    df_entropy <-
        df_ig %>%
        group_by_(feature, quote(proportion)) %>%
        summarize(entropy = -sum(p_log_p)) %>%
        arrange_(feature)
    
    
    # criando gráfico
    # ordenando da menor para a maior entropia
    df_entropy <-
        df_entropy %>%
        data.frame() %>% # para forçar o objeto a ser data.frama puro
        arrange(entropy)

    
    # using ggplot
    library(ggplot2)
    
    ggplot(data=df_entropy, 
           aes(x = reorder(df_entropy[,1], entropy), y=entropy)) +
        geom_bar(stat="identity",color = "black",
                 fill="#DD8888", 
                 width=df_entropy$proportion*10)
    ########################################
    # cálculo de IG
    ########################################
    # soma das entropias dos nós multiplicado pela proporção
    Ef <- as.numeric(summarize (df_entropy,ef = sum(proportion * entropy)))
    
    IG = Ep - Ef
    return(IG)
}

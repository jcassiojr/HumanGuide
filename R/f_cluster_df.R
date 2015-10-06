# carrega dados de base a ser explorada para análise de cluster
f_cluster_df <- function()
{ 
    #if (!requireNamespace("openxlsx", quietly = TRUE)) {
    #    stop("openxlsx needed for this function to work. Please install it.",
    #         call. = FALSE)
    #} else {
    #    if(!require(openxlsx)){install.packages("openxlsx")}
    #} 
    
    # carrega base de cogumelos venenosos para análise exploratória (NÃO FUNCIONA POIS TEM QU ESER NUMÉRICO!)
    #df_mush <- read.csv("./data/agaricus-lepiota.data.txt", header = FALSE)
    #df_nmmush <- read.csv("./data/agaricus-lepiota.nomes.csv", header = FALSE)
    #v_nmmush <- df_nmmush[['V1']]
    #get.names<- function(x,y) {
    #    names(x)<-y
    #    x
    #}
    #df_mush <- get.names(df_mush,v_nmmush)
    #df_mush <- get.names(df_mush,gsub("-",".",names(df_mush),))
    
    #return(df_mush)
    data("filmData")
    df <- filmData[,-4]
    return(df)
}
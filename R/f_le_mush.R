# le dados de cogumelos venenosos
f_le_mush <- function() {
    # carregando dados de cogumelos
    df_mush <- read.csv("./data/agaricus-lepiota.data.txt", header = FALSE)
    # crregando nomes dos dados
    df_nmmush <- read.csv("./data/agaricus-lepiota.nomes.csv", header = FALSE)
    # colocando nomes nos títulos das colunas
    # transformando dataframe em vetor de nomes
    v_nmmush <- df_nmmush[['V1']]
    #class(v_nmmush)
    
    # função para retornar o dataframe com os nomes
    get.names<- function(x,y) {
        names(x)<-y
        x
    }
    # dataframe com os nomes das colunas
    df_mush <- get.names(df_mush,v_nmmush)
    
    return(df_mush)
}

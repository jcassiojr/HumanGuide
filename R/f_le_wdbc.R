# le dados de pesquisa de breast c
############################################
# usando exemplo mais próximo com resultado do Brest C como se fosse modelo HG
############################################
f_le_wdbc <- function() {
    #----------------------------------
    # carregando dados
    #----------------------------------
    df_wdbc <- read.csv("./data/wdbc.data.txt", header = FALSE)
    # carregando nomes dos dados
    df_nmwdbc <- read.csv("./data/wdbc.nomes.csv", header = FALSE)
    # transformando dataframe em vetor de nomes
    v_nmwdbc <- df_nmwdbc[['V1']]
    # função para retornar o dataframe com os nomes
    get.names<- function(x,y) {
        names(x)<-y
        x
    }
    # dataframe com os nomes das colunas
    df_wdbc <- get.names(df_wdbc,v_nmwdbc)
    # cria data.frame com uma coluna TRUE/FALSE
    #newcol <- data.frame(isM=(df_wdbc$Diagnosis == 'M')) 
    # mescla esta coluna no dataframe 
    #df_wdbc <- cbind(df_wdbc, newcol)
    
    return(df_wdbc)
}

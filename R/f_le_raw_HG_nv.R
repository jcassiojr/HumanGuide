# f_le_dadosHG
require("dplyr", quietly = TRUE, warn.conflicts = FALSE)
f_le_raw_HG_nv <- function() {
    # ----- carrega dados reais de Human Guide
    # estes dados são os obtidos pelo sistema HG
    df_goe <- read.csv2("./data/new/goe_20160426_0105.csv", encoding = "UTF-8", 
                             sep = "\t", header = TRUE)
    df_kroton <- read.csv2("./data/new/kroton_20160425_2217.csv", encoding = "UTF-8", 
                              sep = "\t", header = TRUE)
    # eliminando linhas erradas, com TIPOUSER = "SP" ou "DF"
    df_rh99 <- read.csv2("./data/new/rh99_20160425_2158.csv", encoding = "UTF-8", 
                           sep = "\t", header = TRUE)
    # elimina linhas com dados que estavam na coluna errada
    df_rh99 <-
        df_rh99 %>%
        filter(TIPOUSER != "SP" & TIPOUSER != "DF")
    
    # concatena as 3 bases 
    df_out <- rbind(df_goe, df_kroton, df_rh99)
    #df_out <- df_rh99
    # refactoring
    df_out$TIPOUSER <- factor(df_out$TIPOUSER)
    
    # eliminando coluna "X"
    df_out <-
        df_out %>%
        select(-X,-ID) # eliminando coluna "X" e "ID" (esta por não ser única).
    
    # criando um índice único UIDs para cada respondente do arquivo concatenado
    # OBS: se uma mesma pessoa respondeu mais de uma ve, ganha ID diferente 
    df_out$ID <- 1:nrow(df_out)
    
    # alternativamente, testar com estes dados obtidos para a tese
    # com esses dados foi pior: usá-los somente para uso na próxima vez!!!
    #df_hg_use <- read.xlsx("./data/Dados pesquisa HG2007 convertida 11-09-11.xlsx", 
    #                   sheetIndex = 1, header = TRUE)
    # retira acentos de cedilhas
    names(df_out) <- gsub("ç","c",names(df_out))
    names(df_out) <- gsub("ã","a",names(df_out))
    names(df_out) <- gsub("õ","o",names(df_out))
    names(df_out) <- gsub("á","a",names(df_out))
    
    # acrescentar colunas para
    # 
    #l_df <- list(df_hg_train = df_hg_train, df_hg_use = df_hg_use)
    
    # DESATIVADO ATÉ TER DADOS REAIS
    # turnover (mais simples: s/n. depois sofisticar para considerar número de meses
    # de permanência), considerando balanceada
    #turnover <- as.factor(sample(x=c("s","n"), size=nrow(df_hg),
    #                   replace=TRUE, prob=c(1/2,1/2)))
    # inserindo dados de turnover
    #df_hg$turnover <- turnover
    
    # OBS: TESTAR CRIACAO DE DADOS CATEGORICOS A PARTIR DA COLUNA IDADE USANDO:
    # creating categorical variables
    # restData$zipGroups = cut(restData$zipCode,breaks=quantile(restData$zipCode))
    #df_hg_train$Grupoidade = cut(df_hg_train$idade,breaks=quantile(df_hg_train$idade))
    return (df_out)
}
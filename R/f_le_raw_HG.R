# f_le_dadosHG
require("xlsx")
f_le_raw_HG <- function() {
    set.seed(1)
    # ----- carrega dados reais de Human Guide
    # estes dados são os obtidos pelo sistema HG
    df_hg_train <- read.xlsx("./data/PP_answers_20110909_202700.xlsx", 
                         sheetIndex = 1, header = TRUE)
    # alternativamente, testar com estes dados obtidos para a tese
    # com esses dados foi pior: usá-los somente para uso na próxima vez!!!
    df_hg_use <- read.xlsx("./data/Dados pesquisa HG2007 convertida 11-09-11.xlsx", 
                       sheetIndex = 1, header = TRUE)
    # retira acentos de cedilhas
    names(df_hg_use) <- gsub("ç","c",names(df_hg_use))
    names(df_hg_use) <- gsub("ã","a",names(df_hg_use))
    names(df_hg_use) <- gsub("õ","o",names(df_hg_use))
    
    l_df <- list(df_hg_train = df_hg_train, df_hg_use = df_hg_use)
    
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
    
    return (l_df)
}
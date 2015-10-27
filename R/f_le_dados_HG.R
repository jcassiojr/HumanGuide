# f_le_dadosHG
require("xlsx")
f_le_dados_HG <- function() {
    set.seed(1)
    # ----- carrega dados reais de Human Guide
    # estes dados são do estudo antigo?
    #df_hgr <- read.xlsx("./data/Dados pesquisa HG2007 convertida 11-09-11.xlsx", 
    #                    sheetIndex = 1, header = TRUE)
    # estes dados são os obtidos pelo sistema HG
    #df_hg <- read.xlsx("./data/PP_answers_20110909_202700.xlsx", 
    #                     sheetIndex = 1, header = TRUE)
    # alternativamente, testar com estes dados
# com esses dados foi pior: usá-los somente para teste set na próxima vez!!!
    df_hg <- read.xlsx("./data/Dados pesquisa HG2007 convertida 11-09-11.xlsx", 
                       sheetIndex = 1, header = TRUE)
    # turnover (mais simples: s/n. depois sofisticar para considerar número de meses de permanência)
    # considerando balanceada
    turnover <- as.factor(sample(x=c("s","n"), size=nrow(df_hg),
                       replace=TRUE, prob=c(1/2,1/2)))
    # inserindo dados de turnover
    df_hg$turnover <- turnover
    
    return (df_hg)
}
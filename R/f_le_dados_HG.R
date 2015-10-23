# f_le_dadosHG
f_le_dados_HG <- function() {
    set.seed(1)
    # ----- carrega dados reais de Human Guide
    # estes dados são do estudo antigo?
    #df_hgr <- read.xlsx("./data/Dados pesquisa HG2007 convertida 11-09-11.xlsx", 
    #                    sheetIndex = 1, header = TRUE)
    # estes dados são os obtidos pelo sistema HG
    df_hg <- read.xlsx("./data/PP_answers_20110909_202700.xlsx", 
                         sheetIndex = 1, header = TRUE)
    # turnover (mais simples: s/n. depois sofisticar para considerar número de meses de permanência)
    # considerando balanceada
    turnover <- as.factor(sample(x=c("s","n"), size=1327,
                       replace=TRUE, prob=c(1/2,1/2)))
    # inserindo dados de turnover
    df_hg$turnover <- turnover
    
    # Prepare Data - listwise deletion of missing (should I standardize variables?)
    df_hg <- na.omit(df_hg) # listwise deletion of missing
    return (df_hg)
}
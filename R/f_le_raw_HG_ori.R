# f_le_dadosHG
# Esta função lê os dados originais para teste do HG
require("xlsx")
require("dplyr")
f_le_raw_HG_ori <- function() {
    #set.seed(1)
    # ----- carrega dados reais de Human Guide
    # estes dados são os obtidos pelo sistema HG
    df_hg_train <- read.xlsx("./data/PP_answers_20110909_202700.xlsx", 
                         sheetIndex = 1, header = TRUE)
    df_hg_train <-
        df_hg_train %>%
        select(ID, sexo,
               s11, h21, h31, hy41, e51, m61, m71, p81, e91,
               e12, e22, e32, s42, s52, s62, k72, h82, m92,
               h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
               k14, d24, k34, k44, k54, hy64, p74, s84, d94,
               p15, m25, s35, h45, d55, k65, hy75, m85, k95,
               hy16, p26, p36, m46, h56, p66, h76, k86, s96,
               d17, s27, d37, d47, p57, d67, e77, hy87, h97,
               m18, hy28, m38, p48, m58, h68, d78, d88, hy98)
    
    # alternativamente, testar com estes dados obtidos para a tese
    # com esses dados foi pior: usá-los somente para uso na próxima vez!!!
    df_hg_use <- read.xlsx("./data/Dados pesquisa HG2007 convertida 11-09-11.xlsx", 
                       sheetIndex = 1, header = TRUE)
    df_hg_use <-
        df_hg_use %>%
        select(ID, sexo,
               s11, h21, h31, hy41, e51, m61, m71, p81, e91,
               e12, e22, e32, s42, s52, s62, k72, h82, m92,
               h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
               k14, d24, k34, k44, k54, hy64, p74, s84, d94,
               p15, m25, s35, h45, d55, k65, hy75, m85, k95,
               hy16, p26, p36, m46, h56, p66, h76, k86, s96,
               d17, s27, d37, d47, p57, d67, e77, hy87, h97,
               m18, hy28, m38, p48, m58, h68, d78, d88, hy98)

    # retira acentos de cedilhas
    names(df_hg_use) <- gsub("ç","c",names(df_hg_use))
    names(df_hg_use) <- gsub("ã","a",names(df_hg_use))
    names(df_hg_use) <- gsub("õ","o",names(df_hg_use))
    
    
    
    df_ori <- rbind(df_hg_train, df_hg_use)
    # muda sexo de numerico para caracter
    df_ori <-
        df_ori %>%
        mutate(sexo = ifelse(sexo == 1, "m","f"))  
    
    # elimina valores de observações com "?"
    df_ori <- as.data.frame(lapply(df_ori,function(x) if(is.character(x)|is.factor(x)) gsub("\\?",NA,x) else x))
    df_ori <- na.omit(df_ori) # listwise deletion of missing
    
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
    #df_ori$Grupoidade = cut(df_ori$idade,breaks=quantile(df_ori$idade))
    return (df_ori)
}
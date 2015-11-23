#' prepara dataset de scores a partir dos dados originais HG
#' param: i_df -> dataframe com dados do teste HG e coluna de turnover
#' return: dataframe com scores calculados
require(dplyr)

f_tidy_scores_HG <- function(df_in) {
    # substitui valores "?" por NA
    # obs: como estas colunas vieram com "?" foram transformadas em Factor
    # por isso uso as.numeric nelas abaixo)
    # substituição por NA pode estar mascarando os scores!! melhor solução é eliminar linhas
    # onde existe o caracter "?"
    #df_in <- as.data.frame(lapply(df_in,function(x) if(is.character(x)|is.factor(x)) gsub("\\?",NA,x) else x))
    
    # primeiro somente selecionando as colunas necessárias para cálculo de ranking
    df_in <-
        df_in %>%
        select(ID,
               s11, h21, h31, hy41, e51, m61, m71, p81, e91,
               e12, e22, e32, s42, s52, s62, k72, h82, m92,
               h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
               k14, d24, k34, k44, k54, hy64, p74, s84, d94,
               p15, m25, s35, h45, d55, k65, hy75, m85, k95,
               hy16, p26, p36, m46, h56, p66, h76, k86, s96,
               d17, s27, d37, d47, p57, d67, e77, hy87, h97,
               m18, hy28, m38, p48, m58, h68, d78, d88, hy98)
    
    # agora eliminando linhas com NAs
    df_in <- na.omit(df_in) # listwise deletion of missing
    
    # seleciona apenas as colunas necessárias
    df_out <-
        df_in %>%
        mutate(sensibility = s11 + h21 + h31 + hy41 + e51 + m61 + m71 + p81 + e91,
               power = e12 + e22 + e32 + s42 + s52 + s62 + k72 + h82 + m92,
               quality = h13 + k23 + hy33 + e43 + hy53 + e63 + s73 + e83 + p93,
               exposure = k14 + d24 + k34 + k44 + k54 + hy64 + p74 + s84 + d94,
               structure = p15 + m25 + s35 + h45 + d55 + k65 + hy75 + m85 + k95,
               imagination = hy16 + p26 + p36 + m46 + h56 + p66 + h76 + k86 + s96,
               stability = d17 + s27 + d37 + d47 + p57 + d67 + e77 + hy87 + h97,
               contacts = m18 + hy28 + m38 + as.numeric(p48) + m58 + h68 + d78 + d88 + as.numeric(hy98)) %>%
        #select(ID, turnover, sexo, escolaridade, formação, ramoativ, cargo, cidade, power, quality,
        select(ID, sensibility, power, quality,
               exposure, structure, imagination, stability, contacts) 
        #mutate(sexo = ifelse(sexo == 1, "m", "f"))
 
    return(df_out)
}
    
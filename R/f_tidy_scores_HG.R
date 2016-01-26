#' prepara dataset de scores a partir dos dados originais HG
#' param: i_df -> dataframe com dados do teste HG e coluna de turnover
#' return: dataframe com scores calculados
#require(dplyr)

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
        mutate(p48 = as.numeric(as.vector(p48)),
               hy98 = as.numeric(as.vector(hy98))) %>%
        select(ID, TIPOUSER, sexo, idade, cep, cidade, nomerespondente, formacao.em, profissao.na.area.de,
               s11, h21, h31, hy41, e51, m61, m71, p81, e91,
               e12, e22, e32, s42, s52, s62, k72, h82, m92,
               h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
               k14, d24, k34, k44, k54, hy64, p74, s84, d94,
               p15, m25, s35, h45, d55, k65, hy75, m85, k95,
               hy16, p26, p36, m46, h56, p66, h76, k86, s96,
               d17, s27, d37, d47, p57, d67, e77, hy87, h97,
               m18, hy28, m38, p48, m58, h68, d78, d88, hy98)
    
    # colocando NA como indefinido em formação e ocupação
    df_in <-
        df_in %>%
        mutate(profissao.na.area.de = ifelse(is.na(profissao.na.area.de), "INDEFINIDO", as.character(profissao.na.area.de)),
               formacao.em = ifelse(is.na(formacao.em), "INDEFINIDO", as.character(formacao.em)))
    
    # agora eliminando linhas com NAs
    df_in <- na.omit(df_in) # listwise deletion of missing
    
    # seleciona apenas as colunas necessárias
    df_out <-
        df_in %>%
        mutate(sensibility = h13 + h21 + h31 + h45 + h56 + h68 + h76 + h82 + h97,
               power = s11 + s27 + s35 + s42 + s52 + s62 + s73 + s84 + s96,
               quality = e12 + e22 + e32 + e43 + e51 + e63 + e77 + e83 + e91,
               exposure = hy16 + hy28 + hy33 + hy41 + hy53 + hy64 + hy75 + hy87 + hy98,
               structure = k14 + k23 + k34 + k44 + k54 + k65 + k72 + k86 + k95,
               imagination = p15 + p26 + p36 + p48 + p57 + p66 + p74 + p81 + p93,
               stability = d17 + d24 + d37 + d47 + d55 + d67 + d78 + d88 + d94,
               contacts = m18 + m25 + m38 + m46 + m58 + m61 + m71 + m85 + m92) %>%
        #select(ID, turnover, sexo, escolaridade, formação, ramoativ, cargo, cidade, power, quality,
        # examinar p48 e hy98 para ver porque teve que transformar em numerico
        select(ID, TIPOUSER, sexo, nomerespondente, formacao.em, profissao.na.area.de, sensibility, power, quality,
               exposure, structure, imagination, stability, contacts, idade, cep, cidade) 
        #mutate(sexo = ifelse(sexo == 1, "m", "f"))
 
    return(df_out)
}
    
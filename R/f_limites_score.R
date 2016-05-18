# funcao que salva e retorna data frame com scores máximos e mínimos como baseline
# para classificação de intensidade de componente para respondente
require("dplyr", quietly = TRUE, warn.conflicts = FALSE)
require("xlsx", quietly = TRUE, warn.conflicts = FALSE)
source("./R/f_acentos.R") 

f_limites_score <- function() {
    
    # lendo dados de rh99 com codigos ao invés de descrições
    df_raw_hg <- read.csv2("./data/new/rh99_20160425_2158.csv", encoding = "UTF-8", sep = "\t", header = TRUE)
    # filtrando para tirar colunas deslocadas e removendo coluna X
    df_raw_hg <-
        df_raw_hg %>%
        filter(!grepl("df|sp",TIPOUSER)) %>%
        select(-X)
    # normaliza dados retirando acentos, cedilha e forçando minusculas nos campos string
    #df_raw_hg <- f_acentos(df_raw_hg)
    
    # calcula os fatores de acordo com a pontuação
    df_raw_hg <-
        df_raw_hg %>%
        select(ID, nomerespondente, sexo, idade, 
               cidade, uf, profissão.na.área.de, formacao.em,
               s11, h21, h31, hy41, e51, m61, m71, p81, e91,
               e12, e22, e32, s42, s52, s62, k72, h82, m92,
               h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
               k14, d24, k34, k44, k54, hy64, p74, s84, d94,
               p15, m25, s35, h45, d55, k65, hy75, m85, k95,
               hy16, p26, p36, m46, h56, p66, h76, k86, s96,
               d17, s27, d37, d47, p57, d67, e77, hy87, h97,
               m18, hy28, m38, p48, m58, h68, d78, d88, hy98)
    
    # changing factor to numeric
    unfactorize<-c(9:80)
    df_raw_hg[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df_raw_hg[,x])))
    
    # seleciona apenas as colunas necessárias
    df_tidy_hg <-
        df_raw_hg %>%
        mutate(sensibility = h13 + h21 + h31 + h45 + h56 + h68 + h76 + h82 + h97,
               power = s11 + s27 + s35 + s42 + s52 + s62 + s73 + s84 + s96,
               quality = e12 + e22 + e32 + e43 + e51 + e63 + e77 + e83 + e91,
               exposure = hy16 + hy28 + hy33 + hy41 + hy53 + hy64 + hy75 + hy87 + hy98,
               structure = k14 + k23 + k34 + k44 + k54 + k65 + k72 + k86 + k95,
               imagination = p15 + p26 + p36 + p48 + p57 + p66 + p74 + p81 + p93,
               stability = d17 + d24 + d37 + d47 + d55 + d67 + d78 + d88 + d94,
               contacts = m18 + m25 + m38 + m46 + m58 + m61 + m71 + m85 + m92) %>%
        select(ID, nomerespondente, sexo, idade, cidade, uf, profissão.na.área.de, formacao.em, sensibility, power, quality,
               exposure, structure, imagination, stability, contacts)
    
    # remove dados com sujeira
    df_tidy_hg <-
        df_tidy_hg %>%
        filter(!(is.na(power)))
    # aplica a função de análise de componentes principais para obter os scores dos componentes
    pca1 = prcomp(df_tidy_hg[9:16], scale. = TRUE, center = TRUE)
    # scores obtidos
    scores.total <- as.data.frame(pca1$x)
    my.scores.total <- as.data.frame(cbind(ID = df_tidy_hg$ID, 
                                           #sexo = df_tidy_hg$sexo, 
                                           #tipouser = df_tidy_hg$TIPOUSER, 
                                           profissao.na.area.de = df_tidy_hg$profissão.na.área.de, 
                                           formacao.em = df_tidy_hg$formacao.em,
                                           scores.total))
    # cria data frame com limites inferior e superior dos scores por componentes
    out.score.limites <- data.frame(componente = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7"),
                                   max.score = c(max(my.scores.total$PC1),
                                                 max(my.scores.total$PC2),
                                                 max(my.scores.total$PC3),
                                                 max(my.scores.total$PC4),
                                                 max(my.scores.total$PC5),
                                                 max(my.scores.total$PC6),
                                                 max(my.scores.total$PC7)),
                                   min.score = c(min(my.scores.total$PC1),
                                                 min(my.scores.total$PC2),
                                                 min(my.scores.total$PC3),
                                                 min(my.scores.total$PC4),
                                                 min(my.scores.total$PC5),
                                                 min(my.scores.total$PC6),
                                                 min(my.scores.total$PC7)))              
    
    
    # salva data frame
    #write.xlsx2(out.score.limites, "./data/limites-score.xlsx", row.names = FALSE)
    
    return(out.score.limites)
}
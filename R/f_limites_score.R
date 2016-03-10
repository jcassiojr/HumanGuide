# funcao que salva e retorna data frame com scores máximos e mínimos como baseline
# para classificação de intensidade de componente para respondente
f_limites_score <- function() {
    #require("stringr", quietly = TRUE, warn.conflicts = FALSE)
    require("dplyr", quietly = TRUE, warn.conflicts = FALSE)
    require("xlsx", quietly = TRUE, warn.conflicts = FALSE)
    #require("reshape2", quietly = TRUE, warn.conflicts = FALSE)
    
    source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
    source("./R/f_tidy_scores_HG.R") # alterada em 07/12/2015 para trazer nome do respondente tb
    df_raw_hg <- f_le_raw_HG() # lê toda a amostra de dados HG
    
    df_tidy_hg <- f_tidy_scores_HG(df_raw_hg) # calcula os fatores de acordo com a puntuação
    
    # chama funcao que tira acentos e força minúsculos
    df_tidy_hg <- f_acentos(df_tidy_hg)
    
    # para testes usar somente 1000 ocorrências na amostra
    #tam.amostra = 1000
    #df_tidy_hg <- 
    #    df_tidy_hg %>%
    #    sample_n(tam.amostra)
    # restaura fatores como numericos
    df_tidy_hg <-
        df_tidy_hg %>%
        mutate(sensibility = as.numeric(as.vector(sensibility)),
               power = as.numeric(as.vector(power)),
               quality = as.numeric(as.vector(quality)),
               exposure = as.numeric(as.vector(exposure)),
               structure = as.numeric(as.vector(structure)),
               imagination = as.numeric(as.vector(imagination)),
               stability = as.numeric(as.vector(stability)),
               contacts = as.numeric(as.vector(contacts)))
    
    pca1 = prcomp(df_tidy_hg[,7:14], scale. = TRUE, center = TRUE)
    # scores obtidos
    scores.total <- as.data.frame(pca1$x)
    my.scores.total <- as.data.frame(cbind(ID = df_tidy_hg$ID, 
                                           sexo = df_tidy_hg$sexo, 
                                           tipouser = df_tidy_hg$TIPOUSER, 
                                           profissao.na.area.de = df_tidy_hg$profissao.na.area.de, 
                                           formacao.em = df_tidy_hg$formacao.em,
                                           scores.total))
    # troca de TIPOUSER vazio por "indefinido"
    my.scores.total <-
        my.scores.total %>%
        mutate(tipouser = ifelse(tipouser == "", "indefinido", as.character(tipouser)),
               sexo = ifelse(sexo == "f", "feminino","masculino"))
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
                                                 min(my.scores.total$PC7))              
    )
    
    # salva data frame
    write.xlsx2(out.score.limites, "./data/limites-score.xlsx", row.names = FALSE)
    
    return(out.score.limites)
}
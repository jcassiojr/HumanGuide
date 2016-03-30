#' função f_assinatura_HG
#' Gera os plots de assinatura e de classificacao em campos profissionais de respondente selecionado
#' input:
#' nome do arquivo a partir do qual será selecionado o repsondente
#' nome do respondente a gerar os plots
#' output:
#' lista com:
#' plot de assinatura do respondente em relação aos componentes
#' plot de classificação do respondente em relação a campos profissionais
require("doMC", quietly = TRUE, warn.conflicts = FALSE)
require("ggplot2", quietly = TRUE, warn.conflicts = FALSE)
require("xlsx", quietly = TRUE, warn.conflicts = FALSE)
require("dplyr", quietly = TRUE, warn.conflicts = FALSE)

source("./R/f_acentos.R") 

f_assinatura_individual_HG <- function(in.arquivo, in.nome) {
    # ATENCAO: comentar abaixo apos terminados os testes da funcao
    #in.arquivo <- "pp_humanguide_20160307-1349.xlsx"
    #in.nome <- "beatriz welter"
    # ATENCAO +++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    registerDoMC(8) # parallel processing
    
    # OBTENDO SCORES MAXIMOS E MINIMOS
    ##################################
    
    # ou, lendo de arquivo já gravado pela funcao f_limites_score()
    my.score.limites <- read.xlsx ("./data/limites-score.xlsx", sheetIndex = 1)
    
    # limites considerados para percentual de intensidade
    lims.classif <- c(.3, .15, .05) # classificação cass
    
    # OBTENDO SCORES DE UMA PESSOA POR input nome
    ###############################################
  
    df_respondentes <- read.xlsx(paste0("./data/", in.arquivo), encoding = "UTF-8", 
                    sheetIndex = 1, header = TRUE)

    df_respondentes <-
        df_respondentes %>%
        select(ID, nomerespondente, sexo, idade, atua.em.1, atua.em.2, atua.em.3, 
           cidade, uf, profissao, formacao,
           s11, h21, h31, hy41, e51, m61, m71, p81, e91,
           e12, e22, e32, s42, s52, s62, k72, h82, m92,
           h13, k23, hy33, e43, hy53, e63, s73, e83, p93,
           k14, d24, k34, k44, k54, hy64, p74, s84, d94,
           p15, m25, s35, h45, d55, k65, hy75, m85, k95,
           hy16, p26, p36, m46, h56, p66, h76, k86, s96,
           d17, s27, d37, d47, p57, d67, e77, hy87, h97,
           m18, hy28, m38, p48, m58, h68, d78, d88, hy98)

    # seleciona apenas as colunas necessárias
    df_respondentes <-
        df_respondentes %>%
        mutate(sensibility = h13 + h21 + h31 + h45 + h56 + h68 + h76 + h82 + h97,
           power = s11 + s27 + s35 + s42 + s52 + s62 + s73 + s84 + s96,
           quality = e12 + e22 + e32 + e43 + e51 + e63 + e77 + e83 + e91,
           exposure = hy16 + hy28 + hy33 + hy41 + hy53 + hy64 + hy75 + hy87 + hy98,
           structure = k14 + k23 + k34 + k44 + k54 + k65 + k72 + k86 + k95,
           imagination = p15 + p26 + p36 + p48 + p57 + p66 + p74 + p81 + p93,
           stability = d17 + d24 + d37 + d47 + d55 + d67 + d78 + d88 + d94,
           contacts = m18 + m25 + m38 + m46 + m58 + m61 + m71 + m85 + m92) %>%
        select(ID, nomerespondente, sexo, idade, cidade, uf, profissao, formacao, sensibility, power, quality,
           exposure, structure, imagination, stability, contacts) 

    #df_tidy_hg <- f_tidy_scores_HG(df_respondentes) # calcula os fatores de acordo com a puntuação

    # chama funcao que tira acentos e força minúsculos
    df_respondentes <- f_acentos(df_respondentes)

    # restaura fatores como numericos
    df_respondentes <-
        df_respondentes %>%
        mutate(sensibility = as.numeric(as.vector(sensibility)),
           power = as.numeric(as.vector(power)),
           quality = as.numeric(as.vector(quality)),
           exposure = as.numeric(as.vector(exposure)),
           structure = as.numeric(as.vector(structure)),
           imagination = as.numeric(as.vector(imagination)),
           stability = as.numeric(as.vector(stability)),
           contacts = as.numeric(as.vector(contacts)))

    # calculo dos novos fatores para os respondentes do arquivo lido
    # obtendo os scores de acordo com a análise de componentes principais
    ###############################################################################

    pca1 = prcomp(df_respondentes[,9:16], scale. = TRUE, center = TRUE)
    # scores obtidos
    scores.resp<- as.data.frame(pca1$x)
    my.scores.resp <- as.data.frame(cbind(ID =  df_respondentes$ID,
                                     nomerespondente = df_respondentes$nomerespondente,
                                     sexo = df_respondentes$sexo, 
                                     profissao = df_respondentes$profissao, 
                                     formacao = df_respondentes$formacao,
                                     scores.resp))
    # eliminando PC8
    my.scores.resp <-
        my.scores.resp %>%
        select (-PC8)

    # OBTENDO SCORES DE UMA PESSOA PARA OS COMPONENTES
    ###############################################
    # selecionando somente linha do respondente desejado para gerar o plot
    my.scores.resp <- 
        my.scores.resp %>%
        filter(nomerespondente == in.nome) %>%
        select(nomerespondente, PC1, PC2, PC3, PC4, PC5, PC6, PC7)
    
    # colocando scores do selecionado no data frame de limites de score
    my.score.limites$scores.resp <- as.numeric(my.scores.resp[,2:8])
    #nome.resp <- "Alex"
    
    # se score é negativo, calcula o percentual em relação ao mínimo e vice-versa
    my.classif <- 
        my.score.limites %>%
        mutate(perc = abs(ifelse(scores.resp <= 0, -(scores.resp/min.score), (scores.resp/max.score))))
    # inserindo o atributo do componente (positivo ou negativo)
    my.classif <-
        my.classif %>%
        mutate(atributo = ifelse(scores.resp >= 0 & componente == "PC1","OPENNESS",
                        ifelse(scores.resp < 0 & componente == "PC1","CONSCIOUSNESS",
                        ifelse(scores.resp >= 0 & componente == "PC2","AGREEABLENESS",
                        ifelse(scores.resp < 0 & componente == "PC2","DETERMINATION",
                        ifelse(scores.resp >= 0 & componente == "PC3","LONG TERM VISION",
                        ifelse(scores.resp < 0 & componente == "PC3","COMMITMENT",
                        ifelse(scores.resp >= 0 & componente == "PC4","IDEALISM",
                        ifelse(scores.resp < 0 & componente == "PC4","REALISM",
                        ifelse(scores.resp >= 0 & componente == "PC5","EXTROVERSION",
                        ifelse(scores.resp < 0 & componente == "PC5","SOBRIETY",
                        ifelse(scores.resp >= 0 & componente == "PC6","MANAGEMENT",
                        ifelse(scores.resp < 0 & componente == "PC6","RECEPTIVITY",
                        ifelse(scores.resp >= 0 & componente == "PC7","PERSISTENCE",
                                                                        "ADJUSTMENT"
                        ))))))))))))))

    # inserindo classificaçao segundo intensidade do score
    my.classif <-
        my.classif %>%
        mutate(intensidade = ifelse(perc > lims.classif[1],"marcante",
                                    ifelse(perc <= lims.classif[1] & perc > lims.classif[2] ,"moderado",
                                           ifelse(perc <= lims.classif[2] & perc > lims.classif[3] ,"fraco",
                                                  "inexpressivo"
                                           ))))
    # ordenando fatores para fixar ordem da legenda
    my.classif$intensidade <- factor(my.classif$intensidade, 
                                     levels = c("marcante","moderado","fraco","inexpressivo"))
    # ordenando fatores para fixar ordem do eixo x (atributos do PC)
    my.classif$atributo <- factor(my.classif$atributo, 
                                     levels = c("OPENNESS","CONSCIOUSNESS",
                                                "AGREEABLENESS","DETERMINATION",
                                                "LONG TERM VISION","COMMITMENT",
                                                "IDEALISM","REALISM",
                                                "EXTROVERSION","SOBRIETY",
                                                "MANAGEMENT","RECEPTIVITY",
                                                "PERSISTENCE","ADJUSTMENT"
                                                ))
    

    #plotar
    plot.assinatura.resp <- ggplot(my.classif, aes(x=atributo, y=perc)) +
                        geom_bar(aes (fill= factor(intensidade)), stat="identity") +
                        scale_fill_manual(values=c("skyblue", "lightgreen", "yellow", "red1")) +
                        ggtitle(paste("(", in.nome,")")) +
                        xlab("componente Human Guide") +
                        ylab("intensidade") +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # OBTENDO SCORES DE UMA PESSOA PARA OS CAMPOS PROFISSIONAIS
    ###############################################
    my.baseline.assin <- read.xlsx2("./data/BaselineAssinaturaCamposProfisisonais-V2.xlsx", 
                                sheetIndex = 1, colIndex = c(1:12), header = TRUE)
    my.baseline.assin <-
        my.baseline.assin %>%
        select(-X.)
    # mudando fatores para numerico (atenção: está jogando N na coluna que identifica os PCs. Concertar depois!!)
    unfactorize<-c(2:dim(my.baseline.assin)[2])
    my.baseline.assin[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(my.baseline.assin[,x])))
    
    # colocando scores do selecionado no data frame de meaids de score por campo profissional
    my.baseline.assin$nome.resp <- as.numeric(my.scores.resp[,2:8])
    
    # calcula a correlação entre o score do respondente e o score médio para cada campo profissional
    my.df_campos <- data.frame(campo.prof = c("CFM", "CFQ", "CCF", "COA", "CJS", "CCP", "CSL", "CMA", "CCE", "CBS"),
                         corr = c(cor(my.baseline.assin[,c(2,12)])[1,2],
                                  cor(my.baseline.assin[,c(3,12)])[1,2],
                                  cor(my.baseline.assin[,c(4,12)])[1,2],
                                  cor(my.baseline.assin[,c(5,12)])[1,2],
                                  cor(my.baseline.assin[,c(6,12)])[1,2],
                                  cor(my.baseline.assin[,c(7,12)])[1,2],
                                  cor(my.baseline.assin[,c(8,12)])[1,2],
                                  cor(my.baseline.assin[,c(9,12)])[1,2],
                                  cor(my.baseline.assin[,c(10,12)])[1,2],
                                  cor(my.baseline.assin[,c(11,12)])[1,2]
                         ))
    
    # inserindo direcao da correlação (positiva ou negativa)
    my.df_campos <-
        my.df_campos %>%
        mutate(direcao = ifelse(corr >= 0,"positivo",
                                "negativo"
        ))
    
    # inserindo classificaçao e direcao (positiva ou negativa) PODE HAVER ERRO AQUI AO CONSIDERAR ABS!!
    my.df_campos <-
        my.df_campos %>%
        mutate(corr.abs = abs(corr),
               intensidade = ifelse(corr.abs > lims.classif[1],"marcante",
                                ifelse(corr.abs <= lims.classif[1] & corr.abs > lims.classif[2] ,"moderado",
                                ifelse(corr.abs <= lims.classif[2] & corr.abs > lims.classif[3] ,"fraco",
                                "inexpressivo"
                                )))) %>%
        select(-corr.abs)
    
    #plotar
    my.df_campos <-
        my.df_campos %>%
        mutate(ID.campo = seq(nrow((my.df_campos))))
    # classificando intensidade para ordenar no label ggplot
    my.df_campos$intensidade <- factor(my.df_campos$intensidade, 
                                        levels = c("marcante","moderado","fraco","inexpressivo"))
    
    # plot com pontos
    #qplot(df_class.resp$campo.prof,df_class.resp$corr, colour = df_class.resp$intensidade)
    # alternativa
    plot.campo.resp <- ggplot(my.df_campos, aes(x=campo.prof, y=corr, fill= intensidade)) +
        geom_bar(aes (fill= factor(intensidade)), stat="identity") +
        scale_fill_manual(values=c("skyblue", "lightgreen", "yellow", "red1")) +
        xlab("campo profissional") +
        ylab("intensidade") +
        ggtitle(paste("Correlações com Campos Profissionais (", in.nome,")"))

    # retorna lista com plots de assinatura e campo profisisional
    out.l_plots <- list(pl_campo = plot.campo.resp, pl_assinatura = plot.assinatura.resp)
    return(out.l_plots)
}

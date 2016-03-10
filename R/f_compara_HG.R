#' testes comparativos Energia Sustentavel
#' ideia: funcao que recebe o nome do respondente e faz a selecao dele abaixo para retornar os plots
#' MELHORIA: finalizada esta análise, criar funcao somente para arquivo novo
#' input: 
#'   nome do respondente

require("doMC")
require("ggplot2")
require("xlsx")
require("splitstackshape")
require("gridExtra")
require("dplyr")

source("./R/f_acentos.R") 
#source("./R/f_limites_score.R") 

f_compara_HG <- function(in.nome) {

    registerDoMC(5) # parallel processing
    
    # OBTENDO SCORES MAXIMOS E MINIMOS
    ##################################
    
    #my.score.limites <- f_limites_score()
    # ou, lendo de arquivo já gravado pela funcao f_limites_score()
    my.score.limites <- read.xlsx ("./data/limites-score.xlsx", sheetIndex = 1)
    
    # limites considerados para percentual de intensidade
    lims.classif <- c(.3, .15, .05) # classificação cass
    
    ####################################
    # TRATA CLASSIFICACAO ANTERIOR
    ####################################
    
    options(stringsAsFactors=FALSE)
    df_usu.ant <- read.csv2("./data/Classificacao respondentes_macrogrupos HG-V2.csv", encoding = "UTF-8", 
                         nrows = 40, sep = '\t', header = TRUE)
    # for some reason dont,t split by "\t", so I did as bellow
    #df_usu.ant <- as.data.frame(sapply(df_usu.ant, FUN = function(x) as.character(gsub("\t", ";", x))))

    my.names <- names(df_usu.ant)
    df_usu.ant <- cSplit(df_usu.ant, my.names, "\t")
    names(df_usu.ant) <- c("UserID","CPF","FullName","GroupCode","GroupName","Gender","Age","CP1","CP2","CP3")
    my.scores.ant <- f_acentos(df_usu.ant)

    # OBTENDO SCORES DE UMA PESSOA POR input nome
    ###############################################
    #my.score.limites$scores.resp <- scores.alex
    # selecionando somente linha do respondente desejado para gerar o plot
    df_PCs <- 
        my.scores.ant %>%
        filter(FullName == in.nome) %>%
        select(CP1, CP2, CP3)
    my.PCs.ant <- as.data.frame(t(df_PCs))
    my.PCs.ant$componente <- c("PC1", "PC2", "PC3")
    names(my.PCs.ant) <- c("valor", "componente")
    my.PCs.ant <- my.PCs.ant %>% mutate(valor = as.numeric(valor))
    # plot
    plot.classif.ant <- ggplot(my.PCs.ant, aes(x=componente, y=valor)) +
        geom_bar(stat="identity", fill="#FF9999", colour="black") +
        ggtitle(paste("Classificação de Respondente (", in.nome,")")) 
        # coord_polar()
    
    ####################################
    # TRATA CLASSIFICACAO NOVA
    ####################################
  
    df_usu.nov <- read.xlsx("./data/PP_09029666000490_20160302-macrogrupos-novo.xlsx", encoding = "UTF-8", 
                    sheetIndex = 1, header = TRUE)

    df_usu.nov <-
        df_usu.nov %>%
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
    df_usu.nov <-
        df_usu.nov %>%
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

    #df_tidy_hg <- f_tidy_scores_HG(df_usu.nov) # calcula os fatores de acordo com a puntuação

    # chama funcao que tira acentos e força minúsculos
    df_usu.nov <- f_acentos(df_usu.nov)

    # restaura fatores como numericos
    df_usu.nov <-
        df_usu.nov %>%
        mutate(sensibility = as.numeric(as.vector(sensibility)),
           power = as.numeric(as.vector(power)),
           quality = as.numeric(as.vector(quality)),
           exposure = as.numeric(as.vector(exposure)),
           structure = as.numeric(as.vector(structure)),
           imagination = as.numeric(as.vector(imagination)),
           stability = as.numeric(as.vector(stability)),
           contacts = as.numeric(as.vector(contacts)))

    # calculo dos novos fatores
    # obtendo os scores previstos de acordo com a análise de componentes principais
    ###############################################################################

    pca1 = prcomp(df_usu.nov[,9:16], scale. = TRUE, center = TRUE)
    # scores obtidos
    scores.nov<- as.data.frame(pca1$x)
    my.scores.nov <- as.data.frame(cbind(ID =  df_usu.nov$ID,
                                     nomerespondente = df_usu.nov$nomerespondente,
                                     sexo = df_usu.nov$sexo, 
                                     profissao = df_usu.nov$profissao, 
                                     formacao = df_usu.nov$formacao,
                                     scores.nov))
    # eliminando PC8
    my.scores.nov <-
        my.scores.nov %>%
        select (-PC8)

    # criar plot para  usuario selecionado novo
    # abaixo fazer uma vez só e gravar estes scores em arquivo

    # OBTENDO SCORES DE UMA PESSOA POR input nome
    ###############################################
    #my.score.limites$scores.resp <- scores.alex
    # selecionando somente linha do respondente desejado para gerar o plot
    df_PCs <- 
        my.scores.nov %>%
        filter(nomerespondente == in.nome) %>%
        select(PC1, PC2, PC3, PC4, PC5, PC6, PC7)
    
    # colocando scores do selecionado no data frame de limites de score
    my.score.limites$scores.resp <- as.numeric(df_PCs)
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
    plot.classif.nov <- ggplot(my.classif, aes(x=atributo, y=perc)) +
                        geom_bar(aes (fill= factor(intensidade)), stat="identity") +
                        scale_fill_manual(values=c("skyblue", "lightgreen", "yellow", "red1")) +
                        ggtitle(paste("Classificação de Respondente (", in.nome,")")) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #(plot.pessoa)
    # cria tabela para acompanhar o plot
    #tbl.pessoa <- tableGrob(my.classif[,c(1,4,6,7)], rows=NULL)
    # Plot chart and table into one object
    # grid.arrange(plot.pessoa, tbl.pessoa, nrow=2, as.table=TRUE, heights=c(1,1))

    # retorna plot de scores atuais do nome selecionado
    out.l_plots <- list(pl_ant = plot.classif.ant, pl_nov = plot.classif.nov)
    return(out.l_plots)
}

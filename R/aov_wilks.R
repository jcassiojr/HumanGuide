# analise de variança e de wilks
# TESTAR: manova usando uma coluna para cada média de PC1 para cada
# FUNCIONOU!!!
# TIPOUSER
source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
source("./R/f_tidy_scores_HG.R") # alterada em 07/12/2015 para trazer nome do respondente tb

registerDoMC(5) # parallel processing
#df_raw_hg <- f_le_raw_HG_ori() # retorna colunas: ID, sexo, pontos
df_raw_hg <- f_le_raw_HG() # retorna colunas: todas
df_tidy_hg <- f_tidy_scores_HG(df_raw_hg)
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
f_acentos <- function(df_in) { # funcao que tira os acentos e deixa tudo em minuscula
    df_in <- mutate_each(df_in, funs(tolower)) # forçando minúsculas
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("á", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("é", "e", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("í", "i", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ó", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ú", "u", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ã", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("õ", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ç", "c", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("â", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ê", "e", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ô", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("  ", " ", x))))
    return (df_in)
}
df_tidy_hg <- f_acentos(df_tidy_hg)
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

# obtendo os scores previstos de acordo com a análise de componentes principais
pca1 = prcomp(df_tidy_hg[,7:14], scale. = TRUE, center = TRUE)
# scores obtidos
scores.total <- as.data.frame(pca1$x)
my.scores.total <- as.data.frame(cbind(ID = df_tidy_hg$ID, 
                                       sexo = df_tidy_hg$sexo, 
                                       tipouser = df_tidy_hg$TIPOUSER, 
                                       profissao.na.area.de = df_tidy_hg$profissao.na.area.de, 
                                       formacao.em = df_tidy_hg$formacao.em,
                                       scores.total))
my.scores.total <-
    my.scores.total %>%
    mutate(tipouser = ifelse(tipouser == "", "indefinido", as.character(tipouser)),
           sexo = ifelse(sexo == "f", "feminino","masculino"))
my.scores.tipouser <- 
    my.scores.total %>%
    select(tipouser,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) %>%
    mutate(tipouser = as.factor(tipouser))
my.aov.tipouser <-
    my.scores.tipouser %>%
    filter(!(tipouser == "indefinido"))
m <- manova(cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) ~ as.factor(tipouser), data = my.aov.tipouser)
summary(m, test = "Wilks")

# WILKS
grp <- as.factor(my.aov.tipouser$tipouser)
x <- as.matrix(my.aov.tipouser[,2:9])
Wilks.test(x, grouping=grp, method="c")

#++++++++++++++++++++
# ONE WAY ANALYSIS
#+++++++++++++++++++++++
# a analise abaixo identifica se os grupos definidos em relação aos tipos de usuários 
# apresentam diferença nos seus scores médios em relação ao primeiro componente(PC1)
# os resultados de aplicação de anova analysis mostram que sim.
# repetir abaixo para sexo, ext, para todos os componentes e gerar tabela
# em seguida, rodar o Wilks para todos os grupos mas para todos os PCs tb
my.aov.tipouser <-
    my.scores.tipouser %>%
    filter(!(tipouser == "indefinido"))
#select(tipouser,PC1) 
#filter(tipouser == "func publico") %>%
#sample_n(15)
# AOV
# aplica analise de variancia para cada componente
aov.tipouser.PC1 = aov(PC1~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC1)
print(model.tables(aov.tipouser.PC1,"means"),digits=3)
boxplot(PC1~tipouser,data=my.aov.tipouser) #graphical summary appears in graphics window

aov.tipouser.PC2 = aov(PC2~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC2)
aov.tipouser.PC3 = aov(PC3~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC3)
aov.tipouser.PC4 = aov(PC4~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC4)
aov.tipouser.PC1 = aov(PC1~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC1)
aov.tipouser.PC5 = aov(PC5~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC5)
aov.tipouser.PC6 = aov(PC6~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC6)
aov.tipouser.PC7 = aov(PC7~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC7)
aov.tipouser.PC8 = aov(PC8~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC8)#show the summary table

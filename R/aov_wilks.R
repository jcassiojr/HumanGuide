# analise de variança e de wilks
require("doMC")
library("rrcov")
library("xlsx")
require("stringr")
require("reshape2")
source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
source("./R/f_tidy_scores_HG.R") # alterada em 07/12/2015 para trazer nome do respondente tb

registerDoMC(5) # parallel processing
#df_raw_hg <- f_le_raw_HG_ori() # retorna colunas: ID, sexo, pontos

# PREPARACAO DOS DADOS

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
# obtendo os scores previstos de acordo com a análise de componentes principais
# sem scale and center para ver se dá muita diferença (literatura diz que não)
#pca1 = prcomp(df_tidy_hg[,7:14], scale. = FALSE, center = FALSE)
# scores obtidos
scores.total <- as.data.frame(pca1$x)
my.scores.total <- as.data.frame(cbind(ID = df_tidy_hg$ID, 
                                       sexo = df_tidy_hg$sexo, 
                                       tipouser = df_tidy_hg$TIPOUSER,
                                       idade = as.numeric(df_tidy_hg$idade),
                                       cep = df_tidy_hg$cep,
                                       profissao.na.area.de = df_tidy_hg$profissao.na.area.de, 
                                       formacao.em = df_tidy_hg$formacao.em,
                                       scores.total))
my.scores.total <-
    my.scores.total %>%
    mutate(tipouser = ifelse(tipouser == "", "indefinido", as.character(tipouser)),
           sexo = ifelse(sexo == "f", "feminino","masculino"))

df_profs <- read.xlsx2("./data/profs.de.para-V1.xlsx", sheetIndex = 1, header = TRUE)
df_profs <- f_acentos(df_profs)

df_forms <- read.xlsx2("./data/forms.de.para-V1.xlsx", sheetIndex = 1, header = TRUE)
df_forms <- f_acentos(df_forms)

df_campos <- read.xlsx2("./data/Classificacao das carreiras-V2.xlsx", sheetIndex = 1, header = TRUE)
# preparando os dados para as pesquisas a frente
df_campos <- f_acentos(df_campos)

# renomeando colunas
df_forms <-
    df_forms %>%
    rename(formacao.em = DE)

# substituindo a coluna de formação antiga para a convertida na tabela principal
df_change <- left_join(my.scores.total, df_forms, by=c("formacao.em")) 
df_change <-
    df_change %>%
    select(-formacao.em) %>%
    rename(formacao.em = PARA)

# renomeando colunas
df_profs <-
    df_profs %>%
    rename(profissao.na.area.de = DE)

# substituindo a coluna de profissão antiga para a convertida na tabela principal
df_change <- left_join(df_change, df_profs, by=c("profissao.na.area.de")) 
df_change <-
    df_change %>%
    select(-profissao.na.area.de) %>%
    rename(profissao.na.area.de = PARA)

# eliminar formação e profissão marcadas como a "eliminar"
df_change <-
    df_change %>%
    filter(formacao.em != "eliminar" & profissao.na.area.de != "eliminar")

df_carr <- data.frame()

for (j in 1:ncol(df_campos)) {
    # percorre todas as linhas da coluna corrente de df_campos
    for (i in 1:length(df_change$profissao.na.area.de)) {
        # somente pega o string com match exato (ex. administração)
        i_aux <- sum(!is.na(str_match(df_campos[,j],
                                      paste0("^", as.character(df_change$profissao.na.area.de[i]), "$"))))
        
        # para cada profissão encontrada em uma determinada classe, marca a posição correspondente
        # com 1, na célula do dataframe correspondente. Caso contrario, coloca NA
        if(i_aux) {
            df_carr[i,j] = i_aux
        } else {
            df_carr[i,j] = NA
        }
    }
}
# colocando os nomes das classes no dataframe gerado
names(df_carr) <- colnames(df_campos)

# concatenando a coluna de profissoes ao dataframe gerado
my.prev.carr <- cbind(df_change, df_carr)

# transformando colunas em valores por linha, eliminando NAs, de forma a poder ter os scores de
# cada profissão duplicado para cada classe onde a profissão se encontra
# Desta forma, a análise não perde a contribuição do score de cada profissão para cada classe
carrMelt <- melt(my.prev.carr,id=c("ID","profissao.na.area.de",
                                   "PC1","PC2","PC3","PC4","PC5","PC6","PC7"),
                 measure.vars=colnames(df_campos), na.rm = TRUE)
# mudando o nome da variavel de classe
colnames(carrMelt)[10] <- "class.carr"
# eliminando coluna desnecessária
my.prev.carr <-
    carrMelt %>%
    select (-value)




#++++++++++++++++++++
# Wilks ANALYSIS
#+++++++++++++++++++++++
# Manova Wilks sobre grupo TIPO USER
#----------------------------------
my.scores.tipouser <- 
    my.scores.total %>%
    select(tipouser,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) %>%
    mutate(tipouser = as.factor(tipouser))

my.aov.tipouser <-
    my.scores.tipouser %>%
    filter(!(tipouser == "indefinido"))

m <- manova(cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) ~ tipouser, data = my.aov.tipouser)
summary(m, test = "Wilks")

# define vetores para gerar saida em data.frame

# imprime o valor do lambda de Wilks
#a <- "tipo de usuario" 
#w <- sprintf("Wilks' Lambda:  %f",summary(m, test = "Wilks")$stats[3])
#f <- sprintf("F-rate:  %f",summary(m, test = "Wilks")$stats[5])
#p <- sprintf("Pr(>F) ou P-value:  %e",summary(m, test = "Wilks")$stats[11])
df.stats.tipouser <- data.frame(agrupamento = "Tipo de Usuario",
                       Wilks.Lambda = summary(m, test = "Wilks")$stats[3],
                       F.rate = summary(m, test = "Wilks")$stats[5],
                       P.value = summary(m, test = "Wilks")$stats[11])
# WILKS (usado para confirmar)
#grp <- as.factor(my.aov.tipouser$tipouser)
#x <- as.matrix(my.aov.tipouser[,2:9])
#Wilks.test(x, grouping=grp, method="c")

# Manova Wilks sobre grupo SEXO
#----------------------------------
my.scores.sexo <- 
    my.scores.total %>%
    select(sexo,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) %>%
    mutate(sexo = as.factor(sexo))

m <- manova(cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) ~ sexo, data = my.scores.sexo)
summary(m, test = "Wilks")
df.stats.sexo <- data.frame(agrupamento = "Sexo",
                                Wilks.Lambda = summary(m, test = "Wilks")$stats[3],
                                F.rate = summary(m, test = "Wilks")$stats[5],
                                P.value = summary(m, test = "Wilks")$stats[11])
# Manova Wilks sobre grupo Faixa Etária
#----------------------------------
my.scores.fxetaria <- 
    my.scores.total %>%
    select(idade,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) %>%
    mutate(faixa.etaria = ifelse(idade <= 18, "adolescente",
                                 ifelse(idade > 18 & idade <= 29, "jovem adulto",
                                 ifelse(idade > 29 & idade <= 36, "adulto",
                                 "adulto maduro"))))

m <- manova(cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) ~ as.factor(faixa.etaria), data = my.scores.fxetaria)
summary(m, test = "Wilks")
df.stats.fxet <- data.frame(agrupamento = "Faixa Etária",
                            Wilks.Lambda = summary(m, test = "Wilks")$stats[3],
                            F.rate = summary(m, test = "Wilks")$stats[5],
                            P.value = summary(m, test = "Wilks")$stats[11])
# Manova Wilks sobre região de CEP
#----------------------------------
my.scores.cep <-
    my.scores.total %>%
    filter(!(cep == ""))

# separando as dimensões do CEP
my.scores.cep <-
    my.scores.cep %>%
    mutate(CEP.regiao = substring(cep,1,1),
           CEP.subregiao = substring(cep,1,2),
           CEP.setor = substring(cep,1,3),
           CEP.subsetor = substring(cep,1,4)
    )


m <- manova(cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) ~ as.factor(CEP.regiao), data = my.scores.cep)
summary(m, test = "Wilks")
df.stats.cep <- data.frame(agrupamento = "Região de CEP",
                            Wilks.Lambda = summary(m, test = "Wilks")$stats[3],
                            F.rate = summary(m, test = "Wilks")$stats[5],
                            P.value = summary(m, test = "Wilks")$stats[11])


# Manova Wilks sobre Campo Prof
#----------------------------------

m <- manova(cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7) ~ class.carr, data = my.prev.carr)
summary(m, test = "Wilks")
df.stats.cpoprf <- data.frame(agrupamento = "Campo Profissional",
                            Wilks.Lambda = summary(m, test = "Wilks")$stats[3],
                            F.rate = summary(m, test = "Wilks")$stats[5],
                            P.value = summary(m, test = "Wilks")$stats[11])

# agrupando em data frame todas as estatísticas
df.stats <- rbind(df.stats.cpoprf,df.stats.cep,df.stats.fxet,df.stats.sexo, df.stats.tipouser)
knitr::kable(df.stats, digits = 3,format = "html")
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

# AO FINAL FAZER ANALISE POR POS NEG em cada PC
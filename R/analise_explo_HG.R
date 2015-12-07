# Análise Exploratória
require("corrplot", quietly = TRUE, warn.conflicts = FALSE)
require("doMC", quietly = TRUE, warn.conflicts = FALSE)

#source("./R/f_le_raw_HG_ori.R") # usar esta função para ler os dados originais da tese. 
source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
source("./R/f_tidy_scores_HG.R")

registerDoMC(5) # parallel processing
# carga dos dados
df_raw_hg <- f_le_raw_HG() # retorna colunas: todas
df_tidy_hg <- f_tidy_scores_HG(df_raw_hg)



# criar data.frame com os scores obtidos para uma amostra

my.newdata <- 
    df_tidy_hg %>%
    sample_n(100000)

# obtém o modelo
pca1 = prcomp(df_tidy_hg[,4:11], scale. = TRUE, center = TRUE)

# calculando os scores
my.prev <- as.data.frame(predict(pca1, newdata=my.newdata))
my.prev <- cbind(my.newdata[,1:2], my.prev)

# criação das categorias
# montar dataframe para análise de cluster por TIPOUSER
# tipo: AMBEV, FUNC PUBLICO, HOMEM, MULHER)

# 1. obter as médias para cada PC x TIPOUSER
# 2. plotar score médio por TIPOUSR x PC
df_PCmean <- 
    my.prev %>%
    group_by(TIPOUSER) %>%
    summarise(PC1.MEAN = mean(PC1),
              PC2.MEAN = mean(PC2),
              PC3.MEAN = mean(PC3),
              PC4.MEAN = mean(PC4),
              PC5.MEAN = mean(PC5),
              PC6.MEAN = mean(PC6),
              PC7.MEAN = mean(PC7))
df_PCmean$COMP <- c("PC1", "PC2", "PC3", "PC4","PC5", "PC6","PC7")           
# USAR GGPLOT PARA PLOTAR LINHA POR TIPOUSER (Y) x PCs (x)

df_x <- data.frame (PC = c("PC1", "PC1", "PC2", "PC2"),
                    TIPOUSER = c("AMBEV", "AMBEV", "KROTON", "KROTON"),
                    meanPC = c(-.020393, -.32827, .1903, .2293))
bp <- ggplot(df_x, aes(x=PC, y=meanPC, fill=TIPOUSER)) +
    geom_bar(position=position_dodge(), stat="identity")
bp

# outro gráfico (linha)library(ggplot2)
# Basic line plot with points
df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
ggplot(data=df, aes(x=dose, y=len, group=1)) +
    geom_line()+
    geom_point()

# Change the line type
ggplot(data=df, aes(x=dose, y=len, group=1)) +
    geom_line(linetype = "dashed")+
    geom_point()

# Change the color
ggplot(data=df, aes(x=dose, y=len, group=1)) +
    geom_line(color="red")+
    geom_point()
#===========================


# Summarise number of movie ratings by year of movie
dat <- read.table(header=TRUE, text='
     cond group result hline
                  control     A     10     9
                  treatment     A   11.5    12
                  control     B     12     9
                  treatment     B     14    12
                  ')
dat
#>        cond group result hline
#> 1   control     A   10.0     9
#> 2 treatment     A   11.5    12
#> 3   control     B   12.0     9
#> 4 treatment     B   14.0    12

#>      PC TIPOUSER   meanPC    ???(Desvio padrão) ou usar smooth_geom como já fiz nos cursos
#> 1   PC1     AMBEV    10.0     9
#> 2   PC2     AMBEV    11.5    12
#> 3   PC1     KROTON   12.0     9
#> 4   PC2     KROTON   14.0    12
#> .... ... ... ... ... 

# Define basic bar plot
bp <- ggplot(dat, aes(x=cond, y=result, fill=group)) +
    geom_bar(position=position_dodge(), stat="identity")
bp

# The error bars get plotted over one another -- there are four but it looks
# like two
bp + geom_errorbar(aes(y=hline, ymax=hline, ymin=hline), linetype="dashed")

# Explo Boxplot of MPG (score) by Car Cylinders (PC) 
#---------------------------------------------------
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")

# Boxplot of MPG (score) by Car Cylinders (PC) 
#---------------------------------------------------
# para boxplot abaixo preciso fazer uma coluna com PC (obs=PC1, PC2, etc) e outra com o valor do score
# JA SEI: usar package reshape, 
df_prev.melted <- melt(my.prev, id=c("ID","TIPOUSER"), measured=c("PC1","PC2","PC3", "PC4", "PC5", "PC6", "PC7", "PC8"))

bp <- boxplot(value~variable,data=df_prev.melted, main="Scores por Componente", outline = FALSE,
        xlab="Componentes", ylab="Score")

# separar por TIPOUSER, um boxplot para cada
df_KROTON.melted <-
    df_prev.melted %>%
    filter(TIPOUSER == "KROTON")
bp <- boxplot(value~variable,data=df_KROTON.melted, main="Scores por Componente - KROTON", outline = FALSE,
              xlab="Componentes", ylab="Score")

df_AMBEV.melted <-
    df_prev.melted %>%
    filter(TIPOUSER == "AMBEV")
bp <- boxplot(value~variable,data=df_AMBEV.melted, main="Scores por Componente - AMBEV", outline = FALSE,
              xlab="Componentes", ylab="Score")

df_FPUB.melted <-
    df_prev.melted %>%
    filter(TIPOUSER == "FUNC PUBLICO")
bp <- boxplot(value~variable,data=df_FPUB.melted, main="Scores por Componente _ FUNC PUBLICO", outline = FALSE,
              xlab="Componentes", ylab="Score")
# histograma
hist(df_FPUB.melted$value)

# Alternativa interessante (criar uma density plot para cada componente. Criar um grafico para cada TIPOUSER)
# Compare MPG distributions for cars with 
# 4,6, or 8 cylinders
library(sm)
attach(mtcars)

# create value labels 
cyl.f <- factor(cyl, levels= c(4,6,8),
                labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 

# plot densities 
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)

# PARA HG
# TOTAL
# create value labels 
cyl.f <- factor(df_prev.melted$variable, levels= c("PC1","PC2","PC3", "PC4", "PC5", "PC6", "PC7", "PC8"),
                labels = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")) 
# plot densities 
sm.density.compare(df_prev.melted$value, df_prev.melted$variable, xlab="Score")
title(main="Distribuição de Scores por Componente- Total")
# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)

# AMBEV
# create value labels 
cyl.f <- factor(df_AMBEV.melted$variable, levels= c("PC1","PC2","PC3", "PC4", "PC5", "PC6", "PC7", "PC8"),
                labels = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")) 
# plot densities 
sm.density.compare(df_AMBEV.melted$value, df_AMBEV.melted$variable, xlab="Score")
title(main="Distribuição de Scores por Componente- AMBEV")
# KROTON
# create value labels 
cyl.f <- factor(df_KROTON.melted$variable, levels= c("PC1","PC2","PC3", "PC4", "PC5", "PC6", "PC7", "PC8"),
                labels = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")) 
# plot densities 
sm.density.compare(df_KROTON.melted$value, df_KROTON.melted$variable, xlab="Score")
title(main="Distribuição de Scores por Componente- KROTON")
# FUNC PUBLICO
# create value labels 
cyl.f <- factor(df_FPUB.melted$variable, levels= c("PC1","PC2","PC3", "PC4", "PC5", "PC6", "PC7", "PC8"),
                labels = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")) 
# plot densities 
sm.density.compare(df_FPUB.melted$value, df_FPUB.melted$variable, xlab="Score")
title(main="Distribuição de Scores por Componente- FUNC PUBLICO")

# outro tipo de plot
sm.density(df_KROTON.melted$value, model = "Normal")

#---------------------------------
# calculando scores para pessoas conhecidas na amostra
#---------------------------------

# obtendo dados brutos e filtrando para as pessoas selecionadas do arquivo rh99
df_rh99 <- read.csv2("./data/rh99_20151121_0002.csv", encoding = "UTF-8", 
                     sep = "\t", header = TRUE)
# elimina linhas com dados que estavam na coluna errada
df_rh99 <-
    df_rh99 %>%
    filter(TIPOUSER != "SP" & TIPOUSER != "DF")
# fatoriza TIPOUSER para uso posterior em funcoes
df_rh99$TIPOUSER <- factor(df_rh99$TIPOUSER)
# elimina coluna X
df_rh99 <-
    df_rh99 %>%
    select(-X) # eliminando coluna "X"
# obtem usuários selecinados
df_users <- df_rh99 %>%
    filter(grepl("Giselle Welter|Alex Welter|Marco Sinicco|Beatriz Welter|Eneko Fonseca|Ana Alterio|Almir Cozzolino|Fiama Ester de Oliveira|Valdir Rasche|Laura Welter|Sven Peters|Arlindo Marin", df_rh99$nomerespondente))
    #select (ID, nomerespondente)
# Não achei: Fiama Ester de Oliveira (Não está na planilha original), Marco Sinicco, Valdir Rasche, Sven Peters

# obtendo scores previstos destes usuários
my.newdata.users <- f_tidy_scores_HG(df_users)
#my.newdata.tidy <- df_tidy_hg[df_tidy_hg$ID %in% c(df_users$ID),]
#my.newdata.raw <- df_raw_hg[df_raw_hg$ID %in% c(df_users$ID),]

# obtendo os scores previstos
pca1 = prcomp(df_tidy_hg[,3:10], scale. = TRUE, center = TRUE)

# calculando os scores
my.prev.users <- as.data.frame(predict(pca1, newdata=my.newdata.users))
my.prev.users <- cbind(my.newdata.users[,1:3], my.prev.users)


#----------------------------------------
# ver o que faz abaixo (???)
xt2 <- xtabs(PC1 ~TIPOUSER, data=my.prev)

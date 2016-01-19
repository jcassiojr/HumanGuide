# algoritmo de comparação entre pessoa e campos
# usar my.prev.carr preparado em Rmd Análise Descritiva - Exploratoria
# DEPOIS TRANSFORMAR EM RMD
# obtendo os dados


# comprovando que a distribuição dos scores é aproximadamente normal, assim a média é ok para usar nas previsões
par(mfrow=c(2,4))
hist(df_change$PC1, main = "Histograma de Scores da Amostra (PC1)", xlab = "PC1")
hist(df_change$PC2, main = "Histograma de Scores da Amostra (PC2)", xlab = "PC1")
hist(df_change$PC3, main = "Histograma de Scores da Amostra (PC3)", xlab = "PC1")
hist(df_change$PC4, main = "Histograma da de Scores Amostra (PC4)", xlab = "PC1")
hist(df_change$PC5, main = "Histograma da de Scores Amostra (PC5)", xlab = "PC1")
hist(df_change$PC6, main = "Histograma da de Scores Amostra (PC6)", xlab = "PC1")
hist(df_change$PC7, main = "Histograma da de Scores Amostra (PC7)", xlab = "PC1")
   

require(Hmisc)
my.PC.Campo <-
    my.prev.carr %>%
    group_by(class.carr) %>%
    summarise(PC1.medio = mean(PC1),
              PC2.medio = mean(PC2),
              PC3.medio = mean(PC3),
              PC4.medio = mean(PC4),
              PC5.medio = mean(PC5),
              PC6.medio = mean(PC6),
              PC7.medio = mean(PC7))
# inserindo numero sequencial para representar os campos
my.PC.Campo <-
    my.PC.Campo %>%
    mutate(ID.campo = seq(nrow((my.PC.Campo))))
# para as medias transpostas    #AQUI
my.PC.Campo.t <- t(my.PC.Campo)
colnames(my.PC.Campo.t) <- my.PC.Campo.t[1,]

my.PC.Campo.t <- my.PC.Campo.t[-9,] # elimina colunas indesejadas
my.PC.Campo.t <- my.PC.Campo.t[-1,] # elimina colunas indesejadas
my.PC.Campo.t <- as.data.frame(my.PC.Campo.t)
my.PC.Campo.t <-
    my.PC.Campo.t %>%
    mutate(class.carr = rownames(my.PC.Campo.t))
my.PC.Campo.t <-
    my.PC.Campo.t %>%
    mutate(CFM = as.numeric(as.vector(CFM)),
           CFQ = as.numeric(as.vector(CFQ)),
           CCF = as.numeric(as.vector(CCF)),
           COA = as.numeric(as.vector(COA)),
           CJS = as.numeric(as.vector(CJS)),
           CCP = as.numeric(as.vector(CCP)),
           CSL = as.numeric(as.vector(CSL)),
           CMA = as.numeric(as.vector(CMA)),
           CCE = as.numeric(as.vector(CCE)),
           CBS = as.numeric(as.vector(CBS)))
# inserindo numero sequencial para representar os campos
my.PC.Campo.t <-
    my.PC.Campo.t %>%
    mutate(ID.PC = seq(nrow((my.PC.Campo.t))))


pc1 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CFM)) +
    #geom_pointrange(ymin = 0.01, ymax = -0.01) +
    #geom_errorbar(ymin = 0.01, ymax = -0.01) +
    #geom_smooth(method="loess") +
    geom_line() +
    ggtitle("CFM x PC médio") 

# correlações e plots

# 2. aplicar correlaçao e plots abaixo para comparar os campo sde acordo com sua assinatura de Componentes
# primeiro comparando todos com CCF (abaixo) Repetir para os demais
# obs. depois colocar todos como no grid de correlacao da Reducao de dados!!!!
pc1 <- qplot(CFM,CFQ, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CFQ)
pc2 <- qplot(CFM,CCF, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CCF)
pc3 <- qplot(CFM,COA, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$COA)
pc4 <- qplot(CFM,CJS, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CJS)
pc5 <- qplot(CFM,CCP, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CCP)
pc6 <- qplot(CFM,CSL, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CSL)
pc7 <- qplot(CFM,CMA, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CMA)
pc8 <- qplot(CFM,CMA, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CMA)
pc9 <- qplot(CFM,CBS, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CBS)

multiplot(pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, cols=2)   
# outro
my.cor <- cor(my.PC.Campo.t[,1:10])
corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)

# 2. inserir a coluna da pessoa no dataframe my.PC.Campo.t
# inserir mais uma coluna para pessoa e colocar no gráfico acima???
Alex <- c(-2.296264822,-1.735027078,2.862688958,-2.022102719,-0.256122419,-1.251091201,-0.06664273)
Arlindo <- c(6.215298989, -0.179402156,2.192249661,-0.629228456,1.445510203,0.721397732, -0.934846735)
Laura <- c(-0.880558127,-0.310320799,1.41442788,-3.047761097,-0.700773651,1.72406918,-0.082153033)
Beatriz <- c(1.967845169,-0.024716848,0.972965178,2.217476245,-0.164333631,-1.748897734,-0.433052634)
Eneko <- c(1.327869429,-1.82247322,2.158161567,1.257910708,2.418553145,-1.472161385,-0.535689283)
my.PC.Campo.t$alex <- Alex
my.PC.Campo.t$arlindo <- Arlindo
my.PC.Campo.t$beatriz <- Beatriz
my.PC.Campo.t$laura <- Laura
my.PC.Campo.t$eneko <- Eneko

# outras maneiras
# 1. como cada pessoa se correlaciona com os componentes dos campos profissionais
# Alex
my.cor <- cor(my.PC.Campo.t[,c(1:10,13)])
corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)
#corrplot(my.cor, method = "color", addCoef.col="grey", order = "AOE")
# pegar as correlações de Alex com cada componente. 
# aplicar o critério definido com Giselle:
# - > 30% = marcante
# - 15%-30% = moderado
# - 5%-15% = fraco
# - 0%-5% = inexpressivo
# criar coluna no alex.cor com Alex para a classificação do componente
alex.corr <- my.cor[1:10,11] # falta inserir coluna com nomes
alex.corr.df <- data.frame(corr = alex.corr, campo = colnames(my.cor)[-11])
alex.corr <-
    alex.corr %>%
    mutate (forca.cor)
<PAREI AQUI: criar a formula para alimentar com string da forca.cor (ex. "marcante", etc)

# Arlindo
my.cor <- cor(my.PC.Campo.t[,c(1:10,14)])
corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)
# Beatriz
my.cor <- cor(my.PC.Campo.t[,c(1:10,15)])
corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)
# Laura
my.cor <- cor(my.PC.Campo.t[,c(1:10,16)])
corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)
# Eneko
my.cor <- cor(my.PC.Campo.t[,c(1:10,17)])
corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)

# comparação entre pessoas
my.cor <- cor(my.PC.Campo.t[,c(13:17)])
#corrplot.mixed(my.cor, insig = "p-value", lower="ellipse",sig.level = -1, is.corr = TRUE)
corrplot.mixed(my.cor, insig = "p-value",sig.level = -1, is.corr = TRUE)



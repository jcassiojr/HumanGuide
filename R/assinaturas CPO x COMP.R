# ABAIXO USAR PARA COMPARAR QUAIS CAMPOS TEM DISTRIBUICAO SEMELHANTE DE SCORES POR PC
#-------------------------------------------------------------------------------------
# FALTA INSERIR AQUI CODICO ATE OBTER my.prev.carr (ver em teste.R)

# usar my.prev.carr preparado em Rmd Análise Descritiva - Exploratoria
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
# ABAIXO SERÁ A ANÁLISE
# teste de grafico de correlacao entre CAMPO e PESSOA
# inserir no dataframe my.Campo.t a linha da pessoa!!!!!!
qplot(CFM,CFQ, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CFQ)
qplot(CFM,CCF, data= my.PC.Campo.t, geom=c("smooth"),method="lm")
cor(my.PC.Campo.t$CFM,my.PC.Campo.t$CCF)

#pc2 <- ggplot(my.PC.Campo.t, aes(x=class.carr, y=CFQ)) +
pc2 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CFQ)) +
    geom_line() +
    ggtitle("CFQ x PC médio")
pc3 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CCF)) +
    geom_line() +
    ggtitle("CCF x PC médio")
pc4 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=COA)) +
    geom_line() +
    ggtitle("COA x PC médio")
pc5 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CJS)) +
    geom_line() +
    ggtitle("CJS x PC médio")
pc6 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CCP)) +
    geom_line() +
    ggtitle("CCP x PC1 médio")
pc7 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CSL)) +
    geom_line() +
    ggtitle("CSL x PC1 médio")
pc8 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CMA)) +
    geom_line() +
    ggtitle("CMA x PC médio")
pc9 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CCE)) +
    geom_line() +
    ggtitle("CCE x PC médio")
pc10 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CBS)) +
    geom_line() +
    ggtitle("CBS x PC médio")

multiplot(pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, cols=2)   

# heatmap
heatmap(as.matrix(my.PC.Campo[,2:8]), Colv = my.PC.Campo$class.carr) #AQUI
#heatmap(as.matrix(my.PC.Campo.t[,1:10]), Colv = my.PC.Campo.t$class.carr) #AQUI

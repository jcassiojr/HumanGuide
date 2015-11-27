#'função de análise de componentes
#'
library(ggplot2)
library(gclus)
library(Hmisc)
require("corrplot", quietly = TRUE, warn.conflicts = FALSE)
require("FactoMineR")
library(psych)

f_pca_HG <- function(df_in) {
    
    #-----------
    # ANÁLISE 1 - Estatísticas básicas
    #-----------
    my.descr <- describe(df_in[,3:10])
    my.sumar <- summary(df_in[,3:10])
    
    #-----------
    # ANÁLISE 2 - correlações
    # importante remover variáveis perfeitamente correlacionadas
    # correlações acima de 0.3 são bons indicadores de que obteremos resultados 
    #-----------
    # obs: colocar aqui cores para diferenciar sexo (falta carregar nos dados)
    # colocar no documento de saída
    pairs(df_in[,3:10], main = "Matriz de Dispersão (feminino: green, masculino: red)", 
          pch=21, bg=c("green3","red")[unclass(df_in$sexo)])
    # correlação de spearman
    my.cor <- cor(df_in[,3:10], method = "spearman")
    print(my.cor, digits = 4)
    
    # Agrupamentos aparentes das correlações na análise dos dados originais: 
    # G1: structure, sensibility, power
    # G2: exposure, contacts, imagination e stability
    
    # ANÁLISE 3: Cluster analysis
    #+++++++++++++++++++
    ic <- iclust(my.cor, nclusters = 4) # usa Pearson correlation
    summary(ic)
    print(ic)
    
    # Agrupamentos aparentes na análise de clusters dos dados originais: 
    # G1: structure e power
    # G2: sensibility e quality
    # G3: exposure e contacts
    # G4: imagination e stability
    
    ## Compare this to
    #my.corrS <- cor.test(df_in[,3:10], method = "spearm", alternative = "g")
    #my.cor1 <- rcorr(as.matrix(df_in[,3:10]),type=c("spearman"))
    
    # colocando na ordem por componente principal
    #ord <- corrMatOrder(my.cor, order="FPC")
    #my.cor2 <- my.cor[ord,ord]
    #par (ask=TRUE)
    #corrplot.mixed(my.cor, main = "Correlação Ordenada por Componentes Principais")
    # The P value answers this question:
    # If there really is no correlation between X and Y overall,
    # what is the chance that random sampling would result in a
    # correlation coefficient as far from zero (or further) as
    # observed in this experiment?
    #corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE,
    #               bg = "white", addgrid.col = "gray",
    #               lower="ellipse", upper="circle")
    # salvar como PNG
    
    #+++++++++++++++++++++
    # Factor Analysis
    
    # Maximum Likelihood Factor Analysis
    # entering raw data and extracting 3 factors, 
    # with varimax rotation 
    #fit <- factanal(as.matrix(df_in[,3:10]), factors = 3, rotation="varimax")
    #print(fit, digits=2, cutoff=.3, sort=TRUE)
    # plot factor 1 by factor 2 
    #load <- fit$loadings[,1:2] 
    #plot(load,type="n") # set up plot 
    #text(load,labels=names(mydata),cex=.7) # add variable names
    
    #v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
    #v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
    #v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
    #v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
    #v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
    #v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
    #m1 <- cbind(v1,v2,v3,v4,v5,v6)
    #cor(m1)
    #factanal(m1, factors = 3) # varimax is the default
    #factanal(m1, factors = 3, rotation = "promax")
    # The following shows the g factor as PC1
    #prcomp(m1) # signs may depend on platform
    
    # package psych para factor analysis (somente para comparar. Será usada a PCA)
    #library(psych)
    #library(GPArotation)
    #library(matrixStats)
    # m1 <- as.matrix(df_in[,3:10])
    #my.fa <- fa(r=my.cor, nfactors=7, rotate="varimax", n.obs = 2119)
    #print(my.fa,digits = 4)
    # eingenvalues para identificar até que componente usa (Usando Kaiser, até o MR4)
    #print(my.fa$values, digits = 4)
    
    #print(my.fa$loadings, digits = 4)

 
    # ANÁLISE 4 - Principal Component Analysis (PCA)
    # extrair fatores maiores que 1 (Kaiser)
    #-----------------
    pca1 = prcomp(df_in[,3:10], scale = TRUE, center = TRUE) # analise para os 8 fatores
    pca2 = princomp(df_in[,3:10], cor = TRUE)
    pca3 = PCA(df_in[,3:10], graph = TRUE)
    # talvez escolher abaixo, mas com varimax, desde q consig aobter os scores
    pca4 <- principal(my.cor,nfactors = 5,rotate="none", scores = TRUE) # sem rotation bate com os demais acima!!!!
    
    # análise de cluster (alternativa à Factor Analysis)
    # correlacoes entre variáveis refletem que cada item carrega em no máximo um cluster
    # e itens que participam de um cluster se correlacionam pelos seus loadings
    
    
    # any two itens or clusters are joined together into a single new cluster
    # if and only if the coefficient alpha ans coefficient beta for the new cluster 
    # exceeded the average coefficient alpha and coefficient beta of the two
    # separate itens (or clusters) being considered for merging
    # isso assegura que os itens somente serão clusterizados se tiverem maior
    # homogeneidade fatorial
    
    # Very Simple Structure Analysis
    #+++++++++++++++++++++
    #my.vss <- vss(my.cor,title="Very Simple Structure of Human Guide")
    #print(my.vss)
    
    # sqrt of eigenvalues (Standard Deviations)
    # por kaisen , extrai os 5 primeiros componentes dos dados originais para os 4 casos abaixo
    #---------------------
    pca1$sdev 
    pca2$sdev
    sqrt(pca3$eig$eigenvalue)
    sqrt(pca4$values) # apresenta ligeira diferença em relação as abordagens acima
    
    # loadings or rotations: cada compontnes é uma combinação
    #---------------------
    # linear das variáveis (HG quantions). Aqui estão os coeficientes chamados 
    # loadings ou rotations (indicam o quanto cada variável está correlacionada
    # com o componentes especifico, nas colunas)
    pca1$rotation
    # Assim vemos quais fatores têm maior contribuição nos componentes)
    load <- pca1$rotation
    sorted.loadings = load[order(load[,1]),1]
    Main="Loadings plot for PC1"
    xlabs = "Variable Loadings"
    dotplot(sorted.loadings, main = Main, xlab = xlabs)
    
    pca2$loadings
    pca3$var$coord # PCA já escolhe somente os 5 primeiros componentes
    pca4$loadings # escolhi 5 primeiros componentes. Apresenta ligeira diferença em relação as abordagens acima
    
    # plotando os pontos previstos no biplot e no círculo de correlações
    # na plotagem fica claro o caráter oposto do agrupamento pela análise das correlações
    #-------------------------------------
    biplot(pca1, xlabs = rep(".", nrow(pca1$x)))
    
    # círculo de correlações por par de componentes
    #--------------------------
    circle <- function(center = c(0, 0), npoints = 100) {
        r <- 1
        tt <-  seq(0, 2 * pi, length = npoints) 
        xx <- center[1] + r * cos(tt)
        yy <-  center[1] + r * sin(tt) 
        return(data.frame(x = xx, y = yy))
    }
    corcir = circle(c(0, 0), npoints = 100)
    # create data frame with correlations between variables and PCs
    #correlations = as.data.frame(cor(df_in[,2:9], pca1$x))
    correlations = as.data.frame(pca1$rotation)
    # data frame with arrows coordinates
    arrows <- data.frame(x1 = c(0, 0, 0, 0), y1 = c(0, 0, 0, 0), x2 = correlations$PC1,
                         y2 = correlations$PC2)
    # geom_path will do open circles
    ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") +
        geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") +
        geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) +
        geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0,
                                                                   colour = "gray65") + 
        xlim(-1.1, 1.1) +
        ylim(-1.1, 1.1) +
        labs(x = "pc1 aixs", y = "pc2 axis") + ggtitle("Circle of correlations")
    
    # PCs (aka scores)
    #----------------------
    head(pca1$x)
    head(pca2$scores)
    head(pca3$ind$coord) # aqui já aplica o critério de Kaiser e só mostra os 5 comp princ
    #head(pca4$r.scores) # NÃO ESTÁ FUNCIONANDO!!!!
    # total de variância acumulada antes de aplicar viramax
    #------------------------------------------------------------------
    # resultados muito similares aos da tese (tabela 22 - após viramax)
    summary(pca1)
    summary(pca2)
    summary(pca3)
    summary(pca4)
    #my.importance <- summary(pca1)$importance # salvar como matriz
    #print(my.importance, digits = 4)
    
    # eigenvalues - para saber quantos componentes a manter
    #-------------------------------------------------------------------
    # primeiro critério: usando Kayser criterion: manter eingevalues maior 
    # que 1. 
    # Neste caso, manter até o componente 5
    my.kayser.crit <- pca1$sdev ^ 2
    # segundo critério: usando scree plot
    screeplot(pca1, main = "Scree Plot - Human Guide", xlab = "Components")
    #screeplot(pca2, main = "Scree Plot - Human Guide", xlab = "Components")
    
    # ou com linhas
    # Neste caso, manter até o componente 7
    screeplot(pca1, main = "Scree Plot - Human Guide", type = "lines")
    #screeplot(pca2, main = "Scree Plot - Human Guide", type = "lines")

    # ANÁLISE 5 - Aplicando rotação Varimax para interpretar os componentes
    #-----------------------------------------------------------------------
    # aplicando Varimax para os 5 primeiros componentes
    # Apesar dos dados sugerirem 5 componentes vou usar os 3 originais da tese
    # Varimax rotation (change of coordinates
    # that maximizes the sum of the variances of the squared loadings)
    varimax3 <- varimax(pca1$rotation[,1:3], normalize = TRUE)
    print (varimax3$loadings)
    #my.var_load <- as.data.frame(varimax3$loadings[1:8,])
    my.var_load <- varimax3$loadings[1:8,]
    
    
    my.cutoff <- .1 # valor mínimo para aparecer na tabela de loadings
    print(loadings(varimax3),cutoff=my.cutoff)
    
    # interpretação dos loadings após rotação
    #-----------------
    # PC1: imagination e quality alto, stability baixo
    # PC2: contacts alto, exposure baixo
    # PC3: structure alto, power baixo
    # plotar 3D nos 3 fatores (1 eixo por componente) os 8 fatores
    # de forma a interpretar como se distribuem nos 3 fatores
    # usar data frame de loadinsgs de varimax
    
    #library(rgl)
    #plot3d(my.var_load, type = "s")  # great
    
    loadings <- data.frame(my.var_load, .names = row.names(pca1$rotation))
    theta <- seq(0,2*pi,length.out = 100)
    circle <- data.frame(x = cos(theta), y = sin(theta))
    # plot após rotação PC1 x PC2
    #----------------------------
    p <- ggplot(circle,aes(x,y)) + geom_path()
    p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = "a", colour = .names)) +
    coord_fixed(ratio=1) +
    labs(x = "PC1", y = "PC2")
    
    # plot após rotação PC2 x PC3
    #----------------------------
    p <- ggplot(circle,aes(x,y)) + geom_path()
    p + geom_text(data=loadings, 
                  mapping=aes(x = PC2, y = PC3, label = "a", colour = .names)) +
        coord_fixed(ratio=1) +
        labs(x = "PC2", y = "PC3")
    
    # plot após rotação PC1 x PC3
    #----------------------------
    p <- ggplot(circle,aes(x,y)) + geom_path()
    p + geom_text(data=loadings, 
                  mapping=aes(x = PC1, y = PC3, label = "a", colour = .names)) +
        coord_fixed(ratio=1) +
        labs(x = "PC1", y = "PC3")
    
    # ou em 3D (Base PC3)
    library(scatterplot3d)
    df_varload <- as.data.frame(my.var_load)
    with(df_varload, {
        s3d <- scatterplot3d(PC1, PC2, PC3,        # x y and z axis
                             color="blue", pch=19,        # filled blue circles
                             type="h",                    # vertical lines to the x-y plane
                             main="3-D Component Plot in Rotated Space (Base PC3)",
                             xlab="PC1",
                             ylab="PC2",
                             zlab="PC3")
        s3d.coords <- s3d$xyz.convert(PC1, PC2, PC3) # convert 3D coords to 2D projection
        text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
             labels=row.names(df_varload),               # text to plot
             cex=.5, pos=4)           # shrink text 50% and place to right of points)
    })
    # ou em 3D (Base PC2)
   

    with(df_varload, {
        s3d <- scatterplot3d(PC3, PC1, PC2,         # x y and z axis
                             color="blue", pch=19,        # filled blue circles
                             type="h",                    # vertical lines to the x-y plane
                             main="3-D Component Plot in Rotated Space (Base PC3)",
                             xlab="PC3",
                             ylab="PC1",
                             zlab="PC2")
        s3d.coords <- s3d$xyz.convert(PC3, PC1, PC2) # convert 3D coords to 2D projection
        text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
             labels=row.names(df_varload),               # text to plot
             cex=.5, pos=4)           # shrink text 50% and place to right of points)
    })
    # ou em 3D (Base PC1)
 
    with(df_varload, {
        s3d <- scatterplot3d(PC2, PC3, PC1,         # x y and z axis
                             color="blue", pch=19,        # filled blue circles
                             type="h",                    # vertical lines to the x-y plane
                             main="3-D Component Plot in Rotated Space (Base PC3)",
                             xlab="PC2",
                             ylab="PC3",
                             zlab="PC1")
        s3d.coords <- s3d$xyz.convert(PC2, PC3, PC1) # convert 3D coords to 2D projection
        text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
             labels=row.names(df_varload),               # text to plot
             cex=.5, pos=4)           # shrink text 50% and place to right of points)
    })
    

    
    # ANÁLISE 6 - PREVISÕES VIA SCORES
    #----------------------------------
    # scores obtidos antes da previsão (my.prev) e coeficientes após rotação (varimax3$loadings)
    # Ex. PC1 = 
    my.newdata <- head(df_in[,c(1,3:10)])
    my.prev <- as.data.frame(predict(pca1, newdata=my.newdata))
    # obtendo previsao dos 3 primeiros componentes
    my.prev <- cbind(my.newdata, PC1 = my.prev$PC1, PC2 = my.prev$PC2, PC3 = my.prev$PC3)

    # A PARTIR DAQUI IMPLEMENTAREMOS O ALGORITMO QUE CLASSIFICARÁ O CANDIDATO
    # DE ACORDO COM O SCORE OBTIDO ACIMA
    
    
    
    
    
    
    
    
    
    
    
    # ATE AQUI
    
    
    
    
    
    #varimax3_2 <- varimax(pca2$loadings[,1:4], normalize = TRUE)
    
    # salvando matriz de loadings/rotations
    #my_var.load <- varimax3_1$loadings[1:8,]
    
    #my.cutoff <- .1 # valor mínimo para aparecer na tabela de loadings
    #print(loadings(varimax4),cutoff=my.cutoff)

    # plot dos circulos para PCA
    # circle of correlations
    #circle <- function(center = c(0, 0), npoints = 100) {
    #    r <- 1
    #    tt <-  seq(0, 2 * pi, length = npoints) 
    #    xx <- center[1] + r * cos(tt)
    #    yy <-  center[1] + r * sin(tt) 
    #    return(data.frame(x = xx, y = yy))
    #}
    #corcir = circle(c(0, 0), npoints = 100)
    # create data frame with correlations between variables and PCs
    #correlations = as.data.frame(cor(df_in[,2:9], pca1$x))
    #correlations = as.data.frame(pca1$rotation)
    # data frame with arrows coordinates
    #arrows <- data.frame(x1 = c(0, 0, 0, 0), y1 = c(0, 0, 0, 0), x2 = correlations$PC1,
    #                     y2 = correlations$PC2)
    # geom_path will do open circles
    #ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") +
    #    geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") +
    #    geom_text(data = correlations, aes(x = PC1, y = PC2, label = row.names(pca1$rotation), colour = row.names(pca1$rotation))) +
    #    geom_hline(yintercept = 0, colour = "gray65") + 
    #    geom_vline(xintercept = 0,colour = "gray65") + 
    #    xlim(-1.1, 1.1) +
    #    ylim(-1.1, 1.1) +
    #    labs(x = "pc1 aixs", y = "pc2 axis") + ggtitle("Circle of correlations")
    
    # ideia: criar no shiny esta tabela onde permite mudar
    # número de componentes

    # examinando as rotações (ou loadings) usando dotplot (antes de Varimax)
    # para cada componente. Ex. PC1
    # Assim vemos quais fatores têm maior contribuição nos componentes)
    #load <- pca1$rotation
    #sorted.loadings = load[order(load[,1]),1]
    #Main="Loadings plot for PC1"
    #xlabs = "Variable Loadings"
    #dotplot(sorted.loadings, main = Main, xlab = xlabs)
    
    #load <- varimax4$loadings
    #sorted.loadings = load[order(load[,1]),1]
    #Main="Loadings plot for PC1"
    #xlabs = "Variable Loadings"
    #dotplot(sorted.loadings, main = Main, xlab = xlabs)
    # examinando para os componentes, observamos que os 4 primeiros componentes
    # tem maior contribuição
    # Então usaremos eles para aplicar varimax
    
    
    # usando biplot (obs: usar nos 8 fatores!!)
    # o cosseno entre os vetores representa a correlação entre os mesmos
    # variáveis (s11, etc.) como vetores e observações como pontos
    # observando os pontos distribuídos no eixo (componente), podemos
    # ver quais são relacionados direta ou indiretamente
    

    #biplot(pca1, cex = c(1,07))
    # abaixo funciona mas demora alguns minutos
    #biplot(pca1, xlabs = rep(".", nrow(scores)))
    
    
    
    #  
    # obtendo matriz de loadings
    #mt_rot <- varimax4$loadings[1:72,1:4]
    # examinando as rotações (ou loadings) usando dotplot (após de Varimax)
    #load <- varimax4$loadings
    #sorted.loadings = load[order(load[,1]),1]
    #Main="Loadings plot for PC1"
    #xlabs = "Variable Loadings"
    #dotplot(sorted.loadings, main = Main, xlab = xlabs)
    
    
    
    

    # plot of observations
    # load ggplot2

    # create data frame with scores (prcomp chama score de "x")
    # pegando somente os primeiros pois sõa muitos dados
    scores = head(as.data.frame(pca1$x), n = 2119)
    #scores = as.data.frame(pca3$ind$coord)
    # plot of observations
    #ggplot(data = scores, aes(x = Dim.1, y = Dim.2, label = rownames(scores))) +
    ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
        geom_hline(yintercept = 0, colour = "gray65") +
        geom_vline(xintercept = 0, colour = "gray65") +
        geom_text(colour = "tomato", alpha = 0.8, size = 4) +
        ggtitle("PCA plot of Human Guide - Fatores") 
    
    
    # ACIMA PLOTA SCORES de componentes para ver sua distribuicão
    # deve estar em torno de zero (normalizado)
    
    
    # circle of correlations
    #circle <- function(center = c(0, 0), npoints = 100) {
    #   r <- 1
    #    tt <-  seq(0, 2 * pi, length = npoints) 
    #    xx <- center[1] + r * cos(tt)
    ##    yy <-  center[1] + r * sin(tt) 
    #    return(data.frame(x = xx, y = yy))
    #}
    #corcir = circle(c(0, 0), npoints = 100)
    # create data frame with correlations between variables and PCs
    #correlations = as.data.frame(cor(df_in[,2:9], pca1$x))
    # data frame with arrows coordinates
    #arrows <- data.frame(x1 = c(0, 0, 0, 0), y1 = c(0, 0, 0, 0), x2 = correlations$PC1,
    #                     y2 = correlations$PC2)
    # geom_path will do open circles
    #ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") +
    #    geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") +
    #    geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) +
    #    geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0,
    #                                                               colour = "gray65") + 
    #    xlim(-1.1, 1.1) +
    #    ylim(-1.1, 1.1) +
    #    labs(x = "pc1 aixs", y = "pc2 axis") + ggtitle("Circle of correlations")
    
    # salvar os gráficos acima para cada par de componentes (para 4 componentes)
    # ANÁLISE TERMINA AQUI. PARA BAIXO SÃO ALTERNATIVAS
    
    # scores só dos primeiros 10.000 obs
    # plot(pca3)
    
    #plot(pca1, type = "l")
    #summary(pca1)
    
    # predizendo scores de nova amostra
    # IDEIA: plotar no circle plot estes números!!!
    my.prev <- predict(pca1, newdata=tail(df_in[,3:10], 5))
    
    # ploting as previsões
    
    ggplot(data = as.data.frame(my.prev), aes(x = PC1, y = PC2, label = rownames(my.prev))) +
        geom_hline(yintercept = 0, colour = "gray65") +
        geom_vline(xintercept = 0, colour = "gray65") +
        geom_text(colour = "tomato", alpha = 0.8, size = 4) +
        ggtitle("PCA plot of Human Guide - Fatores") 
    
    # para predizer aplico os coeficientes obtidos nos loadings (confirmar se
    # Componente Score Coefficiente MAtrix é o mesmo ue loadings/rotation)
    # ex. para pca1$rotation PC1 = (-0.5297 * score de sensibilidade obtido de prevision acima) +
    # 0.0975 * pontuação de power ...) + .....
    # obtemos um valor
    
    # USO DO score: calculo os scores como acima para cada componente do candidato, obtendo como ele pontua em cada!!
    # colocar algoritmo aqui!!!
    
    
    
    
    
    
    
    # ++++++++++++++++++++++++++++++

    # ROTATION TO GIVE MEANING TO THE FACTORS ??!! (Varimax!!)
    # ver texto de Robin Beaumont,An introduction to Principal Component Analysys &
    # Factor Analysys, pg 9
    
    # NAO VOU APLCIAR VARIMAX POIS DÁ VALORES DIFERENTES PARA CADA TECNICA PCA
   # varimax4 <- varimax(pca1$rotation[,1:4], normalize = TRUE)

    
    # salvando matriz de loadings/rotations
    #my_var.load <- varimax3_1$loadings[1:8,]
    
    #my.cutoff <- .1 # valor mínimo para aparecer na tabela de loadings
    #print(loadings(varimax4),cutoff=my.cutoff)
    
    # obtendo os scores para cada individuo após varimax???
    # USO: a partir dos scores, identifico onde candidato pontua em cada um
    
    
    
    
    
    
    
    
    #    TERMINA AQUI   
    #++++++++++++++++++++++++++++
    
    
    
    
    
    
    # plot

    
    #theta <- seq(0,2*pi,length.out = 100)
    #circle <- data.frame(x = cos(theta), y = sin(theta))
    #p <- ggplot(circle,aes(x,y)) + geom_path()
    
    #loadings <- data.frame(pca1$rotation, 
    #                       .names = row.names(pca1$rotation))
    #p + geom_text(data=loadings, 
    #              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
    #    coord_fixed(ratio=1) +
    #    labs(x = "PC1", y = "PC2")
    
    # usando caret
    require(caret)
    trans = preProcess(df_in[,3:10], 
                       method=c("BoxCox", "center", 
                                "scale", "pca"))
    
    PC = predict(trans, df_in[,3:10])
    # Retained PCs
    head(PC, 3)
    # Loadings
    trans$rotation   
    #plot de correlação
    #require("corrplot")
    #descrCorr <- cor(df_in[,2:9])
    # plotando com p-value para cada correlação
    # interessante para ver correlação entre os componentes!!
    #corrplot.mixed(descrCorr, insig = "p-value",sig.level = -1)
    
    # passar para aqui o gráficos 3D abaixo para teste
    # ver se vale a pena usar caret (comparar valores com prcomp para ver se batem)
    # plot
    library(rgl)
    scores = as.data.frame(head(pca1$x, n = 100))
    pairs(scores[1:3])
    plot3d(scores[1:3])  # great
    
    # colorido (usar este) 
    library(mclust)
    fit <- Mclust(scores[1:4], G=4, modelNames = "EEV")
    plot3d(scores[1:4], col = fit$classification) 
    
    
    
    
 
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # scree plot
    #screeplot(pca1, type = "lines")
    #screeplot(pca1, type = "lines")
    #-----------------------------------
    # Melhor opção
    # PCA with function PCA
    #-----------------------------------
    library(FactoMineR)
    # apply PCA
    pca3 = PCA(head(df_in[,2:9], n = 10000), ncp = 3, graph = TRUE)
    # matrix with eigenvalues
    pca3$eig
    # correlations between variables and PCs
    pca3$var$coord
    # PCs (aka scores)
    head(pca3$ind$coord)
    
    #res.pca <- PCA(head(df_in[,2:9], n = 10000))
    #barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
    #summary(res.pca)
    
    #-------------------------
    # another with iris
    #-------------------------
    # Load data
    data(iris)
    head(iris, 3)
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    
    # apply PCA - scale. = TRUE is highly 
    # advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,
                     center = TRUE,
                     scale. = TRUE) 
    # print method
    print(ir.pca)
    # plot method (quantos componentes reter!!)
    plot(ir.pca, type = "l")
    
    # summary method (os primeiros 2 componentes respondem por 95.99% da variação)
    summary(ir.pca)
    # We can use the predict function if we observe new data and want to predict
    # their PCs values. Just for illustration pretend the last two rows of the 
    # iris data has just arrived and we want to see what is their PCs values:
    # Predict PCs scores
    predict(ir.pca, newdata=tail(log.ir, 2))
    
    # plotting
    library(devtools)
    install_github("ggbiplot", "vqv")
    
    library(ggbiplot)
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                  groups = ir.species, ellipse = TRUE, 
                  circle = TRUE)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', 
                   legend.position = 'top')
    print(g)
    # plot
    require(ggplot2)
    
    theta <- seq(0,2*pi,length.out = 100)
    circle <- data.frame(x = cos(theta), y = sin(theta))
    p <- ggplot(circle,aes(x,y)) + geom_path()
    
    loadings <- data.frame(ir.pca$rotation, 
                           .names = row.names(ir.pca$rotation))
    p + geom_text(data=loadings, 
                  mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
        coord_fixed(ratio=1) +
        labs(x = "PC1", y = "PC2")
    
    # As I mentioned before, it is possible to first apply a Box-Cox transformation
    # to correct for skewness, center and scale each variable and then apply PCA 
    # in one call to the preProcess function of the caret package.
    # By default, the function keeps only the PCs that are necessary to explain 
    # at least 95% of the variability in the data, but this can be changed through
    # the argument thresh.
    
    require(caret)
    trans = preProcess(iris[,1:4], 
                       method=c("BoxCox", "center", 
                                "scale", "pca"))
    PC = predict(trans, iris[,1:4])
    # Retained PCs
    head(PC, 3)
    # Loadings
    trans$rotation   
    #plot de correlação
    require("corrplot")
    descrCorr <- cor(iris[,-5])
    # plotando com p-value para cada correlação
    # interessante para ver correlação entre os componentes!!
    corrplot.mixed(descrCorr, insig = "p-value",sig.level = -1)
    
    # outro
    pc <- princomp(iris[,1:4], cor=TRUE, scores=TRUE)
    library(rgl)
    plot3d(pc$scores[,1:3], col=iris$Species)
    # plot
    scores = as.data.frame(pca1$x)
    pairs(scores[2:4])
    plot3d(scores[2:4])  # great
    # colorido 
    library(mclust)
    fit <- Mclust(scores[2:4], G=4, modelNames = "EEV")
    plot3d(scores[2:4], col = fit$classification) 
    
    
    l_out <- list(my.descr, my.sumar,)
    return (l_out)

}
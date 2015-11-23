#'função de análise de componentes
#'
library(ggplot2)
library(gclus)
library(Hmisc)
require("corrplot")
require(Hmisc)
f_pca_HG <- function(df_in) {
    # data("USArrests")
    
    # initial cluster analysis (aplicar para os 8 fatores!!)
    #my.abs <- abs(cor(df_in[,15:86]))
    # # PCA with function prcomp
    
    pca1 = prcomp(USArrests, scale. = TRUE) # para teste de circle plot!!
    
    #-----------------------------------------------------
    # PCA sobre os dados brutos das respostas das questões
    #-----------------------------------------------------
    pca1_raw = prcomp(df_raw_in[,15:86], scale. = TRUE) # analise para as 72 questões 
    
    
    # total de variância acumulada antes de aplicar viramax
    #------------------------------------------------------------------
    # resultados muito similares aos da tese (tabela 22 - após viramax)
    summary(pca1_raw)
    
    # obtendo os eigenvalues para saber quantos componentes a manter
    #-------------------------------------------------------------------
    # primeiro critério: usando Kayser criterion: manter eingevalues maior que 1
    # neste caso, manter até o componente 26
    pca1_raw$sdev ^ 2
    # segundo critério: usando scree plot
    screeplot(pca1_raw, main = "Scree Plot - Human Guide", xlab = "Components")
    # ou com linhas
    screeplot(pca1_raw, main = "Scree Plot - Human Guide", type = "lines")
    
    # examinando as rotações (ou loadings) usando dotplot (antes de Varimax)
    # para cada cmponente. Ex. PC1
    load <- pca1_raw$rotation
    sorted.loadings = load[order(load[,1]),1]
    Main="Loadings plot for PC1"
    xlabs = "Variable Loadings"
    dotplot(sorted.loadings, main = Main, xlab = xlabs)
   
    # usando biplot (obs: usar nos 8 fatores!!)
    # o cosseno entre os vetores representa a correlação entre os mesmos
    # variáveis (s11, etc.) como vetores e observações como pontos
    # observando os pontos distribuídos no eixo (componente), podemos
    # ver quais são relacionados direta ou indiretamente
    #biplot(pca1, cex = c(1,07))
    
    # Varimax rotation com convergência na sétima rotação (change of coordinates
    # that maximizes the sum of the variances of the squared loadings)
    # o objetivo é um alimpeza nas rotações que encontramos em prcomp call
    # tomando somente os 4 primeiros componentes
    varimax4 <- varimax(pca1_raw$rotation[,1:4])
    # obtendo matriz de loadings
    #mt_rot <- varimax4$loadings[1:72,1:4]
    # examinando as rotações (ou loadings) usando dotplot (após de Varimax)
    load <- varimax4$loadings
    sorted.loadings = load[order(load[,1]),1]
    Main="Loadings plot for PC1"
    xlabs = "Variable Loadings"
    dotplot(sorted.loadings, main = Main, xlab = xlabs)
    
    # sqrt of eigenvalues
    pca1_raw$sdev
    # loadings or rotations: cada compontnes é uma combinação
    # linear das variáveis (HG quantions). Aqui estão os coeficientes chamados 
    # loadings ou rotations (indicam o quanto cada variável está correlacionada
    # com o componentes especifico, nas colunas)
    head(pca1_raw$rotation)
    # PCs (aka scores)
    head(pca1_raw$x)
 
    
    #-----------------------------------------------------
    # PCA sobre os dados processados dos 8 fatores
    #-----------------------------------------------------
    
    # PRIMEIRA ANÁLISE: correlação entre os 8 fatores
    # obs: primeiro fazer com aprte dos dados pois demora muito!
    # scatterplots de correlacões
    pairs(df_in[1:1000,2:9])
    # salvar como PNG
    
    # tentar com descrCorr <- cor(trainDescr)
    # plotando com p-value para cada correlação
    # interessante para ver correlação entre os componentes!!
    # tentar aplicar na rotação!!
    my.cor <- cor(df_in[,2:9])
    # The P value answers this question:
    # If there really is no correlation between X and Y overall,
    # what is the chance that random sampling would result in a
    # correlation coefficient as far from zero (or further) as
    # observed in this experiment?
    corrplot.mixed(my.cor, insig = "p-value", sig.level = -1, is.corr = TRUE)
    # salvar como PNG
    
    pca1 = prcomp(df_in[,2:9], scale. = TRUE) # analise para os 8 fatores
    # sqrt of eigenvalues
    pca1$sdev
    # loadings or rotations: cada compontnes é uma combinação
    # linear das variáveis (HG quantions). Aqui estão os coeficientes chamados 
    # loadings ou rotations (indicam o quanto cada variável está correlacionada
    # com o componentes especifico, nas colunas)
    head(pca1$rotation)
    # PCs (aka scores)
    head(pca1$x)
    # total de variância acumulada antes de aplicar viramax
    #------------------------------------------------------------------
    # resultados muito similares aos da tese (tabela 22 - após viramax)
    summary(pca1)
    my.importance <- summary(pca1)$importance # salvar como matriz
    
    # obtendo os eigenvalues para saber quantos componentes a manter
    #-------------------------------------------------------------------
    # primeiro critério: usando Kayser criterion: manter eingevalues maior 
    # que 1. 
    # Neste caso, manter até o componente 5
    my.kayser.crit <- pca1$sdev ^ 2
    # segundo critério: usando scree plot
    screeplot(pca1, main = "Scree Plot - Human Guide", xlab = "Components")
    # ou com linhas
    # Neste caso, manter até o componente 7
    screeplot(pca1, main = "Scree Plot - Human Guide", type = "lines")
    
    # aplicando Varimax para os 4 primeiros componentes
    # Varimax rotation com convergência na sétima rotação (change of coordinates
    # that maximizes the sum of the variances of the squared loadings)
    # o objetivo é um alimpeza nas rotações que encontramos em prcomp call
    # tomando somente os 4 primeiros componentes
    varimax4 <- varimax(pca1$rotation[,1:4], normalize = TRUE)
    
    # salvando matriz de loadings/rotations
    my_var.load <- varimax4$loadings[1:8,]
    
    my.cutoff <- .1 # valor mínimo para aparecer na tabela de loadings
    print(loadings(varimax4),cutoff=my.cutoff)

    # ideia: criar no shiny esta tabela onde permite mudar
    # número de componentes

    # examinando as rotações (ou loadings) usando dotplot (antes de Varimax)
    # para cada componente. Ex. PC1
    # Assim vemos quais fatores têm maior contribuição nos componentes)
    load <- pca1$rotation
    sorted.loadings = load[order(load[,1]),1]
    Main="Loadings plot for PC1"
    xlabs = "Variable Loadings"
    dotplot(sorted.loadings, main = Main, xlab = xlabs)
    
    load <- varimax4$loadings
    sorted.loadings = load[order(load[,1]),1]
    Main="Loadings plot for PC1"
    xlabs = "Variable Loadings"
    dotplot(sorted.loadings, main = Main, xlab = xlabs)
    # examinando para os componentes, observamos que os 4 primeiros componentes
    # tem maior contribuição
    # Então usaremos eles para aplicar varimax
    
    
    # usando biplot (obs: usar nos 8 fatores!!)
    # o cosseno entre os vetores representa a correlação entre os mesmos
    # variáveis (s11, etc.) como vetores e observações como pontos
    # observando os pontos distribuídos no eixo (componente), podemos
    # ver quais são relacionados direta ou indiretamente
    

    #biplot(pca1_tidy, cex = c(1,07))
    
    
    
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
    scores = head(as.data.frame(pca1$x), n = 100)
    #scores = as.data.frame(pca3$ind$coord)
    # plot of observations
    #ggplot(data = scores, aes(x = Dim.1, y = Dim.2, label = rownames(scores))) +
    ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
        geom_hline(yintercept = 0, colour = "gray65") +
        geom_vline(xintercept = 0, colour = "gray65") +
        geom_text(colour = "tomato", alpha = 0.8, size = 4) +
        ggtitle("PCA plot of Human Guide - Fatores")
    
    # circle of correlations
    circle <- function(center = c(0, 0), npoints = 100) {
        r <- 1
        tt <-  seq(0, 2 * pi, length = npoints) 
        xx <- center[1] + r * cos(tt)
        yy <-  center[1] + r * sin(tt) 
        return(data.frame(x = xx, y = yy))
    }
    corcir = circle(c(0, 0), npoints = 100)
    # create data frame with correlations between variables and PCs
    correlations = as.data.frame(cor(df_in[,2:9], pca1$x))
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
    # Predict PCs
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

}
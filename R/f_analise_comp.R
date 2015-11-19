#'função de análise de componentes
#'
f_analise_comp <- function() {
    data("USArrests")
    # # PCA with function prcomp
    pca1 = prcomp(USArrests, scale. = TRUE)
    # sqrt of eigenvalues
    pca1$sdev
    # loadings
    head(pca1$rotation)
    # PCs (aka scores)
    head(pca1$x)
    
    # Melhor opção
    # PCA with function PCA
    library(FactoMineR)
    # apply PCA
    pca3 = PCA(USArrests, graph = FALSE)
    # matrix with eigenvalues
    pca3$eig
    # correlations between variables and PCs
    pca3$var$coord
    # PCs (aka scores)
    head(pca3$ind$coord)
    
    # plot of observations
    # load ggplot2
    library(ggplot2)
    # create data frame with scores
    #scores = as.data.frame(pca3$ind$coord)
    scores = as.data.frame(pca1$ind$coord)
    # plot of observations
    #ggplot(data = scores, aes(x = Dim.1, y = Dim.2, label = rownames(scores))) +
    ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
        geom_hline(yintercept = 0, colour = "gray65") +
        geom_vline(xintercept = 0, colour = "gray65") +
        geom_text(colour = "tomato", alpha = 0.8, size = 4) +
        ggtitle("PCA plot of USA States - Crime Rates")
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
    correlations = as.data.frame(cor(USArrests, pca1$x))
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
    screeplot(pca1)
    
    
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
}
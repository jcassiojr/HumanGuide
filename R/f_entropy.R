#'cálculo de entropia
#' entrada: coluna de data.frame categorizada para calcular
f_entropy <- function(target) {
    freq <- table(target)/length(target)
    # vectorize
    vec <- as.data.frame(freq)[,2]
    #drop 0 to avoid NaN resulting from log2
    vec<-vec[vec>0]
    #compute entropy
    return(-sum(vec * log2(vec)))
}
#'funcao que retorna o plot de intensidade prevista para cada area 
#'de atuação de respondente selecionado nos dados de uso
#' input: linha de probabilidade de pertencer para cada area de atuacao obtidas para o respondente
#' output: plot com assinatura do respondente
#' 

f_assinatura_atuacao <- function(in.df_resp){
    my.respondente <- as.character(in.df_resp$nomerespondente) # salva nome do respondente
    in.df_resp <- in.df_resp[,-11] # deixa somente as colunas para previsao 
    resp.t <- as.data.frame(t(in.df_resp))
    resp.t <- add_rownames(resp.t, "atuacao")
    names(resp.t)[2] <- my.respondente
    resp.t$atuacao <- factor(resp.t$atuacao, 
                             levels = c("AT1","AT2",
                                        "AT3","AT4",
                                        "AT5","AT6",
                                        "AT7","AT8",
                                        "AT9","AT10"
                             ))
    colnames(resp.t)  <- c("atuacao", "intensidade")
    
    out.pl_atuacao <- ggplot(resp.t, aes(x=atuacao, y=intensidade)) +
        geom_bar(aes (fill= factor(atuacao)), stat="identity") +
        scale_fill_manual(values=c("royalblue", "royalblue1", "royalblue2", "royalblue3","royalblue4",
                                   "springgreen1", "springgreen2", "springgreen3",  "springgreen4", "green4")) +
        ggtitle(paste("(", my.respondente,")")) +
        xlab("Área de Atuação Human Guide") +
        ylab("intensidade") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(out.pl_atuacao)
}